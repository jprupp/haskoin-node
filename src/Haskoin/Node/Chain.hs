-- |
-- Module      : Haskoin.Node.Chain
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- The block header chain.
--
-- The chain process downloads block headers from connected peers, one peer at a
-- time, connects them to the headers it already has, and stores them in RocksDB.
-- It follows the chain with the most work, and publishes a 'ChainBestBlock'
-- event whenever the best block changes. Once the best block is recent (less
-- than two hours old), has at least the minimum chain work of the network, and
-- no peer is left to sync from, it publishes a single 'ChainSynced' event.
--
-- After a peer has delivered all its headers, it is asked to announce new
-- blocks with headers messages, which are fed back to the chain process by
-- "Haskoin.Node". Peers fall back to inventory messages when they cannot
-- announce with headers, and "Haskoin.Node" feeds those to
-- 'chainBlocksAnnounced'.
--
-- = Why it works this way
--
-- * __One peer at a time.__ Headers form a single chain, so asking several
--   peers for the same range at once would only download it several times.
--   The chain syncs from one peer and moves to the next when it is done or
--   times out, which also means every connected peer eventually gets a chance
--   to show a better chain. "Haskoin.Node.Chain.Sync" decides whose turn it
--   is.
--
-- * __Every hint of unknown blocks is followed.__ A peer announces a new block
--   with a headers message only while it believes the node has the parent.
--   When many blocks are found at once, or the node missed an announcement,
--   the peer sends an inventory message instead, or headers that do not
--   connect. Either way the node asks that peer for headers. Ignoring these
--   left the chain stuck at an old height until the peer reconnected.
--
-- * __Quiet means poll.__ If no headers are requested or received for
--   'timeout' seconds, every connected peer is asked for headers again. This
--   costs one small request per peer per quiet period, and puts a bound on how
--   long any unforeseen stall can last.
--
-- * __Most work wins.__ The best chain is the one with the most accumulated
--   proof of work, not the longest or the first seen, as in every Bitcoin
--   implementation.
--
-- * __"Synced" has to be earned.__ Recent headers alone do not prove much: a
--   peer can feed a cheap chain with fresh timestamps. The chain only counts as
--   synced once its work reaches the network's minimum chain work, the same
--   guard Bitcoin Core uses before leaving initial block download. Applications
--   use 'ChainSynced' to start work that only makes sense at the tip, so a false
--   signal would be costly.
--
-- * __Headers are stored, not kept in memory.__ "Haskoin.Node.Chain.Database"
--   describes how, and which key prefixes it uses in the database.
module Haskoin.Node.Chain
  ( ChainConfig (..),
    ChainEvent (..),
    Chain,
    ChainT,
    runChainT,
    withChain,
    chainGetBlock,
    chainGetBest,
    chainGetAncestor,
    chainGetParents,
    chainGetSplitBlock,
    chainPeerConnected,
    chainPeerDisconnected,
    chainIsSynced,
    chainBlockMain,
    chainHeaders,
    chainBlocksAnnounced,
    chainSync,
  )
where

import Control.Monad (filterM, unless, when)
import Control.Monad.Trans.Reader (ReaderT (..), withReaderT)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.RocksDB (ColumnFamily, DB)
import Haskoin
import Haskoin.Node.Chain.Database (ChainDB (..), initChainDB)
import Haskoin.Node.Chain.Sync
import Haskoin.Node.Logger
import Haskoin.Node.Loop (loopForever)
import Haskoin.Node.Peer
import Haskoin.Node.PeerMgr (myVersion)
import NQE
import System.Random (randomRIO)
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

-- | Configuration for chain syncing process.
data ChainConfig = ChainConfig
  { -- | database handle
    db :: !DB,
    -- | column family
    cf :: !(Maybe ColumnFamily),
    -- | network constants
    net :: !Network,
    -- | send header chain events here
    pub :: !(Publisher ChainEvent),
    -- | seconds to wait for headers from the syncing peer before it is
    -- disconnected; also the quiet period after which all peers are asked for
    -- headers again
    timeout :: !NominalDiffTime,
    -- | log messages here
    logger :: !Logger
  }

-- | Messages handled by the chain process.
data ChainMessage
  = -- | a peer sent headers
    ChainHeaders !Peer ![BlockHeader]
  | -- | a peer announced blocks in an inventory message
    ChainBlocksAnnounced !Peer ![BlockHash]
  | -- | a peer completed the handshake
    ChainPeerConnected !Peer
  | -- | a peer disconnected
    ChainPeerDisconnected !Peer
  | -- | periodic tick to check for timeouts and quiet periods
    ChainPing
  | -- | reply once every previous message has been handled
    ChainSyncMsg !(Listen ())

-- | Events originating from chain syncing process.
data ChainEvent
  = -- | chain has new best block
    ChainBestBlock !BlockNode
  | -- | chain is in sync with the network; published only once, with the best
    -- block at that time
    ChainSynced !BlockNode
  deriving stock (Eq, Show)

-- | Handle to a running chain process. Obtain one with 'withChain', or from
-- the 'Haskoin.Node.Node' returned by 'Haskoin.Node.withNode'.
data Chain = Chain
  { -- | configuration
    config :: !ChainConfig,
    -- | mailbox of the chain process
    mailbox :: !(Mailbox ChainMessage),
    -- | syncing state
    state :: !(TVar ChainState)
  }

instance Eq Chain where
  (==) = (==) `on` (.mailbox)

-- | Mutable state for the header chain process.
data ChainState = ChainState
  { -- | which peers to ask for headers
    sync :: !(SyncState Peer),
    -- | has the header chain ever been considered synced?
    beenInSync :: !Bool
  }

-- | Monad transformer with access to a 'Chain'. It has a 'BlockHeaders'
-- instance, so the header functions of haskoin-core, such as 'getAncestor' and
-- 'splitPoint', can run in it against the database of the chain.
type ChainT m = ReaderT Chain m

-- | Run a 'ChainT' action against a chain.
runChainT :: ChainT m a -> Chain -> m a
runChainT = runReaderT

-- | Read and write headers in the database of a chain configuration.
instance (MonadIO m) => BlockHeaders (ReaderT ChainConfig m) where
  addBlockHeader = withReaderT configDB . addBlockHeader
  getBlockHeader = withReaderT configDB . getBlockHeader
  getBestBlockHeader = withReaderT configDB getBestBlockHeader
  setBestBlockHeader = withReaderT configDB . setBestBlockHeader
  addBlockHeaders = withReaderT configDB . addBlockHeaders

-- | Read and write headers in the database of a chain.
instance (MonadIO m) => BlockHeaders (ReaderT Chain m) where
  addBlockHeader = withReaderT (.config) . addBlockHeader
  getBlockHeader = withReaderT (.config) . getBlockHeader
  getBestBlockHeader = withReaderT (.config) getBestBlockHeader
  setBestBlockHeader = withReaderT (.config) . setBestBlockHeader
  addBlockHeaders = withReaderT (.config) . addBlockHeaders

-- | Where the headers of a configuration are stored.
configDB :: ChainConfig -> ChainDB
configDB cfg = ChainDB {db = cfg.db, cf = cfg.cf}

-- | Run a header action against the database of a chain.
withDB :: Chain -> ChainT m a -> m a
withDB = flip runReaderT

-- | Start the chain process and pass its handle to the given action. The process
-- is stopped when the action returns, and its exceptions are re-thrown in the
-- calling thread.
--
-- The database is initialised first: purged if its data version is old, and
-- seeded with the genesis block of the network if it has no best block. The
-- current best block is published as a 'ChainBestBlock' event on start.
withChain ::
  (MonadUnliftIO m) => ChainConfig -> (Chain -> m a) -> m a
withChain cfg action = do
  (inbox, mailbox) <- newMailbox
  now <- liftIO getCurrentTime
  st <- newTVarIO ChainState {sync = emptySync now, beenInSync = False}
  let ch = Chain {config = cfg, mailbox = mailbox, state = st}
  initChainDB cfg.net (configDB cfg)
  withAsync (withClock cfg.timeout mailbox (run ch inbox)) $ \a ->
    link a >> action ch
  where
    run ch inbox = do
      withDB ch getBestBlockHeader >>= chainEvent ch . ChainBestBlock
      loopForever $ do
        logDebug cfg.logger "Awaiting event..."
        receive inbox >>= chainMessage ch

-- | Log and publish a chain event.
chainEvent :: (MonadIO m) => Chain -> ChainEvent -> m ()
chainEvent ch e = do
  case e of
    ChainBestBlock b ->
      logInfo
        ch.config.logger
        ("Best block header at height " <> cs (show b.height))
    ChainSynced b ->
      logInfo
        ch.config.logger
        ("Headers in sync at height " <> cs (show b.height))
  publish e ch.config.pub

-- | Handle a message sent to the chain process.
chainMessage :: (MonadIO m) => Chain -> ChainMessage -> m ()
chainMessage ch = \case
  ChainPeerConnected p -> do
    logDebug ch.config.logger ("Connected peer " <> p.label)
    update ch (peerConnected p)
    syncNext ch
  ChainPeerDisconnected p -> do
    logDebug ch.config.logger ("Disconnected peer " <> p.label)
    releaseIfSyncing ch p
    update ch (peerDisconnected p)
    syncNext ch
  ChainHeaders p hs -> whenConnected ch p (processHeaders ch p hs)
  ChainBlocksAnnounced p bhs -> whenConnected ch p (processAnnouncement ch p bhs)
  ChainPing -> checkSync ch
  ChainSyncMsg r -> atomically (r ())

-- | Ignore messages from peers that are not connected, or that are being
-- disconnected for misbehaving.
whenConnected :: (MonadIO m) => Chain -> Peer -> m () -> m ()
whenConnected ch p go =
  getStatus ch p >>= \case
    Just Dropped -> ignore "being dropped"
    Just _ -> go
    Nothing -> ignore "not connected"
  where
    ignore why =
      logDebug ch.config.logger ("Ignoring peer " <> why <> ": " <> p.label)

-- | Import headers sent by a peer.
--
-- An empty message from the syncing peer means it has nothing more. Headers
-- that do not connect mean the node missed an announcement, so the peer is
-- asked for headers. Invalid headers disconnect the peer. After a full
-- message of 'maxHeadersSize' headers the peer is asked for more, and after a
-- shorter one the syncing peer is done.
processHeaders :: (MonadIO m) => Chain -> Peer -> [BlockHeader] -> m ()
processHeaders ch p [] = do
  logDebug ch.config.logger ("No headers from peer " <> p.label)
  syncing <- isSyncing ch p
  when syncing $ finish ch p
processHeaders ch p hs@(h : _) = do
  logDebug
    ch.config.logger
    ("Processing " <> cs (show (length hs)) <> " headers from peer " <> p.label)
  withDB ch (getBlockHeader h.prev) >>= \case
    Nothing -> do
      logDebug
        ch.config.logger
        ("Headers from peer " <> p.label <> " do not connect")
      update ch (requestSync p)
      syncNext ch
    Just _ ->
      importHeaders ch hs >>= \case
        Left e -> misbehaving ch p ("could not connect headers: " <> cs e)
        Right lst -> do
          syncing <- isSyncing ch p
          let full = length hs >= fromIntegral maxHeadersSize
          if
            | full && syncing -> requestHeaders ch p (Just lst)
            | full -> update ch (requestSync p) >> syncNext ch
            | syncing -> finish ch p
            | otherwise -> syncNotif ch

-- | A peer announced blocks in an inventory message. If any is unknown, ask
-- the peer for headers.
processAnnouncement :: (MonadIO m) => Chain -> Peer -> [BlockHash] -> m ()
processAnnouncement ch p bhs = do
  unknown <- filterM (fmap isNothing . chainGetBlock ch) bhs
  unless (null unknown) $ do
    logDebug
      ch.config.logger
      ( "Peer "
          <> p.label
          <> " announced "
          <> cs (show (length unknown))
          <> " unknown blocks"
      )
    update ch (requestSync p)
    syncNext ch

-- | Disconnect the syncing peer if it has not answered in time. If no peer is
-- syncing, start with a queued one, or queue every peer if nothing has
-- happened for a while.
checkSync :: (MonadIO m) => Chain -> m ()
checkSync ch = do
  now <- liftIO getCurrentTime
  s <- (.sync) <$> readTVarIO ch.state
  let quiet t = now `diffUTCTime` t > ch.config.timeout
  case syncingPeer s of
    Just (p, t)
      | quiet t -> misbehaving ch p "timed out"
      | otherwise -> return ()
    Nothing
      | not (null (queuedPeers s)) -> syncNext ch
      | quiet (lastActivity s) && not (null (connectedPeers s)) -> do
          logDebug ch.config.logger "No recent header activity, polling peers"
          update ch (pollPeers now)
          syncNext ch
      | otherwise -> return ()

-- | Unless a peer is being synced against already, start with the first queued
-- peer that 'setBusy' can reserve. Check whether the chain is synced if there
-- is nobody to sync against.
syncNext :: (MonadIO m) => Chain -> m ()
syncNext ch = do
  s <- (.sync) <$> readTVarIO ch.state
  when (isNothing (syncingPeer s)) $ go (queuedPeers s)
  where
    go [] = syncNotif ch
    go (p : ps) =
      setBusy p >>= \case
        True -> do
          logDebug ch.config.logger ("Syncing against peer " <> p.label)
          now <- liftIO getCurrentTime
          update ch (startSync now p)
          requestHeaders ch p Nothing
        False -> do
          logDebug ch.config.logger ("Could not lock peer: " <> p.label)
          go ps

-- | Ask the syncing peer for the headers after the given block, or after the
-- best block.
requestHeaders :: (MonadIO m) => Chain -> Peer -> Maybe BlockNode -> m ()
requestHeaders ch p after = do
  now <- liftIO getCurrentTime
  update ch (refreshSync now p)
  bn <- maybe (withDB ch getBestBlockHeader) return after
  loc <- withDB ch (blockLocator bn)
  logDebug
    ch.config.logger
    ( "Requesting headers after height "
        <> cs (show bn.height)
        <> " from peer "
        <> p.label
    )
  MGetHeaders GetHeaders {version = myVersion, locator = loc, stop = zero}
    `sendMessage` p
  where
    zero = "0000000000000000000000000000000000000000000000000000000000000000"

-- | The syncing peer has no more headers. Ask it to announce new blocks with
-- headers, release it, and move on.
finish :: (MonadIO m) => Chain -> Peer -> m ()
finish ch p = do
  logDebug ch.config.logger ("Releasing syncing peer " <> p.label)
  MSendHeaders `sendMessage` p
  update ch (finishSync p)
  setFree p
  syncNext ch

-- | Disconnect a peer that sent invalid headers or timed out, and move on
-- without waiting for its disconnection.
misbehaving :: (MonadIO m) => Chain -> Peer -> Text -> m ()
misbehaving ch p why = do
  logWarn ch.config.logger ("Disconnecting peer " <> p.label <> ": " <> why)
  releaseIfSyncing ch p
  update ch (dropPeer p)
  killPeer p
  syncNext ch

-- | Release a peer reserved for syncing.
releaseIfSyncing :: (MonadIO m) => Chain -> Peer -> m ()
releaseIfSyncing ch p = do
  syncing <- isSyncing ch p
  when syncing $ setFree p

-- | Apply a transition to the sync state.
update :: (MonadIO m) => Chain -> (SyncState Peer -> SyncState Peer) -> m ()
update ch f = atomically . modifyTVar ch.state $ \s -> s {sync = f s.sync}

getStatus :: (MonadIO m) => Chain -> Peer -> m (Maybe PeerStatus)
getStatus ch p = peerStatus p . (.sync) <$> readTVarIO ch.state

isSyncing :: (MonadIO m) => Chain -> Peer -> m Bool
isSyncing ch p =
  getStatus ch p <&> \case
    Just (Syncing _ _) -> True
    _ -> False

-- | Validate and store a run of headers, and publish the best block if it
-- changed. Returns the node of the last header.
importHeaders ::
  (MonadIO m) => Chain -> [BlockHeader] -> m (Either String BlockNode)
importHeaders ch hs = do
  now <- liftIO getCurrentTime
  let timestamp = floor (utcTimeToPOSIXSeconds now)
  before <- withDB ch getBestBlockHeader
  withDB ch (connectBlocks ch.config.net timestamp hs) >>= \case
    Left e -> do
      logError ch.config.logger ("connectBlocks: " <> cs e)
      return (Left e)
    Right [] -> return (Left "no headers")
    Right (lst : _) -> do
      update ch (touchSync now)
      after <- withDB ch getBestBlockHeader
      when (before.header /= after.header) $
        chainEvent ch (ChainBestBlock after)
      return (Right lst)

-- | Publish a 'ChainSynced' event the first time the chain is synced: the best
-- block is less than two hours old, has the 'minimumChainWork' of the network,
-- and no peer is being synced against or waiting to be.
--
-- Bitcoin Core requires the same work before it leaves initial block
-- download. A chain with less work is not the real one, however recent its
-- last block is.
syncNotif :: (MonadIO m) => Chain -> m ()
syncNotif ch = do
  best <- withDB ch getBestBlockHeader
  now <- liftIO getCurrentTime
  let recent = now `diffUTCTime` blockTime best <= 7200
      enough = best.work >= ch.config.net.minimumChainWork
  notify <- atomically $ do
    s <- readTVar ch.state
    let idle = isNothing (syncingPeer s.sync) && null (queuedPeers s.sync)
    if recent && enough && idle && not s.beenInSync
      then writeTVar ch.state s {beenInSync = True} >> return True
      else return False
  when notify $ chainEvent ch (ChainSynced best)
  where
    blockTime = posixSecondsToUTCTime . fromIntegral . (.header.timestamp)

-- | Run a loop in the background that sends a 'ChainPing' to the chain
-- process at random intervals. They are at most a quarter of the timeout, so
-- timeouts are noticed promptly, and between 0.1 and 20 seconds, so a short
-- timeout does not make the chain spin and a long one does not make it doze.
withClock ::
  (MonadUnliftIO m) => NominalDiffTime -> Mailbox ChainMessage -> m a -> m a
withClock to mbox go =
  withAsync clock $ \a -> link a >> go
  where
    upper :: Int
    upper = max 100000 (min 20000000 (floor (to * 1000000) `div` 4))
    clock = loopForever $ do
      threadDelay =<< randomRIO (upper `div` 10, upper)
      ChainPing `send` mbox

-- | Get a block header from the block chain. Returns headers on side chains
-- too; use 'chainBlockMain' to check whether a block is in the main chain.
chainGetBlock :: (MonadIO m) => Chain -> BlockHash -> m (Maybe BlockNode)
chainGetBlock ch bh = withDB ch (getBlockHeader bh)

-- | Get best block header from chain process.
chainGetBest :: (MonadIO m) => Chain -> m BlockNode
chainGetBest ch = withDB ch getBestBlockHeader

-- | Get ancestor of 'BlockNode' at 'BlockHeight' from chain process. Returns
-- 'Nothing' if the height is above that of the node.
chainGetAncestor ::
  (MonadIO m) => Chain -> BlockHeight -> BlockNode -> m (Maybe BlockNode)
chainGetAncestor ch h bn = withDB ch (getAncestor h bn)

-- | Get parents of 'BlockNode' starting at 'BlockHeight' from chain process.
-- The result is in ascending order, from the block at the given height up to
-- the parent of the given node, which is not included.
chainGetParents ::
  (MonadIO m) => Chain -> BlockHeight -> BlockNode -> m [BlockNode]
chainGetParents ch height = go []
  where
    go acc b
      | height >= b.height = return acc
      | otherwise =
          chainGetBlock ch b.header.prev >>= \case
            Nothing -> return acc
            Just p -> go (p : acc) p

-- | Get last common block from chain process: the most recent block that is an
-- ancestor of both nodes.
chainGetSplitBlock ::
  (MonadIO m) => Chain -> BlockNode -> BlockNode -> m BlockNode
chainGetSplitBlock ch l r = withDB ch (splitPoint l r)

-- | Notify chain that a new peer is connected. It is queued to be asked for
-- headers.
chainPeerConnected :: (MonadIO m) => Chain -> Peer -> m ()
chainPeerConnected ch p = ChainPeerConnected p `send` ch.mailbox

-- | Notify chain that a peer has disconnected.
chainPeerDisconnected :: (MonadIO m) => Chain -> Peer -> m ()
chainPeerDisconnected ch p = ChainPeerDisconnected p `send` ch.mailbox

-- | Is given 'BlockHash' in the main chain?
chainBlockMain :: (MonadIO m) => Chain -> BlockHash -> m Bool
chainBlockMain ch bh = do
  best <- chainGetBest ch
  chainGetBlock ch bh >>= \case
    Nothing -> return False
    Just bn -> (== Just bn) <$> chainGetAncestor ch bn.height best

-- | Is chain in sync with network? Once 'True', it stays 'True' even if the
-- chain falls behind later.
chainIsSynced :: (MonadIO m) => Chain -> m Bool
chainIsSynced ch = (.beenInSync) <$> readTVarIO ch.state

-- | Peer sends a bunch of headers to the chain process. "Haskoin.Node" calls
-- this for every headers message received from an online peer.
chainHeaders :: (MonadIO m) => Chain -> Peer -> [BlockHeader] -> m ()
chainHeaders ch p hs = ChainHeaders p hs `send` ch.mailbox

-- | Peer announced blocks in an inventory message. If any of them is unknown,
-- the peer is asked for headers. "Haskoin.Node" calls this for every
-- inventory message with blocks received from an online peer.
chainBlocksAnnounced :: (MonadIO m) => Chain -> Peer -> [BlockHash] -> m ()
chainBlocksAnnounced ch p bhs = ChainBlocksAnnounced p bhs `send` ch.mailbox

-- | Wait until the chain process has handled every message sent to it before.
chainSync :: (MonadIO m) => Chain -> m ()
chainSync ch = ChainSyncMsg `query` ch.mailbox
