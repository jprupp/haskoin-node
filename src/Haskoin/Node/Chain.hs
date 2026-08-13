{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  )
where

import Control.Monad (forM_, forever, guard, when)
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Data.ByteString qualified as B
import Data.Function (on)
import Data.List (delete, nub)
import Data.Maybe (isJust, isNothing)
import Data.Serialize
import Data.String.Conversions (cs)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word (Word32)
import Database.RocksDB (ColumnFamily, DB)
import Database.RocksDB qualified as R
import Database.RocksDB.Query
import Haskoin hiding (Key)
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
    -- | timeout in seconds
    timeout :: !NominalDiffTime
  }

data ChainMessage
  = ChainHeaders !Peer ![BlockHeader]
  | ChainPeerConnected !Peer
  | ChainPeerDisconnected !Peer
  | ChainPing

-- | Events originating from chain syncing process.
data ChainEvent
  = -- | chain has new best block
    ChainBestBlock !BlockNode
  | -- | chain is in sync with the network
    ChainSynced !BlockNode
  deriving (Eq, Show)

-- | State and configuration.
data Chain = Chain
  { config :: !ChainConfig,
    mailbox :: !(Mailbox ChainMessage),
    state :: !(TVar ChainState)
  }

instance Eq Chain where
  (==) = (==) `on` (.mailbox)

-- | Database key for version.
data ChainDataVersionKey = ChainDataVersionKey
  deriving (Eq, Ord, Show)

instance Key ChainDataVersionKey

instance KeyValue ChainDataVersionKey Word32

instance Serialize ChainDataVersionKey where
  get = do
    guard . (== 0x92) =<< getWord8
    return ChainDataVersionKey
  put ChainDataVersionKey = putWord8 0x92

data ChainSync = ChainSync
  { peer :: !Peer,
    timestamp :: !UTCTime,
    best :: !(Maybe BlockNode)
  }

-- | Mutable state for the header chain process.
data ChainState = ChainState
  { -- | peer to sync against and time of last received message
    syncing :: !(Maybe ChainSync),
    -- | queue of peers to sync against
    peers :: ![Peer],
    -- | has the header chain ever been considered synced?
    beenInSync :: !Bool
  }

-- | Key for block header in database.
newtype BlockHeaderKey = BlockHeaderKey BlockHash deriving (Eq, Show)

instance Serialize BlockHeaderKey where
  get = do
    guard . (== 0x90) =<< getWord8
    BlockHeaderKey <$> get
  put (BlockHeaderKey bh) = do
    putWord8 0x90
    put bh

-- | Key for best block in database.
data BestBlockKey = BestBlockKey deriving (Eq, Show)

instance KeyValue BlockHeaderKey BlockNode

instance KeyValue BestBlockKey BlockNode

instance Serialize BestBlockKey where
  get = do
    guard . (== 0x91) =<< getWord8
    return BestBlockKey
  put BestBlockKey = putWord8 0x91

type ChainT m = ReaderT Chain m

runChainT :: ChainT m a -> Chain -> m a
runChainT = runReaderT

instance (MonadIO m) => BlockHeaders (ReaderT ChainConfig m) where
  addBlockHeader bn = ReaderT $ \ChainConfig {db, cf} -> liftIO $ do
    case cf of
      Nothing -> insert db (BlockHeaderKey h) bn
      Just cf' -> insertCF db cf' (BlockHeaderKey h) bn
    where
      h = headerHash bn.header
  getBlockHeader bh = ReaderT $ \ChainConfig {db, cf} -> liftIO $ do
    retrieveCommon db cf (BlockHeaderKey bh)
  getBestBlockHeader = ReaderT $ \ChainConfig {db, cf} -> liftIO $ do
    retrieveCommon db cf BestBlockKey >>= \case
      Nothing -> error "Could not get best block from database"
      Just b -> return b
  setBestBlockHeader bn = ReaderT $ \ChainConfig {db, cf} -> liftIO $ do
    case cf of
      Nothing -> insert db BestBlockKey bn
      Just cf' -> insertCF db cf' BestBlockKey bn
  addBlockHeaders bns = ReaderT $ \ChainConfig {db, cf} -> liftIO $ do
    writeBatch db (map (f cf) bns)
    where
      h bn = headerHash bn.header
      f cf bn = case cf of
        Nothing -> insertOp (BlockHeaderKey (h bn)) bn
        Just cf' -> insertOpCF cf' (BlockHeaderKey (h bn)) bn

instance (MonadIO m) => BlockHeaders (ChainT m) where
  addBlockHeader bn = withReaderT (.config) (addBlockHeader bn)
  getBlockHeader bh = withReaderT (.config) (getBlockHeader bh)
  getBestBlockHeader = withReaderT (.config) getBestBlockHeader
  setBestBlockHeader bn = withReaderT (.config) (setBestBlockHeader bn)
  addBlockHeaders bns = withReaderT (.config) (addBlockHeaders bns)

withChain ::
  (MonadLoggerIO m, MonadUnliftIO m) => ChainConfig -> (Chain -> m a) -> m a
withChain cfg action = do
  (inbox, mailbox) <- newMailbox
  st <-
    newTVarIO
      ChainState
        { syncing = Nothing,
          beenInSync = False,
          peers = []
        }
  let ch = Chain {config = cfg, mailbox = mailbox, state = st}
  initChainDB cfg
  withAsync (main_loop ch inbox) $ \a ->
    link a >> action ch
  where
    main_loop ch inbox =
      withSyncLoop ch.mailbox (run ch inbox)
    run ch inbox = do
      runReaderT getBestBlockHeader cfg
        >>= chainEvent cfg.pub . ChainBestBlock
      forever $ do
        $(logDebugS) "Chain" "Awaiting event..."
        msg <- receive inbox
        chainConfigMessage ch msg

chainEvent :: (MonadLoggerIO m) => Publisher ChainEvent -> ChainEvent -> m ()
chainEvent pub e = do
  case e of
    ChainBestBlock b ->
      $(logInfoS)
        "Chain"
        ("Best block header at height " <> cs (show b.height))
    ChainSynced b ->
      $(logInfoS)
        "Chain"
        ("Headers in sync at height " <> cs (show b.height))
  publish e pub

processHeaders :: (MonadLoggerIO m) => Chain -> Peer -> [BlockHeader] -> m ()
processHeaders ch p hs = do
  let len = length hs
  $(logDebugS)
    "Chain"
    ("Processing " <> cs (show len) <> " headers from peer " <> p.label)
  let net = ch.config.net
  now <- liftIO getCurrentTime
  pbest <- runReaderT getBestBlockHeader ch.config
  importHeaders ch now hs >>= \case
    Nothing -> do
      $(logWarnS)
        "Chain"
        ("Could not connect headers from peer " <> p.label)
      killPeer p
    Just done -> do
      setLastReceived ch.state
      best <- runReaderT getBestBlockHeader ch.config
      when (pbest.header /= best.header) $
        chainEvent ch.config.pub (ChainBestBlock best)
      if done
        then do
          MSendHeaders `sendMessage` p
          finishPeer ch.state p
          syncNewPeer ch
          syncNotif ch
        else syncPeer ch p

syncNewPeer :: (MonadLoggerIO m) => Chain -> m ()
syncNewPeer ch =
  getSyncingPeer ch.state >>= \case
    Just _ -> return ()
    Nothing ->
      nextPeer ch.state >>= \case
        Nothing -> return ()
        Just p -> do
          $(logDebugS) "Chain" ("Syncing against peer " <> p.label)
          syncPeer ch p

syncNotif :: (MonadLoggerIO m) => Chain -> m ()
syncNotif ch =
  notifySynced ch >>= \case
    False -> return ()
    True ->
      runReaderT getBestBlockHeader ch.config
        >>= chainEvent ch.config.pub . ChainSynced

syncPeer :: (MonadLoggerIO m) => Chain -> Peer -> m ()
syncPeer ch p = do
  t <- liftIO getCurrentTime
  m <-
    chainSyncingPeer ch.state >>= \case
      Just ChainSync {peer = s, best = m}
        | p == s -> syncing_me t m
        | otherwise -> return Nothing
      Nothing -> syncing_new t
  forM_ m $ \g -> do
    $(logDebugS)
      "Chain"
      ("Requesting headers from peer " <> p.label)
    MGetHeaders g `sendMessage` p
  where
    syncing_new t =
      setSyncingPeer ch.state p >>= \case
        False -> return Nothing
        True -> do
          $(logDebugS) "Chain" ("Locked peer " <> p.label)
          h <- runReaderT getBestBlockHeader ch.config
          Just <$> syncHeaders ch t h p
    syncing_me t m = do
      h <- case m of
        Nothing -> runReaderT getBestBlockHeader ch.config
        Just h -> return h
      Just <$> syncHeaders ch t h p

chainConfigMessage :: (MonadLoggerIO m) => Chain -> ChainMessage -> m ()
chainConfigMessage ch (ChainHeaders p hs) =
  processHeaders ch p hs
chainConfigMessage ch (ChainPeerConnected p) = do
  $(logDebugS) "Chain" ("Connected peer " <> p.label)
  addPeer ch.state p
  syncNewPeer ch
chainConfigMessage ch (ChainPeerDisconnected p) = do
  $(logDebugS) "Chain" ("Disconnected peer " <> p.label)
  finishPeer ch.state p
  syncNewPeer ch
chainConfigMessage ch ChainPing = do
  $(logDebugS) "Chain" "Internal clock event"
  let to = ch.config.timeout
  now <- liftIO getCurrentTime
  chainSyncingPeer ch.state >>= \case
    Just ChainSync {peer = p, timestamp = t}
      | now `diffUTCTime` t > to -> do
          $(logWarnS)
            "Chain"
            ("Syncing peer " <> p.label <> " timed out")
          killPeer p
      | otherwise -> return ()
    Nothing -> syncNewPeer ch

withSyncLoop :: (MonadUnliftIO m) => Mailbox ChainMessage -> m a -> m a
withSyncLoop mbox mf =
  withAsync go $ \a ->
    link a >> mf
  where
    go = forever $ do
      delay <- randomRIO (2 * 10 ^ 6, 2 * 10 ^ 7)
      threadDelay delay
      ChainPing `send` mbox

-- | Version of the database.
dataVersion :: Word32
dataVersion = 1

-- | Initialize header database. If version is different from current, the
-- database is purged of conflicting elements first.
initChainDB :: (MonadIO m) => ChainConfig -> m ()
initChainDB cfg@ChainConfig {db, cf, net} = liftIO $ do
  ver <- retrieveCommon db cf ChainDataVersionKey
  when (ver /= Just dataVersion) $ purgeChainDB cfg >>= writeBatch db
  case cf of
    Nothing -> insert db ChainDataVersionKey dataVersion
    Just cf' -> insertCF db cf' ChainDataVersionKey dataVersion
  retrieveCommon db cf BestBlockKey >>= \b ->
    when (isNothing (b :: Maybe BlockNode)) . flip runReaderT cfg $ do
      addBlockHeader (genesisNode net)
      setBestBlockHeader (genesisNode net)

-- | Purge database of elements having keys that may conflict with those used in
-- this module.
purgeChainDB :: (MonadIO m) => ChainConfig -> m [R.BatchOp]
purgeChainDB ChainConfig {db, cf} = liftIO $ do
  with_iter $ \it -> do
    R.iterSeek it (B.singleton 0x90)
    recurse_delete it
  where
    with_iter = case cf of
      Nothing -> R.withIter db
      Just cf' -> R.withIterCF db cf'
    recurse_delete it =
      R.iterKey it >>= \case
        Just k
          | B.head k == 0x90 || B.head k == 0x91 -> do
              case cf of
                Nothing -> R.delete db k
                Just cf' -> R.deleteCF db cf' k
              R.iterNext it
              (R.Del k :) <$> recurse_delete it
        _ -> return []

-- | Import a bunch of continuous headers. Returns 'True' if the number of
-- headers is 2000, which means that there are possibly more headers to sync
-- from whatever peer delivered these.
importHeaders ::
  (MonadIO m) => Chain -> UTCTime -> [BlockHeader] -> m (Maybe Bool)
importHeaders ch now hs =
  connect >>= \case
    Left _ -> return Nothing
    Right _
      | null hs -> return (Just False)
      | otherwise -> do
          bb <- get_last
          atomically . modifyTVar ch.state $ \s ->
            s {syncing = set_best bb <$> s.syncing}
          return (Just (length hs == 2000))
  where
    set_best bb ChainSync {..} = ChainSync {best = bb, ..}
    timestamp = floor (utcTimeToPOSIXSeconds now)
    connect = runReaderT (connectBlocks ch.config.net timestamp hs) ch.config
    get_last = runReaderT (getBlockHeader (headerHash (last hs))) ch.config

-- | Check if best block header is in sync with the rest of the block chain by
-- comparing the best block with the current time, verifying that there are no
-- peers in the queue to be synced, and no peer is being synced at the moment.
-- This function will only return 'True' once. It should be used to decide
-- whether to notify other processes that the header chain has been synced. The
-- state of the chain will be flipped to synced when this function returns
-- 'True'.
notifySynced :: (MonadIO m) => Chain -> m Bool
notifySynced ch = do
  bb <- runReaderT getBestBlockHeader ch.config
  df <- (`diffUTCTime` block_time bb) <$> liftIO getCurrentTime
  atomically $ do
    s <- readTVar ch.state
    if
      | df > 7200 -> return False
      | isJust s.syncing -> return False
      | not (null s.peers) -> return False
      | s.beenInSync -> return False
      | otherwise -> do
          writeTVar ch.state s {beenInSync = True}
          return True
  where
    block_time =
      posixSecondsToUTCTime . fromIntegral . (.header.timestamp)

-- | Get next peer to sync against from the queue.
nextPeer :: (MonadLoggerIO m) => TVar ChainState -> m (Maybe Peer)
nextPeer st = fmap (.peers) (readTVarIO st) >>= go
  where
    go [] = return Nothing
    go (p : ps) =
      setSyncingPeer st p >>= \case
        True -> return (Just p)
        False -> go ps

-- | Set a syncing peer and generate a 'GetHeaders' data structure with a block
-- locator to send to that peer for syncing.
syncHeaders ::
  (MonadIO m) => Chain -> UTCTime -> BlockNode -> Peer -> m GetHeaders
syncHeaders ch now bb p = do
  atomically $
    modifyTVar ch.state $ \s ->
      s
        { syncing =
            Just
              ChainSync
                { peer = p,
                  timestamp = now,
                  best = Nothing
                },
          peers = delete p s.peers
        }
  loc <- runReaderT (blockLocator bb) ch.config
  return
    GetHeaders
      { version = myVersion,
        locator = loc,
        stop = z
      }
  where
    z = "0000000000000000000000000000000000000000000000000000000000000000"

-- | Set the time of last received data to now if a syncing peer is active.
setLastReceived :: (MonadIO m) => TVar ChainState -> m ()
setLastReceived st = do
  now <- liftIO getCurrentTime
  let f ChainSync {..} = ChainSync {timestamp = now, ..}
  atomically (modifyTVar st (\s -> s {syncing = f <$> s.syncing}))

-- | Add a new peer to the queue of peers to sync against.
addPeer :: (MonadIO m) => TVar ChainState -> Peer -> m ()
addPeer st p = do
  atomically (modifyTVar st (\s -> s {peers = nub (p : s.peers)}))

-- | Get syncing peer if there is one.
getSyncingPeer :: (MonadIO m) => TVar ChainState -> m (Maybe Peer)
getSyncingPeer st =
  readTVarIO st >>= \case
    ChainState {syncing = Just ChainSync {peer}} -> return (Just peer)
    _ -> return Nothing

setSyncingPeer :: (MonadLoggerIO m) => TVar ChainState -> Peer -> m Bool
setSyncingPeer st p =
  setBusy p >>= \case
    False -> do
      $(logDebugS)
        "Chain"
        ("Could not lock peer: " <> p.label)
      return False
    True -> do
      $(logDebugS) "Chain" $
        ("Locked peer: " <> p.label)
      set_it
      return True
  where
    set_it = do
      now <- liftIO getCurrentTime
      atomically $ modifyTVar st $ \s ->
        s
          { syncing =
              Just
                ChainSync
                  { peer = p,
                    timestamp = now,
                    best = Nothing
                  }
          }

-- | Remove a peer from the queue of peers to sync and unset the syncing peer if
-- it is set to the provided peer.
finishPeer :: (MonadLoggerIO m) => TVar ChainState -> Peer -> m ()
finishPeer st p =
  remove_peer >>= \case
    False ->
      $(logDebugS)
        "Chain"
        ("Removed peer from queue: " <> p.label)
    True -> do
      $(logDebugS)
        "Chain"
        ("Releasing syncing peer: " <> p.label)
      setFree p
  where
    remove_peer =
      atomically $
        readTVar st >>= \s -> case s.syncing of
          Just ChainSync {peer = p'}
            | p == p' -> do
                unset_syncing
                return True
          _ -> do
            remove_from_queue
            return False
    unset_syncing =
      modifyTVar st $ \x ->
        x {syncing = Nothing}
    remove_from_queue =
      modifyTVar st $ \x ->
        x {peers = delete p x.peers}

-- | Return syncing peer data.
chainSyncingPeer :: (MonadIO m) => TVar ChainState -> m (Maybe ChainSync)
chainSyncingPeer st = (.syncing) <$> readTVarIO st

-- | Get a block header from the block chain.
chainGetBlock :: (MonadIO m) => Chain -> BlockHash -> m (Maybe BlockNode)
chainGetBlock ch bh = runReaderT (getBlockHeader bh) ch.config

-- | Get best block header from chain process.
chainGetBest :: (MonadIO m) => Chain -> m BlockNode
chainGetBest ch = runReaderT getBestBlockHeader ch.config

-- | Get ancestor of 'BlockNode' at 'BlockHeight' from chain process.
chainGetAncestor ::
  (MonadIO m) => Chain -> BlockHeight -> BlockNode -> m (Maybe BlockNode)
chainGetAncestor ch h bn = runReaderT (getAncestor h bn) ch.config

-- | Get parents of 'BlockNode' starting at 'BlockHeight' from chain process.
chainGetParents ::
  (MonadIO m) => Chain -> BlockHeight -> BlockNode -> m [BlockNode]
chainGetParents ch height top =
  go [] top
  where
    go acc b
      | height >= b.height = return acc
      | otherwise = do
          m <- chainGetBlock ch b.header.prev
          case m of
            Nothing -> return acc
            Just p -> go (p : acc) p

-- | Get last common block from chain process.
chainGetSplitBlock ::
  (MonadIO m) => Chain -> BlockNode -> BlockNode -> m BlockNode
chainGetSplitBlock ch l r = runReaderT (splitPoint l r) ch.config

-- | Notify chain that a new peer is connected.
chainPeerConnected :: (MonadIO m) => Chain -> Peer -> m ()
chainPeerConnected ch p =
  ChainPeerConnected p `send` ch.mailbox

-- | Notify chain that a peer has disconnected.
chainPeerDisconnected :: (MonadIO m) => Chain -> Peer -> m ()
chainPeerDisconnected ch p =
  ChainPeerDisconnected p `send` ch.mailbox

-- | Is given 'BlockHash' in the main chain?
chainBlockMain :: (MonadIO m) => Chain -> BlockHash -> m Bool
chainBlockMain ch bh =
  chainGetBest ch >>= \bb ->
    chainGetBlock ch bh >>= \case
      Nothing ->
        return False
      bm@(Just bn) ->
        (== bm) <$> chainGetAncestor ch bn.height bb

-- | Is chain in sync with network?
chainIsSynced :: (MonadIO m) => Chain -> m Bool
chainIsSynced ch =
  (.beenInSync) <$> readTVarIO (ch.state)

-- | Peer sends a bunch of headers to the chain process.
chainHeaders :: (MonadIO m) => Chain -> Peer -> [BlockHeader] -> m ()
chainHeaders ch p hs =
  ChainHeaders p hs `send` ch.mailbox
