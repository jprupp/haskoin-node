-- |
-- Module      : Haskoin.Node.Peer
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- A single connection to a peer on the Bitcoin network.
--
-- Each peer runs as its own process. It reads messages from the network,
-- parses them, and publishes them as 'PeerEvent's. It also accepts messages to
-- send to the peer through its mailbox. Peers are normally started and managed
-- by "Haskoin.Node.PeerMgr"; applications mostly use 'sendMessage',
-- 'getBlocks', 'getTxs' and 'getData' on peers obtained from
-- 'Haskoin.Node.PeerMgr.getPeers'.
--
-- = Defensive by design
--
-- A peer is anyone on the Internet. The code here assumes the worst of it:
--
-- * The 24-byte header of each message is read and checked first: network
--   magic, and payload size against the limit for that command. Only then is
--   the payload read, so a peer cannot make the node buffer a huge message it
--   would refuse anyway.
--
-- * Anything that does not parse closes the connection with a
--   'PeerException' saying why. There is no attempt to recover, because a
--   peer that sends garbage once is not worth the risk.
--
-- * Reading pauses when the node falls behind ('maxPendingMessages'), so the
--   speed of the node, not the speed of the peer, sets the pace.
module Haskoin.Node.Peer
  ( PeerConfig (..),
    PeerEvent (..),
    PeerException (..),
    Conduits (..),
    WithConnection,
    Peer (..),
    PeerMessage (..),
    peer,
    wrapPeer,
    sendMessage,
    killPeer,
    getBlocks,
    getTxs,
    getData,
    pingPeer,
    getBusy,
    setBusy,
    setFree,
    peerMessageDone,
    maxPendingMessages,
  )
where

import Conduit
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO)
import Control.Monad
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Serialize (decode, runGet, runPut)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32)
import Haskoin
import Haskoin.Node.Logger
import Haskoin.Node.Loop (loopForever)
import Haskoin.Node.Peer.Request
import NQE
import System.Random
import System.Timeout (timeout)

-- | The two directions of a network connection, as conduits.
--
-- Connections are abstract conduits rather than sockets, so that the same peer
-- code runs over TCP, through a proxy, or against an in-memory mock in tests.
data Conduits = Conduits
  { -- | bytes received from the peer
    inboundConduit :: ConduitT () ByteString IO (),
    -- | bytes to send to the peer
    outboundConduit :: ConduitT ByteString Void IO ()
  }

-- | A function that opens a connection, passes its 'Conduits' to the given
-- action, and closes the connection when the action returns.
type WithConnection = (Conduits -> IO ()) -> IO ()

-- | Configuration of a peer process.
data PeerConfig = PeerConfig
  { -- | peer events are published here
    pub :: !(Publisher PeerEvent),
    -- | network constants
    net :: !Network,
    -- | name of the peer in logs, usually its address
    label :: !Text,
    -- | how to open the connection to the peer
    connect :: !WithConnection,
    -- | log messages here
    logger :: !Logger
  }

-- | Events published about peers.
data PeerEvent
  = -- | the peer completed the version handshake and is online
    PeerConnected !Peer
  | -- | the peer disconnected
    PeerDisconnected !Peer
  | -- | the peer sent a message
    PeerMessage !Peer !Message
  deriving (Eq)

-- | Why a peer process stopped with an error. The peer manager logs it and
-- forgets the peer.
--
-- These used to be calls to 'error' with a message. A type lets callers and
-- tests tell the reasons apart without parsing text.
data PeerException
  = -- | the peer closed the connection in the middle of a message
    ConnectionClosed
  | -- | the 24-byte message header could not be parsed
    InvalidMessageHeader
  | -- | the message is for another network; holds its magic bytes
    WrongNetwork !Word32
  | -- | the payload is larger than allowed for the command; holds its size
    MessageTooLarge !MessageCommand !Word32
  | -- | the payload could not be parsed; holds the reason
    InvalidPayload !MessageCommand !String
  deriving stock (Show)

instance Exception PeerException

-- | Mailbox for a peer. Peers are compared by their mailbox, and shown by their
-- label.
data Peer = Peer
  { -- | send 'PeerMessage's to the peer process here
    mailbox :: !(Mailbox PeerMessage),
    -- | events from this peer are published here
    pub :: !(Publisher PeerEvent),
    -- | name of the peer in logs, usually its address
    label :: !Text,
    -- | whether the peer is reserved for a task; see 'setBusy'
    busy :: !(TVar Bool),
    -- | messages from this peer that the node has not finished handling
    pending :: !(TVar Int)
  }

instance Eq Peer where
  (==) = (==) `on` (.mailbox)

instance Show Peer where
  show = cs . (.label)

-- | Incoming messages that a peer accepts.
data PeerMessage
  = -- | disconnect from the peer
    KillPeer
  | -- | send a network message to the peer
    SendMessage !Message

-- | Build a 'Peer' handle from its configuration, its busy flag, its counter of
-- pending messages, and the mailbox of its process.
wrapPeer ::
  PeerConfig ->
  TVar Bool ->
  TVar Int ->
  Mailbox PeerMessage ->
  Peer
wrapPeer cfg busy pending mbox =
  Peer
    { mailbox = mbox,
      pub = cfg.pub,
      label = cfg.label,
      busy = busy,
      pending = pending
    }

-- | Most messages from one peer that may wait for the node to handle them.
-- When a peer gets this far ahead, the node stops reading from its socket
-- until it catches up, so that a peer that sends faster than the node can
-- handle fills its own TCP window instead of the node's memory.
maxPendingMessages :: Int
maxPendingMessages = 16

-- | Tell the peer process that the node has finished handling one of the
-- messages that it published, so that it may read another one.
peerMessageDone :: Peer -> IO ()
peerMessageDone p = atomically $ modifyTVar' p.pending (max 0 . subtract 1)

-- | Run peer process in current thread. The 'Peer' must be the one that
-- 'wrapPeer' built with the mailbox of this inbox.
--
-- Opens the connection, then reads and publishes messages from the peer while
-- sending the messages that arrive in the inbox. Returns when 'killPeer' is
-- called or the connection is closed, and throws if the peer sends invalid
-- data.
peer ::
  PeerConfig ->
  Peer ->
  Inbox PeerMessage ->
  IO ()
peer cfg@PeerConfig {label, net, pub, logger} p inbox =
  cfg.connect peer_session
  where
    -- Commands from the mailbox, as a stream of messages to send.
    go = do
      m <- liftIO $ do
        logDebug logger $ label <> " awaiting event..."
        receive inbox
      dispatchMessage cfg m >>= bool (return ()) go
    peer_session ad = do
      let ins = ad.inboundConduit
          ons = ad.outboundConduit
          src =
            runConduit $
              ins
                .| inPeerConduit cfg
                .| mapM_C send_msg
          snk = outPeerConduit net .| ons
      withAsync src $ \as -> do
        link as
        runConduit (go .| snk)
    -- Wait until the node has room for another message from this peer.
    send_msg m = do
      atomically $ do
        n <- readTVar p.pending
        check (n < maxPendingMessages)
        writeTVar p.pending (n + 1)
      publish (PeerMessage p m) pub

-- | Internal function to dispatch peer messages.
dispatchMessage ::
  PeerConfig ->
  PeerMessage ->
  ConduitT i Message IO Bool
dispatchMessage PeerConfig {label, logger} (SendMessage m) = do
  liftIO $ logDebug logger (label <> " sending cmd " <> cs (show (msgType m)))
  yield m
  return True
dispatchMessage PeerConfig {label, logger} KillPeer = do
  liftIO $ logInfo logger (label <> " disconnecting")
  return False

-- | Internal conduit to parse messages coming from peer. The header is checked
-- before the payload is read, so that a peer cannot make the node buffer a
-- payload that it will reject anyway. The size limit is the 'maxPayloadSize'
-- of the command. On Bitcoin Cash only messages that carry block content may
-- be larger than 2 MiB. A peer that sends anything invalid is disconnected.
--
-- The counts of list messages are not checked here. 'getMessage' fails as
-- soon as it reads a count above the limit of the message. The limits are
-- 2000 headers and 1000 addresses, for example.
inPeerConduit ::
  PeerConfig -> ConduitT ByteString Message IO ()
inPeerConduit PeerConfig {label, net, logger} = loopForever $ do
  debug (label <> " awaiting message...")
  x <- takeExactly 24
  MessageHeader mgc cmd len _ <- case decode x of
    Left _ -> bad InvalidMessageHeader
    Right h -> return h
  when (mgc /= net.magic) $ bad (WrongNetwork mgc)
  when (len > maxPayloadSize net cmd) $ bad (MessageTooLarge cmd len)
  debug (label <> " sent cmd " <> cs (show cmd))
  y <- takeExactly (fromIntegral len)
  case runGet (getMessage net) $ x `B.append` y of
    Left e -> bad (InvalidPayload cmd (firstLine e))
    Right m -> do
      debug (label <> " sent payload for cmd " <> cs (show cmd))
      yield m
  where
    debug = liftIO . logDebug logger
    takeExactly n = do
      bs <- takeCE n .| foldC
      when (B.length bs /= n) $ bad ConnectionClosed
      return bs
    bad :: PeerException -> ConduitT i o IO a
    bad e = liftIO $ do
      logError logger (label <> ": " <> cs (show e))
      throwIO e
    -- The reason is on the first line of the parser's error text. The lines
    -- after it are call stack noise.
    firstLine = takeWhile (/= '\n')

-- | Outgoing peer conduit to serialize and send messages.
outPeerConduit :: (Monad m) => Network -> ConduitT Message ByteString m ()
outPeerConduit net = awaitForever $ yield . runPut . putMessage net

-- | Ask a peer to disconnect. The peer process stops after sending any messages
-- queued before this one.
killPeer :: Peer -> IO ()
killPeer p = KillPeer `send` p.mailbox

-- | Send a network message to peer. Returns as soon as the message is queued.
sendMessage :: Message -> Peer -> IO ()
sendMessage m p = SendMessage m `send` p.mailbox

-- | Check whether a peer is reserved for a task.
getBusy :: Peer -> IO Bool
getBusy p = readTVarIO p.busy

-- | Try to reserve a peer for a task, such as syncing headers. Returns 'True' if
-- the peer was free and is now reserved, or 'False' if it was already busy.
--
-- Reservation exists because some conversations with a peer span many
-- messages, like downloading a long run of headers. Two tasks using the same peer
-- for such a conversation at once would confuse each other's replies.
setBusy :: Peer -> IO Bool
setBusy p =
  atomically $ do
    b <- readTVar p.busy
    unless b $ writeTVar p.busy True
    return $ not b

-- | Release a peer reserved with 'setBusy'.
setFree :: Peer -> IO ()
setFree p = atomically $ writeTVar p.busy False

-- | Request full blocks from peer. Will return 'Nothing' if the list of blocks
-- returned by the peer is incomplete, comes out of order, or a timeout is
-- reached.
--
-- The 'Int' argument is the timeout in seconds. On networks with SegWit,
-- blocks are requested with their witness data.
getBlocks ::
  Network ->
  Int ->
  Peer ->
  [BlockHash] ->
  IO (Maybe [Block])
getBlocks net time p bhs =
  (>>= traverse block) <$> getData time p (GetData ivs)
  where
    block = either (const Nothing) Just
    c
      | net.segWit = InvWitnessBlock
      | otherwise = InvBlock
    ivs = map (InvVector c . (.get)) bhs

-- | Request transactions from peer. Will return 'Nothing' if the list of
-- transactions returned by the peer is incomplete, comes out of order, or a
-- timeout is reached.
--
-- The 'Int' argument is the timeout in seconds. Peers normally only serve
-- transactions that are in their mempool.
getTxs ::
  Network ->
  Int ->
  Peer ->
  [TxHash] ->
  IO (Maybe [Tx])
getTxs net time p ths =
  (>>= traverse tx) <$> getData time p (GetData ivs)
  where
    tx = either Just (const Nothing)
    c
      | net.segWit = InvWitnessTx
      | otherwise = InvTx
    ivs = map (InvVector c . (.get)) ths

-- | Request transactions and/or blocks from peer. Return 'Nothing' if any
-- single inventory fails to be retrieved, if they come out of order, or if
-- timeout is reached.
--
-- The first argument is the timeout in seconds. A ping is sent right after the
-- request; since peers answer in order, receiving the matching pong before all
-- items means that the peer will not send the rest. Unrelated messages that
-- arrive meanwhile are ignored. Transactions are returned as 'Left' and blocks
-- as 'Right', in the order they were requested. "Haskoin.Node.Peer.Request"
-- has the rules.
getData ::
  Int -> Peer -> GetData -> IO (Maybe [Either Tx Block])
getData seconds p gd@(GetData ivs) =
  withSubscription p.pub $ \inb -> do
    r <- randomIO
    MGetData gd `sendMessage` p
    MPing (Ping r) `sendMessage` p
    join <$> timeout (seconds * 1000 * 1000) (step inb (newRequest r ivs))
  where
    step inb = \case
      Complete items -> return (Just items)
      Failed _ -> return Nothing
      Continue req -> filterReceive p inb >>= step inb . stepRequest req

-- | Ping a peer and await response. Return 'False' if response not received
-- before timeout. The first argument is the timeout in seconds.
pingPeer :: Int -> Peer -> IO Bool
pingPeer time p =
  fmap isJust . withSubscription p.pub $ \sub -> do
    r <- randomIO
    MPing (Ping r) `sendMessage` p
    receiveMatchS time sub $ \case
      PeerMessage p' (MPong (Pong r'))
        | p == p' && r == r' -> Just ()
      _ -> Nothing

-- | Receive the next message from the given peer, discarding events about other
-- peers.
filterReceive :: Peer -> Inbox PeerEvent -> IO Message
filterReceive p inb =
  receive inb >>= \case
    PeerMessage p' m | p == p' -> return m
    _ -> filterReceive p inb
