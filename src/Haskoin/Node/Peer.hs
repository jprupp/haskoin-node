{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

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
module Haskoin.Node.Peer
  ( PeerConfig (..),
    PeerEvent (..),
    Conduits (..),
    WithConnection,
    Peer (..),
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
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Function (on)
import Data.List (union)
import Data.Maybe (isJust)
import Data.Serialize (decode, runGet, runPut)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32)
import Haskoin
import Haskoin.Node.Logger
import Haskoin.Node.Loop (loopForever)
import NQE
import System.Random
import UnliftIO

-- | The two directions of a network connection, as conduits.
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
peerMessageDone :: (MonadIO m) => Peer -> m ()
peerMessageDone p = atomically $ modifyTVar' p.pending (max 0 . subtract 1)

-- | Run peer process in current thread. The 'Peer' must be the one that
-- 'wrapPeer' built with the mailbox of this inbox.
--
-- Opens the connection, then reads and publishes messages from the peer while
-- sending the messages that arrive in the inbox. Returns when 'killPeer' is
-- called or the connection is closed, and throws if the peer sends invalid
-- data.
peer ::
  (MonadUnliftIO m) =>
  PeerConfig ->
  Peer ->
  Inbox PeerMessage ->
  m ()
peer cfg@PeerConfig {label, net, connect, pub, logger} p inbox =
  withRunInIO $ \run -> connect (run . peer_session)
  where
    go = do
      logDebug logger $ label <> " awaiting event..."
      msg <- receive inbox
      dispatchMessage cfg msg >>= bool (return ()) go
    peer_session ad = do
      let ins = transPipe liftIO ad.inboundConduit
          ons = transPipe liftIO ad.outboundConduit
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
    send_msg msg = do
      atomically $ do
        n <- readTVar p.pending
        checkSTM (n < maxPendingMessages)
        writeTVar p.pending (n + 1)
      publish (PeerMessage p msg) pub

-- | Internal function to dispatch peer messages.
dispatchMessage ::
  (MonadIO m) =>
  PeerConfig ->
  PeerMessage ->
  ConduitT i Message m Bool
dispatchMessage PeerConfig {label, logger} (SendMessage msg) = do
  logDebug logger (label <> " sending cmd " <> cs (show (msgType msg)))
  yield msg
  return True
dispatchMessage PeerConfig {label, logger} KillPeer = do
  logInfo logger (label <> " disconnecting")
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
  (MonadIO m) => PeerConfig -> ConduitT ByteString Message m ()
inPeerConduit PeerConfig {label, net, logger} = loopForever $ do
  logDebug logger (label <> " awaiting message...")
  x <- takeCE 24 .| foldC
  when (B.null x) $ do
    logError logger (label <> " sent empty message header")
    error "Peer sent empty message header"
  MessageHeader mgc cmd len _ <- case decode x of
    Left _ -> bad "invalid message header"
    Right h -> return h
  when (mgc /= net.magic) $
    bad ("wrong network magic " <> cs (show mgc))
  when (len > maxPayloadSize net cmd) $
    bad
      ( "too large payload of "
          <> cs (show len)
          <> " bytes for cmd "
          <> cs (show cmd)
      )
  logDebug logger (label <> " sent cmd " <> cs (show cmd))
  y <- takeCE (fromIntegral len) .| foldC
  when (B.length y /= fromIntegral len) $
    bad ("truncated payload for cmd " <> cs (show cmd))
  case runGet (getMessage net) $ x `B.append` y of
    Left e ->
      bad
        ( "invalid payload for cmd "
            <> cs (show cmd)
            <> ": "
            <> cs (firstLine e)
        )
    Right msg -> do
      logDebug
        logger
        (label <> " sent payload for cmd " <> cs (show cmd))
      yield msg
  where
    bad :: (MonadIO m) => Text -> m a
    bad e = do
      logError logger (label <> " sent " <> e)
      error ("Peer sent " <> cs e)
    -- The reason is on the first line of the parser's error text. The lines
    -- after it are call stack noise.
    firstLine = takeWhile (/= '\n')

-- | Outgoing peer conduit to serialize and send messages.
outPeerConduit :: (Monad m) => Network -> ConduitT Message ByteString m ()
outPeerConduit net = awaitForever $ yield . runPut . putMessage net

-- | Ask a peer to disconnect. The peer process stops after sending any messages
-- queued before this one.
killPeer :: (MonadIO m) => Peer -> m ()
killPeer p = KillPeer `send` p.mailbox

-- | Send a network message to peer. Returns as soon as the message is queued.
sendMessage :: (MonadIO m) => Message -> Peer -> m ()
sendMessage msg p = SendMessage msg `send` p.mailbox

-- | Check whether a peer is reserved for a task.
getBusy :: (MonadIO m) => Peer -> m Bool
getBusy p = readTVarIO p.busy

-- | Try to reserve a peer for a task, such as syncing headers. Returns 'True' if
-- the peer was free and is now reserved, or 'False' if it was already busy.
setBusy :: (MonadIO m) => Peer -> m Bool
setBusy p =
  atomically $ do
    b <- readTVar p.busy
    unless b $ writeTVar p.busy True
    return $ not b

-- | Release a peer reserved with 'setBusy'.
setFree :: (MonadIO m) => Peer -> m ()
setFree p = atomically $ writeTVar p.busy False

-- | Request full blocks from peer. Will return 'Nothing' if the list of blocks
-- returned by the peer is incomplete, comes out of order, or a timeout is
-- reached.
--
-- The 'Int' argument is the timeout in seconds. On networks with SegWit,
-- blocks are requested with their witness data.
getBlocks ::
  (MonadUnliftIO m) =>
  Network ->
  Int ->
  Peer ->
  [BlockHash] ->
  m (Maybe [Block])
getBlocks net time p bhs =
  runMaybeT $ mapM f =<< MaybeT (getData time p (GetData ivs))
  where
    f (Right b) = return b
    f (Left _) = MaybeT $ return Nothing
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
  (MonadUnliftIO m) =>
  Network ->
  Int ->
  Peer ->
  [TxHash] ->
  m (Maybe [Tx])
getTxs net time p ths =
  runMaybeT $ mapM f =<< MaybeT (getData time p (GetData ivs))
  where
    f (Right _) = MaybeT $ return Nothing
    f (Left t) = return t
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
-- items means that the peer will not send the rest. Transactions are returned
-- as 'Left' and blocks as 'Right', in the order they were requested.
getData ::
  (MonadUnliftIO m) => Int -> Peer -> GetData -> m (Maybe [Either Tx Block])
getData seconds p gd@(GetData ivs) =
  withSubscription p.pub $ \inb -> do
    r <- randomIO
    MGetData gd `sendMessage` p
    MPing (Ping r) `sendMessage` p
    fmap join
      . timeout (seconds * 1000 * 1000)
      . runMaybeT
      $ get_thing inb r [] ivs
  where
    get_thing _inb _r acc [] =
      return $ reverse acc
    get_thing inb r acc hss@(InvVector t h : hs) =
      lift (filterReceive p inb) >>= \case
        MTx tx
          | is_tx t && (txHash tx).get == h ->
              get_thing inb r (Left tx : acc) hs
        MBlock b@(Block bh _)
          | is_block t && (headerHash bh).get == h ->
              get_thing inb r (Right b : acc) hs
        MNotFound (NotFound nvs)
          | not (null (nvs `union` hs)) ->
              MaybeT $ return Nothing
        MPong (Pong r')
          | r == r' ->
              MaybeT $ return Nothing
        _
          | null acc ->
              get_thing inb r acc hss
          | otherwise ->
              MaybeT $ return Nothing
    is_tx InvWitnessTx = True
    is_tx InvTx = True
    is_tx _ = False
    is_block InvWitnessBlock = True
    is_block InvBlock = True
    is_block _ = False

-- | Ping a peer and await response. Return 'False' if response not received
-- before timeout. The first argument is the timeout in seconds.
pingPeer :: (MonadUnliftIO m) => Int -> Peer -> m Bool
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
filterReceive :: (MonadIO m) => Peer -> Inbox PeerEvent -> m Message
filterReceive p inb =
  receive inb >>= \case
    PeerMessage p' msg | p == p' -> return msg
    _ -> filterReceive p inb
