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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

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
  )
where

import Conduit
import Control.Exception
import Control.Monad
import Control.Monad.Logger
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
import NQE
import System.Random
import UnliftIO

data Conduits = Conduits
  { inboundConduit :: ConduitT () ByteString IO (),
    outboundConduit :: ConduitT ByteString Void IO ()
  }

type WithConnection = (Conduits -> IO ()) -> IO ()

data PeerConfig = PeerConfig
  { pub :: !(Publisher PeerEvent),
    net :: !Network,
    label :: !Text,
    connect :: !WithConnection
  }

data PeerEvent
  = PeerConnected !Peer
  | PeerDisconnected !Peer
  | PeerMessage !Peer !Message
  deriving (Eq)

-- | Mailbox for a peer.
data Peer = Peer
  { mailbox :: !(Mailbox PeerMessage),
    pub :: !(Publisher PeerEvent),
    label :: !Text,
    busy :: !(TVar Bool)
  }

instance Eq Peer where
  (==) = (==) `on` (.mailbox)

instance Show Peer where
  show = cs . (.label)

-- | Incoming messages that a peer accepts.
data PeerMessage
  = KillPeer
  | SendMessage !Message

wrapPeer ::
  PeerConfig ->
  TVar Bool ->
  Mailbox PeerMessage ->
  Peer
wrapPeer cfg busy mbox =
  Peer
    { mailbox = mbox,
      pub = cfg.pub,
      label = cfg.label,
      busy = busy
    }

-- | Run peer process in current thread.
peer ::
  (MonadLoggerIO m, MonadUnliftIO m) =>
  PeerConfig ->
  TVar Bool ->
  Inbox PeerMessage ->
  m ()
peer cfg@PeerConfig {label, net, connect, pub} busy inbox = do
  let p = wrapPeer cfg busy (inboxToMailbox inbox)
  withRunInIO $ \run -> connect (run . peer_session p)
  where
    go = do
      $(logDebugS) "Peer" $ label <> " awaiting event..."
      msg <- receive inbox
      dispatchMessage cfg msg >>= bool (return ()) go
    peer_session p ad = do
      let ins = transPipe liftIO ad.inboundConduit
          ons = transPipe liftIO ad.outboundConduit
          src =
            runConduit $
              ins
                .| inPeerConduit cfg
                .| mapM_C (send_msg p)
          snk = outPeerConduit net .| ons
      withAsync src $ \as -> do
        link as
        runConduit (go .| snk)
    send_msg p msg = publish (PeerMessage p msg) pub

-- | Internal function to dispatch peer messages.
dispatchMessage ::
  (MonadLoggerIO m) =>
  PeerConfig ->
  PeerMessage ->
  ConduitT i Message m Bool
dispatchMessage PeerConfig {label} (SendMessage msg) = do
  $(logDebugS) "Peer" (label <> " sending: " <> cs (show msg))
  yield msg
  return True
dispatchMessage PeerConfig {label} KillPeer = do
  $(logInfoS) "Peer" (label <> " disconnecting")
  return False

-- | Internal conduit to parse messages coming from peer.
inPeerConduit :: (MonadLoggerIO m) => PeerConfig -> ConduitT ByteString Message m ()
inPeerConduit pc@PeerConfig {label, net} = do
  $(logDebugS) "Peer" (label <> ": awaiting message...")
  x <- takeCE 24 .| foldC
  when (B.null x) $ do
    $(logWarnS) "Peer" (label <> " sent empty header")
  case decode x of
    Left e -> do
      $(logWarnS) "Peer" (label <> " sent invalid header")
    Right (MessageHeader _ cmd len _)
      | len > 32 * 2 ^ (20 :: Int) ->
          $(logWarnS) "Peer" $
            label
              <> " wants to send "
              <> cs (show len)
              <> " bytes (too large) for cmd "
              <> cs (show cmd)
      | otherwise -> do
          $(logDebugS) "Peer" (label <> " sent cmd " <> cs (show cmd))
          y <- takeCE (fromIntegral len) .| foldC
          case runGet (getMessage net) $ x `B.append` y of
            Left e ->
              $(logErrorS)
                "Peer"
                (label <> ": sent invalid payload for cmd " <> cs (show cmd))
            Right msg -> do
              $(logDebugS)
                "Peer"
                (label <> " sent full message for cmd " <> cs (show cmd))
              yield msg
              inPeerConduit pc

-- | Outgoing peer conduit to serialize and send messages.
outPeerConduit :: (Monad m) => Network -> ConduitT Message ByteString m ()
outPeerConduit net = awaitForever $ yield . runPut . putMessage net

-- | Kill a peer with the provided exception.
killPeer :: (MonadIO m) => Peer -> m ()
killPeer p = KillPeer `send` p.mailbox

-- | Send a network message to peer.
sendMessage :: (MonadIO m) => Message -> Peer -> m ()
sendMessage msg p = SendMessage msg `send` p.mailbox

getBusy :: (MonadIO m) => Peer -> m Bool
getBusy p = readTVarIO p.busy

setBusy :: (MonadIO m) => Peer -> m Bool
setBusy p =
  atomically $ do
    b <- readTVar p.busy
    unless b $ writeTVar p.busy True
    return $ not b

setFree :: (MonadIO m) => Peer -> m ()
setFree p = atomically $ writeTVar p.busy False

-- | Request full blocks from peer. Will return 'Nothing' if the list of blocks
-- returned by the peer is incomplete, comes out of order, or a timeout is
-- reached.
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
-- before timeout.
pingPeer :: (MonadUnliftIO m) => Int -> Peer -> m Bool
pingPeer time p =
  fmap isJust . withSubscription p.pub $ \sub -> do
    r <- randomIO
    MPing (Ping r) `sendMessage` p
    receiveMatchS time sub $ \case
      PeerMessage p' (MPong (Pong r'))
        | p == p' && r == r' -> Just ()
      _ -> Nothing

filterReceive :: (MonadIO m) => Peer -> Inbox PeerEvent -> m Message
filterReceive p inb =
  receive inb >>= \case
    PeerMessage p' msg | p == p' -> return msg
    _ -> filterReceive p inb
