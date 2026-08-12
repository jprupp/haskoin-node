{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Haskoin.Node
  ( module Haskoin.Node.Peer,
    module Haskoin.Node.PeerMgr,
    module Haskoin.Node.Chain,
    NodeConfig (..),
    NodeEvent (..),
    Node (..),
    withNode,
    withConnection,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Logging
import Control.Monad (forever)
import Control.Monad.Cont (ContT (..), MonadCont (callCC), cont, runCont, runContT)
import Control.Monad.Trans (lift)
import Data.Conduit.Network
import Data.String.Conversions (cs)
import Data.Time.Clock (NominalDiffTime)
import Database.RocksDB (ColumnFamily, DB)
import Haskoin
import Haskoin.Node.Chain
import Haskoin.Node.Peer
import Haskoin.Node.PeerMgr
import NQE
import Network.Socket
import Text.Read (readMaybe)

-- | General node configuration.
data NodeConfig = NodeConfig
  { -- | maximum number of connected peers allowed
    maxPeers :: !Int,
    -- | database handler
    db :: !DB,
    -- | database column family
    cf :: !(Maybe ColumnFamily),
    -- | static list of peers to connect to
    peers :: ![String],
    -- | activate peer discovery
    discover :: !Bool,
    -- | network address for the local host
    address :: !NetworkAddress,
    -- | network constants
    net :: !Network,
    -- | node events are sent to this publisher
    pub :: !(Publisher NodeEvent),
    -- | timeout in seconds
    timeout :: !NominalDiffTime,
    -- | peer disconnect after seconds
    maxPeerLife :: !NominalDiffTime,
    connect :: !(SockAddr -> WithConnection)
  }

data Node = Node
  { peerMgr :: !PeerMgr,
    chain :: !Chain
  }

data NodeEvent
  = ChainEvent !ChainEvent
  | PeerEvent !PeerEvent
  deriving (Eq)

withConnection :: SockAddr -> WithConnection
withConnection na f =
  fromSockAddr na >>= \case
    Nothing -> errorSL "Node" ("Peer address invalid: " <> cs (show na))
    Just cset ->
      runTCPClient cset $ \ad ->
        f (Conduits (appSource ad) (appSink ad))

fromSockAddr :: SockAddr -> IO (Maybe ClientSettings)
fromSockAddr sa = go `catch` e
  where
    go = do
      (maybe_host, maybe_port) <- getNameInfo flags True True sa
      return $
        clientSettings
          <$> (readMaybe =<< maybe_port)
          <*> (cs <$> maybe_host)
    flags = [NI_NUMERICHOST, NI_NUMERICSERV]
    e :: (Monad m) => SomeException -> m (Maybe a)
    e _ = return Nothing

chainEvents :: PeerMgr -> Inbox ChainEvent -> Publisher NodeEvent -> IO ()
chainEvents mgr input output = forever $ do
  event <- receive input
  case event of
    ChainBestBlock bb -> peerMgrBest mgr bb.height
    _ -> return ()
  publish (ChainEvent event) output

peerEvents ::
  Chain -> PeerMgr -> Inbox PeerEvent -> Publisher NodeEvent -> IO ()
peerEvents ch mgr input output = forever $ do
  event <- receive input
  case event of
    PeerConnected p ->
      chainPeerConnected ch p
    PeerDisconnected p ->
      chainPeerDisconnected ch p
    PeerMessage p msg -> do
      case msg of
        MVersion v ->
          peerMgrVersion mgr p v
        MVerAck ->
          peerMgrVerAck mgr p
        MPing (Ping n) ->
          peerMgrPing mgr p n
        MPong (Pong n) ->
          peerMgrPong mgr p n
        MAddr (Addr ns) ->
          peerMgrAddrs mgr p (map snd ns)
        MHeaders (Headers hs) ->
          chainHeaders ch p (map fst hs)
        _ -> return ()
      ticklePeer mgr p
  publish (PeerEvent event) output

-- | Launch node process in the foreground.
withNode :: NodeConfig -> (Node -> IO a) -> IO a
withNode NodeConfig {..} action = flip runContT return $ do
  peerPub <- ContT withPublisher
  peerSub <- ContT (withSubscription peerPub)
  chainPub <- ContT withPublisher
  chainSub <- ContT (withSubscription chainPub)
  let peerMgrCfg = PeerMgrConfig {pub = peerPub, ..}
  let chainCfg = ChainConfig {pub = chainPub, ..}
  chain <- ContT (withChain chainCfg)
  peerMgr <- ContT $ withPeerMgr peerMgrCfg
  lift . link =<< ContT (withAsync $ chainEvents peerMgr chainSub pub)
  lift . link =<< ContT (withAsync $ peerEvents chain peerMgr peerSub pub)
  lift $ action Node {..}
