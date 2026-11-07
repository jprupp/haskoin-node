{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module      : Haskoin.Node
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- A peer-to-peer node for Bitcoin (BTC) and Bitcoin Cash (BCH).
--
-- The node connects to other peers on the network, keeps a number of them
-- online, and synchronises the block chain headers, which it stores in a
-- RocksDB database. It does not download or validate full blocks or
-- transactions by itself: it publishes events and lets the application request
-- whatever data it needs from the connected peers.
--
-- A node is made of two main components, which run in their own threads:
--
-- * The peer manager ("Haskoin.Node.PeerMgr") discovers and connects to peers,
--   performs the version handshake, pings peers to measure latency, and
--   disconnects peers that misbehave, are too slow or have been connected for
--   too long.
--
-- * The chain ("Haskoin.Node.Chain") downloads block headers from peers,
--   connects them to the best chain, and stores them in the database.
--
-- Everything that happens is published as a 'NodeEvent' to the 'Publisher'
-- given in 'NodeConfig'. Subscribe to it to learn about new best blocks, peers
-- connecting and disconnecting, and every message received from a peer, such as
-- inventory announcements, blocks and transactions.
--
-- = Example
--
-- @
-- main :: IO ()
-- main =
--   'withLogger' 'LevelInfo' $ \\logger ->
--   withDB "\/tmp\/node" def{createIfMissing = True} $ \\db ->
--   'withPublisher' $ \\pub ->
--   'withSubscription' pub $ \\events -> do
--     let cfg =
--           'NodeConfig'
--             { maxPeers = 8,
--               db = db,
--               cf = Nothing,
--               peers = [],
--               discover = True,
--               address = NetworkAddress 0 (sockToHostAddress (SockAddrInet 0 0)),
--               net = btc,
--               pub = pub,
--               timeout = 120,
--               maxPeerLife = 48 * 3600,
--               connect = 'withConnection',
--               logger = logger
--             }
--     'withNode' cfg $ \\node -> forever $
--       'receive' events >>= \\case
--         'ChainEvent' ('ChainBestBlock' bn) -> print bn.height
--         _ -> return ()
-- @
--
-- = Requesting data
--
-- Use 'getPeers' to list the peers that are online, then 'getBlocks',
-- 'getTxs' or 'getData' to request data from one of them. Use
-- 'chainGetBlock', 'chainGetBest' and related functions to query the headers
-- that the node has stored.
module Haskoin.Node
  ( -- * Node
    NodeConfig (..),
    NodeEvent (..),
    Node (..),
    withNode,
    withConnection,
    -- * Peers
    module Haskoin.Node.Peer,
    -- * Peer manager
    module Haskoin.Node.PeerMgr,
    -- * Chain
    module Haskoin.Node.Chain,
    -- * Logging
    module Haskoin.Node.Logger,
  )
where

import Control.Monad.Cont (ContT (..), MonadCont (callCC), cont, runCont, runContT)
import Control.Monad.Trans (lift)
import Data.Conduit.Network
import Data.String.Conversions (cs)
import Data.Time.Clock (NominalDiffTime)
import Database.RocksDB (ColumnFamily, DB)
import Haskoin
import Haskoin.Node.Chain
import Haskoin.Node.Logger
import Haskoin.Node.Loop (loopForever)
import Haskoin.Node.Peer
import Haskoin.Node.PeerMgr
import NQE
import Network.Socket
import Text.Read (readMaybe)
import UnliftIO

-- | General node configuration. Passed to 'withNode'.
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
    -- | seconds without a response before a peer is considered unresponsive
    -- and disconnected; also used as timeout for handshakes and for syncing
    -- headers
    timeout :: !NominalDiffTime,
    -- | disconnect a peer after it has been connected for this many seconds,
    -- so that connections rotate over time
    maxPeerLife :: !NominalDiffTime,
    -- | how to open a connection to a peer address; normally
    -- 'withConnection', but can be replaced to use a proxy or for testing
    connect :: !(SockAddr -> WithConnection),
    -- | log messages here
    logger :: !Logger
  }

-- | A running node, as given by 'withNode'. Use its fields to query and
-- control the peer manager and the chain.
data Node = Node
  { -- | the peer manager
    peerMgr :: !PeerMgr,
    -- | the block header chain
    chain :: !Chain
  }

-- | Events published by the node to the 'Publisher' in 'NodeConfig'.
data NodeEvent
  = -- | something happened to the header chain, such as a new best block
    ChainEvent !ChainEvent
  | -- | a peer connected, disconnected or sent a message
    PeerEvent !PeerEvent
  deriving (Eq)

-- | Open a TCP connection to a peer address and pass its conduits to a
-- function. The connection is closed when the function returns. Calls 'error'
-- if the address cannot be converted into a host and port.
withConnection :: SockAddr -> WithConnection
withConnection na f =
  fromSockAddr na >>= \case
    Nothing -> error $ "Invalid address " ++ show na
    Just cset ->
      runTCPClient cset $ \ad ->
        f (Conduits (appSource ad) (appSink ad))

-- | Turn a socket address into client settings for a TCP connection, or
-- 'Nothing' if it cannot be converted.
fromSockAddr :: (MonadUnliftIO m) => SockAddr -> m (Maybe ClientSettings)
fromSockAddr sa = go `catch` e
  where
    go = do
      (maybe_host, maybe_port) <- liftIO $ getNameInfo flags True True sa
      return $
        clientSettings
          <$> (readMaybe =<< maybe_port)
          <*> (cs <$> maybe_host)
    flags = [NI_NUMERICHOST, NI_NUMERICSERV]
    e :: (Monad m) => SomeException -> m (Maybe a)
    e _ = return Nothing

-- | Forward events from the chain to the node publisher, telling the peer
-- manager about every new best block height so that it can tell peers apart
-- that are behind.
chainEvents ::
  (MonadIO m) => PeerMgr -> Inbox ChainEvent -> Publisher NodeEvent -> m ()
chainEvents mgr input output = loopForever $ do
  event <- receive input
  case event of
    ChainBestBlock bb -> peerMgrBest mgr bb.height
    _ -> return ()
  publish (ChainEvent event) output

-- | Route events from peers to the peer manager and chain. A message from a
-- peer is handled to completion before the peer may send another one past its
-- small allowance of pending messages (see 'maxPendingMessages'), so that the
-- node's queues cannot grow faster than it can work. Address and header
-- messages are only taken from peers that completed the handshake.
peerEvents ::
  (MonadIO m) =>
  Chain -> PeerMgr -> Inbox PeerEvent -> Publisher NodeEvent -> m ()
peerEvents ch mgr input output = loopForever $ do
  event <- receive input
  case event of
    PeerConnected p ->
      chainPeerConnected ch p
    PeerDisconnected p ->
      chainPeerDisconnected ch p
    PeerMessage p msg -> do
      case msg of
        MVersion v ->
          peerMgrVersion mgr p v >> peerMgrSync mgr
        MVerAck ->
          peerMgrVerAck mgr p >> peerMgrSync mgr
        MPing (Ping n) ->
          peerMgrPing mgr p n >> peerMgrSync mgr
        MPong (Pong n) ->
          peerMgrPong mgr p n >> peerMgrSync mgr
        MAddr (Addr ns) ->
          whenOnline p $ peerMgrAddrs mgr p (map snd ns) >> peerMgrSync mgr
        MHeaders (Headers hs) ->
          whenOnline p $ chainHeaders ch p (map fst hs) >> chainSync ch
        _ -> return ()
      ticklePeer mgr p
      peerMessageDone p
  publish (PeerEvent event) output
  where
    whenOnline p act =
      getOnlinePeer mgr p >>= \case
        Just OnlinePeer {online = True} -> act
        _ -> return ()

-- | Launch node process in the foreground.
--
-- Start the chain and the peer manager in background threads, pass a 'Node'
-- handle to the given action, and stop everything when the action returns. If
-- any component fails, the exception is re-thrown in the calling thread.
withNode ::
  (MonadUnliftIO m) => NodeConfig -> (Node -> m a) -> m a
withNode NodeConfig {..} action = flip runContT return $ do
  peerPub <- ContT withPublisher
  peerSub <- ContT (withSubscription peerPub)
  chainPub <- ContT withPublisher
  chainSub <- ContT (withSubscription chainPub)
  let peerMgrCfg =
        PeerMgrConfig {pub = peerPub, logger = withSource "PeerMgr" logger, ..}
  let chainCfg =
        ChainConfig {pub = chainPub, logger = withSource "Chain" logger, ..}
  chain <- ContT (withChain chainCfg)
  peerMgr <- ContT $ withPeerMgr peerMgrCfg
  link =<< ContT (withAsync $ chainEvents peerMgr chainSub pub)
  link =<< ContT (withAsync $ peerEvents chain peerMgr peerSub pub)
  lift $ action Node {..}
