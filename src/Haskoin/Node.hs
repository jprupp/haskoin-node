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
--   connects them to the best chain, and stores them in the database. It
--   follows the announcements of new blocks, whether they come as headers or
--   as inventory, and polls peers when it has heard nothing for a while.
--
-- Everything that happens is published as a 'NodeEvent' to the 'Publisher'
-- given in 'NodeConfig'. Subscribe to it to learn about new best blocks, peers
-- connecting and disconnecting, and every message received from a peer, such as
-- inventory announcements, blocks and transactions.
--
-- = Purpose
--
-- Talking to the Bitcoin network well is a job of its own, independent of what
-- a program does with the data. Peers have to be found, kept, measured and
-- dropped; messages have to be parsed defensively, because anyone can connect;
-- and the header chain, the backbone that tells which blocks are real, has to
-- be followed through reorganisations. This library does that job and nothing
-- else, so that wallets, indexers and monitors can be built on it without each
-- reimplementing it.
--
-- = Design
--
-- * __Headers only.__ The node syncs block headers, which are small and enough
--   to know the best chain, and leaves blocks and transactions to the
--   application. Applications differ wildly in what they need: an indexer wants
--   every block, a wallet a few transactions, a monitor nothing but
--   announcements. Fetching on request serves all of them.
--
-- * __Events, not callbacks.__ Everything the node learns is published. The
--   node does not know who listens or how many listeners there are, and a slow
--   listener cannot reach into the node's own state.
--
-- * __Processes with one job each.__ The peer manager, the chain and every
--   peer connection are separate NQE processes. Each owns its state and is fed
--   by messages, which keeps the concurrency easy to reason about and lets a
--   failing peer die without disturbing the rest.
--
-- * __Plain 'IO'.__ Every function runs in 'IO', without monad transformers
--   or @MonadIO@ and @MonadUnliftIO@ constraints. The types say exactly what
--   runs, GHC has nothing to specialise, and the long-running loops of the node
--   cannot leak memory through an unspecialised dictionary (see
--   "Haskoin.Node.Loop"). Programs that work in their own monad call the node
--   with @liftIO@.
--
-- * __Trust nothing from the network.__ Peers are strangers. Every message is
--   checked against size limits before it is read, invalid input disconnects
--   the peer, and a peer cannot make the node allocate memory faster than it
--   can work.
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

import Control.Concurrent.Async (link, withAsync)
import Data.Time.Clock (NominalDiffTime)
import Database.RocksDB (ColumnFamily, DB)
import Haskoin
import Haskoin.Node.Address (withConnection)
import Haskoin.Node.Chain
import Haskoin.Node.Logger
import Haskoin.Node.Loop (loopForever)
import Haskoin.Node.Peer
import Haskoin.Node.PeerMgr
import NQE
import Network.Socket (SockAddr)

-- | General node configuration. Passed to 'withNode'.
--
-- Connecting is a parameter ('connect') rather than hard-wired so that tests can
-- plug in simulated peers, and deployments can route through a proxy.
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
  deriving stock (Eq)

-- | Forward events from the chain to the node publisher, telling the peer
-- manager about every new best block height so that it can tell peers apart
-- that are behind.
chainEvents ::
  PeerMgr -> Inbox ChainEvent -> Publisher NodeEvent -> IO ()
chainEvents mgr input output = loopForever $ do
  event <- receive input
  case event of
    ChainBestBlock bb -> peerMgrBest mgr bb.height
    _ -> return ()
  publish (ChainEvent event) output

-- | Route events from peers to the peer manager and chain. A message from a
-- peer is handled to completion before the peer may send another one past its
-- small allowance of pending messages (see 'maxPendingMessages'), so that the
-- node's queues cannot grow faster than it can work. Address, header and
-- inventory messages are only taken from peers that are online: the handshake
-- is complete and the peer is not being disconnected.
-- Inventory with blocks goes to the chain, which asks for headers if it does
-- not know them.
peerEvents ::
  Chain -> PeerMgr -> Inbox PeerEvent -> Publisher NodeEvent -> IO ()
peerEvents ch mgr input output = loopForever $ do
  event <- receive input
  case event of
    PeerConnected p ->
      chainPeerConnected ch p
    PeerDisconnected p ->
      chainPeerDisconnected ch p
    PeerMessage p m -> do
      case m of
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
        MInv (Inv ivs) -> case [BlockHash iv.invHash | iv <- ivs, isBlock iv] of
          [] -> return ()
          bhs -> whenOnline p $ chainBlocksAnnounced ch p bhs >> chainSync ch
        _ -> return ()
      ticklePeer mgr p
      peerMessageDone p
  publish (PeerEvent event) output
  where
    whenOnline p act =
      getOnlinePeer mgr p >>= \case
        Just o | o.online -> act
        _ -> return ()
    isBlock iv = iv.invType `elem` [InvBlock, InvWitnessBlock]

-- | Launch node process in the foreground.
--
-- Start the chain and the peer manager in background threads, pass a 'Node'
-- handle to the given action, and stop everything when the action returns. If
-- any component fails, the exception is re-thrown in the calling thread.
withNode ::
  NodeConfig -> (Node -> IO a) -> IO a
withNode cfg action =
  withPublisher $ \peerPub ->
    withSubscription peerPub $ \peerSub ->
      withPublisher $ \chainPub ->
        withSubscription chainPub $ \chainSub ->
          withChain (chainConfig chainPub) $ \chain ->
            withPeerMgr (peerMgrConfig peerPub) $ \peerMgr ->
              withAsync (chainEvents peerMgr chainSub cfg.pub) $ \a1 ->
                withAsync (peerEvents chain peerMgr peerSub cfg.pub) $ \a2 -> do
                  link a1
                  link a2
                  action Node {peerMgr = peerMgr, chain = chain}
  where
    chainConfig pub =
      ChainConfig
        { db = cfg.db,
          cf = cfg.cf,
          net = cfg.net,
          pub = pub,
          timeout = cfg.timeout,
          logger = withSource "Chain" cfg.logger
        }
    peerMgrConfig pub =
      PeerMgrConfig
        { maxPeers = cfg.maxPeers,
          peers = cfg.peers,
          discover = cfg.discover,
          address = cfg.address,
          net = cfg.net,
          pub = pub,
          timeout = cfg.timeout,
          maxPeerLife = cfg.maxPeerLife,
          connect = cfg.connect,
          logger = withSource "PeerMgr" cfg.logger
        }
