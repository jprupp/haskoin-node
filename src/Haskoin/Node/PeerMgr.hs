-- |
-- Module      : Haskoin.Node.PeerMgr
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- The peer manager keeps the node connected to the network.
--
-- It keeps a book of peer addresses, filled from the static peers in the
-- configuration, from the DNS seeds of the network when discovery is on, and
-- from addresses relayed by connected peers. A background loop connects to new
-- peers while there are fewer than 'maxPeers' of them.
--
-- For each connection, the peer manager sends a version message, waits for the
-- peer's version and acknowledgement, and then announces the peer as connected
-- with a 'PeerConnected' event. Peers are pinged periodically to measure their
-- latency, and the list returned by 'getPeers' is sorted by median ping time, so
-- that the fastest peers come first. A peer is disconnected if it stops
-- responding, does not answer a ping in time, does not complete the handshake
-- in time, or has been connected for longer than 'maxPeerLife'. A
-- 'PeerDisconnected' event is published when an announced peer goes away.
--
-- Most of the @peerMgr*@ functions are used by "Haskoin.Node" to feed messages
-- received from peers to the peer manager; applications rarely need to call
-- them directly.
--
-- = State machines
--
-- The rules live in pure modules, and this one stores their state and carries
-- out what they decide:
--
-- * "Haskoin.Node.PeerMgr.Lifecycle": the life of each connection, from the
--   handshake ("Haskoin.Node.PeerMgr.Handshake") through pings to its
--   disconnection, with a reason.
--
-- * "Haskoin.Node.PeerMgr.Addresses": which address to try next, and how long
--   to wait before trying an address again after its connection ended.
--
-- = Why it works this way
--
-- * __Peers are ranked by measured latency.__ The fastest peers deliver
--   blocks and transactions soonest, so applications that pick the first peer
--   from 'getPeers' get the best service without measuring anything themselves.
--
-- * __Connections rotate.__ Peers are dropped after 'maxPeerLife' even when
--   they behave. A node that keeps the same few peers forever is easier to
--   isolate from the honest network, and stuck with them if they degrade.
--   Rotation keeps the peer set fresh.
--
-- * __Silence is failure.__ A peer that sends nothing, or ignores a ping, for
--   longer than 'timeout' is dropped. Waiting for a peer that has gone away
--   would stall whatever was waiting on it.
--
-- * __A dropped peer is gone at once.__ A peer told to disconnect is no longer
--   offered by 'getPeers' and its messages are no longer routed, even before
--   its connection closes.
--
-- * __Bad addresses wait.__ An address that fails is tried again later each
--   time, and one whose peer broke the protocol is not tried again for a long
--   time, so the node does not hammer dead peers or return to bad ones.
--
-- * __Relayed addresses are untrusted.__ Peers gossip addresses, and a
--   malicious peer could gossip private or loopback addresses to make the node
--   probe its own network. Only public addresses are accepted, from gossip and
--   from DNS seeds alike, and only a bounded number are kept, so gossip cannot
--   exhaust memory.
--
-- * __One connection per network group.__ Addresses that the node learned are
--   picked at random, and at most one connection is open to each network group
--   (a /16 of IPv4, a /32 of IPv6; see 'Haskoin.Node.Address.netGroup'), as in
--   Bitcoin Core. One operator with one block of addresses can then hold one
--   slot, not all of them. Configured peers are exempt: the operator chose
--   them.
module Haskoin.Node.PeerMgr
  ( PeerMgrConfig (..),
    PeerEvent (..),
    OnlinePeer (..),
    PeerState (..),
    Stage (..),
    DisconnectReason (..),
    PeerMgr,
    withPeerMgr,
    peerMgrBest,
    peerMgrVersion,
    peerMgrPing,
    peerMgrPong,
    peerMgrAddrs,
    peerMgrVerAck,
    peerMgrSync,
    getPeers,
    getOnlinePeer,
    ticklePeer,
    buildVersion,
    myVersion,
    toSockAddr,
    toHostService,
    isRoutable,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, ExceptionInLinkedThread (..), link, withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException, fromException)
import Control.Monad (forM_, when, (<=<))
import Data.Function (on)
import Data.List (find, sort)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word64)
import GHC.Records (HasField (..))
import Haskoin
import Haskoin.Node.Address (NetGroup, isRoutable, netGroup, toHostService, toSockAddr)
import Haskoin.Node.Logger
import Haskoin.Node.Loop (loopForever)
import Haskoin.Node.Peer
import Haskoin.Node.Peer.Lock (Lock (..))
import Haskoin.Node.PeerMgr.Addresses
import Haskoin.Node.PeerMgr.Handshake
import Haskoin.Node.PeerMgr.Lifecycle
import NQE
import Network.Socket (SockAddr)
import System.Random (randomIO, randomRIO)

-- | Configuration of the peer manager. "Haskoin.Node" builds it from the
-- 'Haskoin.Node.NodeConfig'.
data PeerMgrConfig = PeerMgrConfig
  { -- | maximum number of peers to connect to
    maxPeers :: !Int,
    -- | static peers to connect to, as @host@ or @host:port@; IPv6 addresses
    -- go in square brackets, as in @[::1]:8333@
    peers :: ![String],
    -- | connect to the network seeds and to addresses relayed by peers
    discover :: !Bool,
    -- | address of this node, sent in version messages
    address :: !NetworkAddress,
    -- | network constants
    net :: !Network,
    -- | peer events are published here
    pub :: !(Publisher PeerEvent),
    -- | seconds without a message from a peer, or without an answer to a
    -- ping, before it is disconnected
    timeout :: !NominalDiffTime,
    -- | seconds after which a peer is disconnected regardless
    maxPeerLife :: !NominalDiffTime,
    -- | how to open a connection to a peer address
    connect :: !(SockAddr -> WithConnection),
    -- | log messages here
    logger :: !Logger,
    -- | look up a stored block header; given to every peer as the
    -- @lookupBlock@ of its t'PeerConfig', to check the blocks it sends
    lookupBlock :: !(BlockHash -> IO (Maybe BlockNode))
  }

-- | Handle to a running peer manager. Obtain one with 'withPeerMgr', or from
-- the 'Haskoin.Node.Node' returned by 'Haskoin.Node.withNode'.
data PeerMgr = PeerMgr
  { -- | configuration
    config :: !PeerMgrConfig,
    -- | supervisor that runs the peer processes
    supervisor :: !Supervisor,
    -- | mailbox of the peer manager process
    mailbox :: !(Mailbox PeerMgrMessage),
    -- | height of the best block, sent to peers in version messages
    best :: !(TVar BlockHeight),
    -- | addresses to connect to
    addresses :: !(TVar (AddressBook SockAddr)),
    -- | open connections, fastest peers first
    peers :: !(TVar [OnlinePeer])
  }

-- | Messages handled by the peer manager process.
data PeerMgrMessage
  = -- | connect to a peer address
    Connect !SockAddr
  | -- | check the timers of a peer
    CheckPeer !Peer
  | -- | a peer process stopped, with the exception that stopped it
    PeerDied !Child !(Maybe SomeException)
  | -- | the height of the best block changed
    ManagerBest !BlockHeight
  | -- | a peer acknowledged our version
    PeerVerAck !Peer
  | -- | a peer sent its version
    PeerVersion !Peer !Version
  | -- | a peer pinged us with a nonce
    PeerPing !Peer !Word64
  | -- | a peer answered a ping with a nonce
    PeerPong !Peer !Word64
  | -- | a peer relayed addresses
    PeerAddrs !Peer ![NetworkAddress]
  | -- | reply once every previous message has been handled
    Sync !(Listen ())

-- | A connection to a peer.
--
-- Peers are compared for equality by their 'Peer' handle, and ordered by the
-- median of their recent ping times, so that sorting a list puts the fastest
-- peers first. Peers without any ping measurement sort as if their median ping
-- were 60 seconds.
--
-- The fields @version@, @online@, @verack@, @ping@, @pings@, @connected@ and
-- @tickled@ of earlier releases are now computed from 'state', so record dot
-- syntax such as @o.version@ keeps working. They cannot be set or pattern
-- matched.
data OnlinePeer = OnlinePeer
  { -- | network address of the peer
    address :: !SockAddr,
    -- | stage of the connection and its timers; see
    -- "Haskoin.Node.PeerMgr.Lifecycle"
    state :: !PeerState,
    -- | thread running the peer process
    async :: !(Async ()),
    -- | handle to talk to the peer
    mailbox :: !Peer,
    -- | nonce of the version message we sent, used to detect connections to
    -- ourselves
    nonce :: !Word64
  }

instance Eq OnlinePeer where
  (==) = (==) `on` (.mailbox)

instance Ord OnlinePeer where
  compare = compare `on` \o -> fromMaybe 60 (median o.state.pings)

-- | Version message sent by the peer, if received.
instance HasField "version" OnlinePeer (Maybe Version) where
  getField o = stageVersion o.state

-- | Whether the peer is online: the handshake is complete and the peer is not
-- being disconnected.
instance HasField "online" OnlinePeer Bool where
  getField o = isOnline o.state

-- | Whether the peer acknowledged our version.
instance HasField "verack" OnlinePeer Bool where
  getField o = case o.state.stage of
    Handshaking h -> hasVerAck h
    Online _ _ -> True
    Disconnecting _ -> False

-- | Time and nonce of the ping awaiting an answer, if any.
instance HasField "ping" OnlinePeer (Maybe (UTCTime, Word64)) where
  getField o = pendingPing o.state

-- | Round-trip times of the last eleven pings, sorted.
instance HasField "pings" OnlinePeer [NominalDiffTime] where
  getField o = sort o.state.pings

-- | When the connection was opened.
instance HasField "connected" OnlinePeer UTCTime where
  getField o = o.state.connected

-- | When the peer last sent a message.
instance HasField "tickled" OnlinePeer UTCTime where
  getField o = o.state.lastHeard

-- | Start a peer manager and pass it to the given action. The peer manager and
-- all its peers are stopped when the action returns.
--
-- The peer manager does not connect to any peer until it learns the height of
-- the best block through 'peerMgrBest'. "Haskoin.Node" does this as soon as the
-- chain is loaded.
withPeerMgr :: PeerMgrConfig -> (PeerMgr -> IO a) -> IO a
withPeerMgr cfg action = do
  inbox <- newInbox
  let died (a, e) = PeerDied a e `sendSTM` inbox
  withSupervisor (Notify died) $ \sup -> do
    best <- newTVarIO 0
    addresses <- newTVarIO emptyBook
    peers <- newTVarIO []
    let mgr =
          PeerMgr
            { config = cfg,
              supervisor = sup,
              mailbox = inboxToMailbox inbox,
              best = best,
              addresses = addresses,
              peers = peers
            }
    withAsync (peerManager mgr inbox) $ \a ->
      withConnectLoop mgr (link a >> action mgr)

-- | Main loop of the peer manager process. Waits for the best block height, then
-- handles messages forever.
peerManager :: PeerMgr -> Inbox PeerMgrMessage -> IO ()
peerManager mgr inbox = do
  logDebug mgr.config.logger "Getting best block..."
  (atomically . writeTVar mgr.best) <=< receiveMatch inbox $ \case
    ManagerBest b -> Just b
    _ -> Nothing
  loopForever $ do
    logDebug mgr.config.logger "Awaiting event..."
    dispatch mgr =<< receive inbox

-- | Time limits of connections.
limits :: PeerMgr -> Limits
limits mgr = Limits {timeout = mgr.config.timeout, maxLife = mgr.config.maxPeerLife}

-- | Handle a message sent to the peer manager.
dispatch :: PeerMgr -> PeerMgrMessage -> IO ()
dispatch mgr = \case
  PeerVersion p v -> do
    logDebug mgr.config.logger ("Received version from peer " <> p.label)
    nonces <- map (.nonce) <$> readTVarIO mgr.peers
    peerInput mgr p $ either Refused (GotHandshake . GotVersion) (checkVersion mgr.config.net nonces v)
  PeerVerAck p -> do
    logDebug mgr.config.logger ("Received version ack from peer " <> p.label)
    peerInput mgr p (GotHandshake GotVerAck)
  PeerPong p n -> do
    now <- getCurrentTime
    peerInput mgr p (GotPong now n)
  CheckPeer p -> do
    now <- getCurrentTime
    nonce <- randomIO
    peerInput mgr p (Tick now nonce)
  PeerPing p n -> MPong (Pong n) `sendMessage` p
  PeerAddrs p nas -> relayedAddresses mgr p nas
  ManagerBest h -> atomically $ writeTVar mgr.best h
  Connect sa -> connectPeer mgr sa
  PeerDied a e -> peerDied mgr a e
  Sync r -> atomically (r ())

-- | Apply an input to the state of a connection and carry out the resulting
-- actions. Inputs for connections that are gone are ignored.
peerInput :: PeerMgr -> Peer -> PeerInput -> IO ()
peerInput mgr p input = do
  result <- atomically $ do
    ops <- readTVar mgr.peers
    case find ((== p) . (.mailbox)) ops of
      Nothing -> return Nothing
      Just o -> do
        let (st, actions) = stepPeer (limits mgr) input o.state
            o' = o {state = st}
        writeTVar mgr.peers (sort (o' : filter (/= o) ops))
        return (Just (o', actions))
  forM_ result $ \(o, actions) -> forM_ actions (perform o)
  where
    perform o = \case
      ReplyVerAck -> MVerAck `sendMessage` p
      Announce -> do
        atomically $ modifyTVar' mgr.addresses (markConnected o.address)
        logInfo mgr.config.logger ("Connected to peer " <> p.label)
        publish (PeerConnected p) mgr.config.pub
        logConnectedPeers mgr
      SendPing n -> MPing (Ping n) `sendMessage` p
      Disconnect r -> do
        logWarn
          mgr.config.logger
          ("Disconnecting peer " <> p.label <> ": " <> cs (show r))
        killPeer p

-- | Record that a peer just sent a message, so that it is not disconnected for
-- being idle. "Haskoin.Node" calls this for every message received.
ticklePeer :: PeerMgr -> Peer -> IO ()
ticklePeer mgr p = getCurrentTime >>= peerInput mgr p . Heard

-- | Learn the routable addresses that an online peer relayed, if discovery is
-- on.
relayedAddresses :: PeerMgr -> Peer -> [NetworkAddress] -> IO ()
relayedAddresses mgr p nas
  | mgr.config.discover =
      getOnlinePeer mgr p >>= \case
        Just o | isOnline o.state -> do
          let sas = filter isRoutable $ map (hostToSockAddr . (.address)) nas
          logDebug
            mgr.config.logger
            ( "Received "
                <> cs (show (length nas))
                <> " addresses from peer "
                <> p.label
                <> ", "
                <> cs (show (length sas))
                <> " routable"
            )
          atomically . modifyTVar' mgr.addresses $ \book ->
            foldr (addAddress Discovered) book sas
        _ ->
          logDebug
            mgr.config.logger
            ("Ignoring addresses from peer that is not online " <> p.label)
  | otherwise =
      logDebug mgr.config.logger ("Ignoring addresses from peer " <> p.label)

-- | Forget a connection whose process stopped, decide when to try its address
-- again, and publish its disconnection if it had been announced.
peerDied :: PeerMgr -> Child -> Maybe SomeException -> IO ()
peerDied mgr a e = do
  now <- getCurrentTime
  gone <- atomically $ do
    ops <- readTVar mgr.peers
    case find ((== a) . (.async)) ops of
      Nothing -> return Nothing
      Just o -> do
        let outcome = connectionOutcome o.state e
        writeTVar mgr.peers (filter (/= o) ops)
        modifyTVar' mgr.addresses (finishAddress now outcome o.address)
        return (Just (o, outcome))
  case gone of
    Nothing -> logError mgr.config.logger "Unknown peer process stopped"
    Just (o, outcome) -> do
      logInfo mgr.config.logger $
        "Connection to peer "
          <> o.mailbox.label
          <> " ended: "
          <> cs (show outcome)
          <> maybe "" (\x -> " (" <> cs (show x) <> ")") e
      when o.state.announced $
        publish (PeerDisconnected o.mailbox) mgr.config.pub
      logConnectedPeers mgr

-- | How a connection ended, from its last state and the exception that stopped
-- its process, if any.
--
-- A peer process only stops without an exception when told to disconnect. If
-- the peer manager did not do that, another process did, such as the chain
-- after invalid headers, so the address waits as after a failure.
connectionOutcome :: PeerState -> Maybe SomeException -> Outcome
connectionOutcome st e =
  case (st.stage, peerException =<< e) of
    (Disconnecting TooOld, _) -> Disconnected
    (Disconnecting (Rejected _), _) -> Misbehaved
    (Disconnecting _, _) -> Failed
    (_, Just ConnectionClosed)
      | st.announced -> Disconnected
      | otherwise -> Failed
    (_, Just _) -> Misbehaved
    (_, Nothing) -> Failed
  where
    peerException x =
      fromException x
        <|> (fromException x >>= \(ExceptionInLinkedThread _ inner) -> fromException inner)

-- | Log how many peers are online out of the maximum.
logConnectedPeers :: PeerMgr -> IO ()
logConnectedPeers mgr = do
  l <- length <$> getPeers mgr
  logInfo
    mgr.config.logger
    ("Peers connected: " <> cs (show l) <> "/" <> cs (show mgr.config.maxPeers))

-- | Next address to connect to. The configured peers and, if discovery is on,
-- the seeds are loaded when there is nothing to try and they were not loaded
-- recently.
--
-- Configured peers come first. Otherwise an address is picked at random among
-- those in a network group that the node is not connected to yet, as Bitcoin
-- Core requires of its automatic outbound connections (see 'netGroup').
getNewPeer :: PeerMgr -> IO (Maybe SockAddr)
getNewPeer mgr = do
  now <- getCurrentTime
  reload <- needsReload now <$> readTVarIO mgr.addresses
  when reload $ loadPeers mgr now
  r <- randomIO
  atomically $ do
    used <- usedGroups <$> readTVar mgr.peers
    stateTVar mgr.addresses $ \book ->
      case takeAddress now r ((`notElem` used) . netGroup) book of
        Just (sa, book') -> (Just sa, book')
        Nothing -> (Nothing, book)

-- | Network groups of the open connections.
usedGroups :: [OnlinePeer] -> [NetGroup]
usedGroups = map (netGroup . (.address))

-- | Resolve the configured peers and, if discovery is on, the seeds of the
-- network, and add them to the address book. Seed results must be routable,
-- like relayed addresses: whoever answers the DNS query could otherwise point
-- the node at its own network. Bitcoin Core's address manager refuses them
-- too. On regtest the seed is @localhost@, so local peers there have to be
-- configured.
loadPeers :: PeerMgr -> UTCTime -> IO ()
loadPeers mgr now = do
  let net = mgr.config.net
      resolve = fmap concat . mapM (toSockAddr net)
  configured <- resolve mgr.config.peers
  seeds <- if mgr.config.discover then filter isRoutable <$> resolve net.seeds else return []
  atomically . modifyTVar' mgr.addresses $ \book ->
    markLoaded now $
      foldr (addAddress Seeded) (foldr (addAddress Configured) book configured) seeds

-- | Start a peer process for an address taken from the address book and send
-- it our version, unless there are no free slots, the address is connected
-- already, or it is a seed or gossiped address in a network group that the
-- node is connected to already. Then the address is given back.
connectPeer :: PeerMgr -> SockAddr -> IO ()
connectPeer mgr sa = do
  ops <- readTVarIO mgr.peers
  source <- addressSource sa <$> readTVarIO mgr.addresses
  if
    | length ops >= mgr.config.maxPeers -> giveBack "no free slots"
    | any ((== sa) . (.address)) ops -> giveBack "already connected"
    | source /= Just Configured && netGroup sa `elem` usedGroups ops ->
        giveBack "network group in use"
    | otherwise -> do
        logInfo mgr.config.logger ("Connecting to peer " <> cs (show sa))
        nonce <- randomIO
        height <- readTVarIO mgr.best
        now <- getCurrentTime
        let net = mgr.config.net
            services = if net.segWit then 8 else 0
            rmt = NetworkAddress services (sockToHostAddress sa)
            unix = floor (utcTimeToPOSIXSeconds now)
            ver = buildVersion net nonce height mgr.config.address rmt unix
            pc =
              PeerConfig
                { pub = mgr.config.pub,
                  net = net,
                  label = cs (show sa),
                  connect = mgr.config.connect sa,
                  logger = withSource "Peer" mgr.config.logger,
                  lookupBlock = mgr.config.lookupBlock
                }
        (inbox, mailbox) <- newMailbox
        busy <- newTVarIO Free
        pending <- newTVarIO 0
        let p = wrapPeer pc busy pending mailbox
        -- The peer manager handles messages one at a time, so the peer cannot
        -- die before it is in the list.
        a <- mgr.supervisor `addChild` withHousekeeping mgr p (peer pc p inbox)
        MVersion ver `sendMessage` p
        atomically . modifyTVar' mgr.peers $ \x ->
          sort $
            OnlinePeer
              { address = sa,
                state = newPeerState now,
                async = a,
                mailbox = p,
                nonce = nonce
              }
              : x
  where
    giveBack why = do
      logDebug
        mgr.config.logger
        ("Not connecting to peer " <> cs (show sa) <> ": " <> why)
      atomically $ modifyTVar' mgr.addresses (returnAddress sa)

-- | Run a peer process along with a loop that asks the peer manager to check
-- the peer's timers at random intervals between a quarter and a half of the
-- timeout.
withHousekeeping :: PeerMgr -> Peer -> IO () -> IO ()
withHousekeeping mgr p go =
  withAsync checks $ \a -> link a >> go
  where
    micros = floor (mgr.config.timeout * 1000000) :: Int
    checks = loopForever $ do
      threadDelay =<< randomRIO (micros `div` 4, micros `div` 2)
      CheckPeer p `send` mgr.mailbox

-- | Run a loop in the background that connects to a new peer every 0.1 to 5
-- seconds while there are free slots.
withConnectLoop :: PeerMgr -> IO a -> IO a
withConnectLoop mgr act =
  withAsync go $ \a -> link a >> act
  where
    go = loopForever $ do
      n <- length <$> readTVarIO mgr.peers
      when (n < mgr.config.maxPeers) $
        getNewPeer mgr >>= mapM_ (\sa -> Connect sa `send` mgr.mailbox)
      threadDelay =<< randomRIO (100000 :: Int, 5000000)

-- | Peers that are online, sorted so that those with the lowest median ping
-- come first. Peers in the handshake or being disconnected are left out.
getPeers :: PeerMgr -> IO [OnlinePeer]
getPeers mgr = filter (isOnline . (.state)) <$> readTVarIO mgr.peers

-- | Information about a connection, or 'Nothing' if the peer manager does not
-- know it. The peer may still be in the handshake, or being disconnected;
-- check its 'state'.
getOnlinePeer :: PeerMgr -> Peer -> IO (Maybe OnlinePeer)
getOnlinePeer mgr p = find ((== p) . (.mailbox)) <$> readTVarIO mgr.peers

-- | Tell the peer manager the height of the best block.
peerMgrBest :: PeerMgr -> BlockHeight -> IO ()
peerMgrBest mgr bh = ManagerBest bh `send` mgr.mailbox

-- | Tell the peer manager that a peer acknowledged our version.
peerMgrVerAck :: PeerMgr -> Peer -> IO ()
peerMgrVerAck mgr p = PeerVerAck p `send` mgr.mailbox

-- | Tell the peer manager that a peer sent its version.
peerMgrVersion :: PeerMgr -> Peer -> Version -> IO ()
peerMgrVersion mgr p ver = PeerVersion p ver `send` mgr.mailbox

-- | Tell the peer manager that a peer sent a ping, so that it answers with a pong.
peerMgrPing :: PeerMgr -> Peer -> Word64 -> IO ()
peerMgrPing mgr p nonce = PeerPing p nonce `send` mgr.mailbox

-- | Tell the peer manager that a peer answered a ping.
peerMgrPong :: PeerMgr -> Peer -> Word64 -> IO ()
peerMgrPong mgr p nonce = PeerPong p nonce `send` mgr.mailbox

-- | Tell the peer manager about addresses relayed by a peer. They are added to
-- the known addresses if discovery is on and they are routable.
peerMgrAddrs :: PeerMgr -> Peer -> [NetworkAddress] -> IO ()
peerMgrAddrs mgr p addrs = PeerAddrs p addrs `send` mgr.mailbox

-- | Wait until the peer manager has handled every message sent to it before.
peerMgrSync :: PeerMgr -> IO ()
peerMgrSync mgr = Sync `query` mgr.mailbox

-- | Median of a list, or 'Nothing' if it is empty.
median :: (Ord a, Fractional a) => [a] -> Maybe a
median ls
  | null ls =
      Nothing
  | even (length ls) =
      Just . (/ 2) . sum . take 2 $
        drop (length ls `div` 2 - 1) ls'
  | otherwise =
      Just (ls' !! (length ls `div` 2))
  where
    ls' = sort ls

-- | Build our version message from the network, a random nonce, the best block
-- height, our address, the remote address and the current time in seconds since
-- the Unix epoch.
buildVersion ::
  Network ->
  Word64 ->
  BlockHeight ->
  NetworkAddress ->
  NetworkAddress ->
  Word64 ->
  Version
buildVersion net nonce height loc rmt time =
  -- The version, timestamp and start height of a version message are signed
  -- numbers, as in Bitcoin Core. The values the node sends are far below the
  -- point where the conversion would change them.
  Version
    { version = fromIntegral myVersion,
      services = loc.services,
      timestamp = fromIntegral time,
      addrRecv = rmt,
      addrSend = loc,
      nonce = nonce,
      userAgent = VarString net.userAgent,
      startHeight = fromIntegral height,
      relay = True
    }

-- | Protocol version that the node speaks.
myVersion :: Word32
myVersion = 70012
