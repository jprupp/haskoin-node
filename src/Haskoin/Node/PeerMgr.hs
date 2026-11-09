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
-- It maintains a pool of known peer addresses, filled from the static peers in
-- the configuration, from the DNS seeds of the network when discovery is on, and
-- from addresses relayed by connected peers. A background loop connects to new
-- peers while there are fewer than 'maxPeers' of them.
--
-- For each connection, the peer manager sends a version message, waits for the
-- peer's version and acknowledgement, and then announces the peer as connected
-- with a 'PeerConnected' event. The handshake is the state machine in
-- "Haskoin.Node.PeerMgr.Handshake": each peer's progress is a 'Handshake'
-- value, and a message that does not fit the current state disconnects the
-- peer. Peers are pinged periodically to measure their
-- latency, and the list returned by 'getPeers' is sorted by median ping time, so
-- that the fastest peers come first. A peer is disconnected if it stops
-- responding, does not answer a ping in time, or has been connected for longer
-- than 'maxPeerLife'. A 'PeerDisconnected' event is published when an online
-- peer goes away.
--
-- Most of the @peerMgr*@ functions are used by "Haskoin.Node" to feed messages
-- received from peers to the peer manager; applications rarely need to call
-- them directly.
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
-- * __Relayed addresses are untrusted.__ Peers gossip addresses, and a
--   malicious peer could gossip private or loopback addresses to make the node
--   probe its own network. Only public addresses are accepted, and only a
--   bounded number are kept, so gossip cannot exhaust memory.
module Haskoin.Node.PeerMgr
  ( PeerMgrConfig (..),
    PeerEvent (..),
    OnlinePeer (..),
    Handshake (..),
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

import Control.Arrow (first)
import Control.Monad
import Data.Function (on)
import Data.List (find, nub, sort)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversions (cs)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word64)
import GHC.Records (HasField (..))
import Haskoin
import Haskoin.Node.Address (isRoutable, toHostService, toSockAddr)
import Haskoin.Node.Logger
import Haskoin.Node.Loop (loopForever)
import Haskoin.Node.Peer
import Haskoin.Node.PeerMgr.Handshake
import NQE
import Network.Socket
import System.Random (randomIO, randomRIO)
import UnliftIO
import UnliftIO.Concurrent

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
    logger :: !Logger
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
    -- | known addresses not yet connected to, at most 'maxKnownAddresses'
    addresses :: !(TVar (Set SockAddr)),
    -- | peers being connected to or online, fastest first
    peers :: !(TVar [OnlinePeer])
  }

-- | Messages handled by the peer manager process.
data PeerMgrMessage
  = -- | connect to a peer address
    Connect !SockAddr
  | -- | check whether a peer timed out or must be pinged
    CheckPeer !Peer
  | -- | a peer process stopped
    PeerDied !Child
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

-- | Data structure representing an online peer.
--
-- Peers are compared for equality by their 'Peer' handle, and ordered by the
-- median of their recent ping times, so that sorting a list puts the fastest
-- peers first. Peers without any ping measurement sort as if their median ping
-- were 60 seconds.
--
-- The fields @version@, @online@ and @verack@ of earlier releases are now
-- computed from 'handshake', so @o.version@ and @o.online@ keep working with
-- record dot syntax. They cannot be set or pattern matched.
data OnlinePeer = OnlinePeer
  { -- | network address of the peer
    address :: !SockAddr,
    -- | progress of the version handshake
    handshake :: !Handshake,
    -- | thread running the peer process
    async :: !(Async ()),
    -- | handle to talk to the peer
    mailbox :: !Peer,
    -- | nonce of the version message we sent, used to detect connections to
    -- ourselves
    nonce :: !Word64,
    -- | time and nonce of the ping awaiting an answer, if any
    ping :: !(Maybe (UTCTime, Word64)),
    -- | round-trip times of the last eleven pings, sorted
    pings :: ![NominalDiffTime],
    -- | when the connection was made
    connected :: !UTCTime,
    -- | when the peer last sent a message
    tickled :: !UTCTime
  }

instance Eq OnlinePeer where
  (==) = (==) `on` f
    where
      f OnlinePeer {mailbox = p} = p

-- | Version message sent by the peer, if received.
instance HasField "version" OnlinePeer (Maybe Version) where
  getField o = handshakeVersion o.handshake

-- | Whether the handshake is complete: both version and verack received.
instance HasField "online" OnlinePeer Bool where
  getField o = isEstablished o.handshake

-- | Whether the peer acknowledged our version.
instance HasField "verack" OnlinePeer Bool where
  getField o = hasVerAck o.handshake

instance Ord OnlinePeer where
  compare = compare `on` f
    where
      f OnlinePeer {pings = pings} = fromMaybe 60 (median pings)

-- | Start a peer manager and pass it to the given action. The peer manager and
-- all its peers are stopped when the action returns.
--
-- The peer manager does not connect to any peer until it learns the height of
-- the best block through 'peerMgrBest'. "Haskoin.Node" does this as soon as the
-- chain is loaded.
withPeerMgr ::
  (MonadUnliftIO m) =>
  PeerMgrConfig ->
  (PeerMgr -> m a) ->
  m a
withPeerMgr cfg action = do
  ibx <- newInbox
  withSupervisor (Notify (death ibx)) $ \sup -> do
    bb <- newTVarIO 0
    kp <- newTVarIO Set.empty
    ob <- newTVarIO []
    let mgr =
          PeerMgr
            { config = cfg,
              supervisor = sup,
              mailbox = inboxToMailbox ibx,
              best = bb,
              addresses = kp,
              peers = ob
            }
    go mgr ibx
  where
    death ibx (a, _e) = PeerDied a `sendSTM` ibx
    go mgr ibx = withAsync (peerManager mgr ibx) $ \a ->
      withConnectLoop mgr (link a >> action mgr)

-- | Main loop of the peer manager process. Waits for the best block height, then
-- handles messages forever.
peerManager ::
  (MonadUnliftIO m) => PeerMgr -> Inbox PeerMgrMessage -> m ()
peerManager mgr ibx = do
  logDebug mgr.config.logger "Getting best block..."
  putBestBlock mgr <=< receiveMatch ibx $ \case
    ManagerBest b -> Just b
    _ -> Nothing
  loopForever $ do
    logDebug mgr.config.logger "Awaiting event..."
    dispatch mgr =<< receive ibx

-- | Store the height of the best block.
putBestBlock :: (MonadIO m) => PeerMgr -> BlockHeight -> m ()
putBestBlock mgr bb = atomically $ writeTVar mgr.best bb

-- | Read the height of the best block.
getBestBlock :: (MonadIO m) => PeerMgr -> m BlockHeight
getBestBlock mgr = readTVarIO mgr.best

-- | Add the static peers and, if discovery is on, the network seeds to the known addresses.
loadPeers :: (MonadIO m) => PeerMgr -> m ()
loadPeers mgr = do
  loadStaticPeers mgr
  loadNetSeeds mgr

-- | Resolve the static peers in the configuration and add them to the known addresses.
loadStaticPeers :: (MonadIO m) => PeerMgr -> m ()
loadStaticPeers mgr =
  mapM_ (newPeer mgr) . concat
    =<< mapM (liftIO . toSockAddr mgr.config.net) mgr.config.peers

-- | If discovery is on, resolve the DNS seeds of the network and add them to the known addresses.
loadNetSeeds :: (MonadIO m) => PeerMgr -> m ()
loadNetSeeds mgr =
  when mgr.config.discover $ do
    ss <-
      concat
        <$> mapM (liftIO . toSockAddr mgr.config.net) mgr.config.net.seeds
    mapM_ (newPeer mgr) ss

-- | Log how many peers are online out of the maximum.
logConnectedPeers :: (MonadIO m) => PeerMgr -> m ()
logConnectedPeers mgr = do
  let m = mgr.config.maxPeers
  l <- length <$> getConnectedPeers mgr
  logInfo
    mgr.config.logger
    ("Peers connected: " <> cs (show l) <> "/" <> cs (show m))

-- | All peers being connected to or online, fastest first.
getOnlinePeers :: (MonadIO m) => PeerMgr -> m [OnlinePeer]
getOnlinePeers mgr = readTVarIO mgr.peers

-- | Peers that completed the handshake, fastest first.
getConnectedPeers :: (MonadIO m) => PeerMgr -> m [OnlinePeer]
getConnectedPeers mgr = filter (isEstablished . (.handshake)) <$> getOnlinePeers mgr

-- | Publish a peer event.
managerEvent :: (MonadIO m) => PeerMgr -> PeerEvent -> m ()
managerEvent mgr e = publish e mgr.config.pub

-- | Handle a message sent to the peer manager.
dispatch ::
  (MonadUnliftIO m) => PeerMgr -> PeerMgrMessage -> m ()
dispatch mgr (PeerVersion p v) = do
  logDebug
    mgr.config.logger
    ("Received peer " <> p.label <> " version " <> cs (show v))
  handshakeEvent mgr p (GotVersion v)
dispatch mgr (PeerVerAck p) = do
  logDebug mgr.config.logger ("Received version ack from peer " <> p.label)
  handshakeEvent mgr p GotVerAck
dispatch mgr (PeerAddrs p nas)
  | mgr.config.discover =
      getOnlinePeer mgr p >>= \case
        Just o | isEstablished o.handshake -> do
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
          forM_ sas (newPeer mgr)
        _ ->
          logDebug
            mgr.config.logger
            ("Ignoring addresses from peer before handshake " <> p.label)
  | otherwise =
      logDebug mgr.config.logger ("Ignoring addresses from peer " <> p.label)
dispatch _ (Sync r) =
  atomically (r ())
dispatch mgr (PeerPong p n) = do
  logDebug
    mgr.config.logger
    ("Received pong " <> cs (show n) <> " from peer " <> p.label)
  now <- liftIO getCurrentTime
  atomically (gotPong mgr.peers n now p)
dispatch mgr (PeerPing p n) = do
  logDebug
    mgr.config.logger
    ("Responding to ping " <> cs (show n) <> " from peer " <> p.label)
  MPong (Pong n) `sendMessage` p
dispatch mgr (ManagerBest h) = do
  logDebug mgr.config.logger ("Setting best block to " <> cs (show h))
  putBestBlock mgr h
dispatch mgr (Connect sa) = do
  connectPeer mgr sa
dispatch mgr (PeerDied a) = do
  processPeerOffline mgr a
dispatch mgr (CheckPeer p) = do
  logDebug mgr.config.logger ("Housekeeping for peer " <> p.label)
  checkPeer mgr p

-- | Advance the handshake of a peer with a message it sent, and carry out the
-- resulting actions. A message that does not fit the state of the handshake,
-- or a version that 'checkVersion' rejects, disconnects the peer.
handshakeEvent :: (MonadIO m) => PeerMgr -> Peer -> HandshakeEvent -> m ()
handshakeEvent mgr p event = do
  result <- atomically $ do
    ops <- readTVar mgr.peers
    case find ((== p) . (.mailbox)) ops of
      Nothing -> return Nothing
      Just o -> do
        let valid = case event of
              GotVersion v -> void (checkVersion (map (.nonce) ops) v)
              GotVerAck -> Right ()
            step = valid >> stepHandshake o.handshake event
        forM_ step $ \(h, _) -> insertPeer mgr.peers o {handshake = h}
        return (Just (snd <$> step))
  case result of
    Nothing -> do
      logWarn
        mgr.config.logger
        ("Handshake message from unknown peer " <> p.label)
      killPeer p
    Just (Left e) -> do
      logWarn
        mgr.config.logger
        ("Rejecting peer " <> p.label <> ": " <> cs (show e))
      killPeer p
    Just (Right actions) -> forM_ actions $ \case
      SendVerAck -> do
        logDebug mgr.config.logger ("Sending version ack to peer " <> p.label)
        MVerAck `sendMessage` p
      AnnounceConnected -> announcePeer mgr p

-- | Record that a peer just sent a message, so that it is not disconnected for
-- being idle. "Haskoin.Node" calls this for every message received.
ticklePeer :: (MonadIO m) => PeerMgr -> Peer -> m ()
ticklePeer mgr p = do
  logDebug mgr.config.logger ("Tickling peer " <> p.label)
  t <- liftIO getCurrentTime
  atomically (modifyPeer mgr.peers p (\o -> o {tickled = t}))

-- | Disconnect a peer that has been connected for too long, has been idle for
-- too long, or did not answer a ping in time. Otherwise ping it if no ping is
-- pending.
checkPeer :: (MonadIO m) => PeerMgr -> Peer -> m ()
checkPeer mgr p =
  atomically (mgr.peers `findPeer` p) >>= \case
    Just o -> do
      now <- liftIO getCurrentTime
      let expired = mgr.config.maxPeerLife `addUTCTime` o.connected
          deadline = mgr.config.timeout `addUTCTime` o.tickled
          -- Other traffic must not keep a peer that ignores our ping.
          pingDeadline = (\(t, _) -> mgr.config.timeout `addUTCTime` t) <$> o.ping
      if
        | now > expired -> do
            logError mgr.config.logger ("Peer " <> p.label <> " is too old")
            killPeer p
        | now > deadline -> do
            logError mgr.config.logger ("Peer " <> p.label <> " timed out")
            killPeer p
        | maybe False (now >) pingDeadline -> do
            logError mgr.config.logger ("Peer " <> p.label <> " did not answer ping")
            killPeer p
        | isNothing o.ping -> sendPing mgr p
        | otherwise -> return ()
    Nothing -> do
      logError mgr.config.logger ("Peer " <> p.label <> " is offline")

-- | Send a ping with a random nonce to an online peer and remember when it was sent.
sendPing :: (MonadIO m) => PeerMgr -> Peer -> m ()
sendPing mgr p = do
  atomically (mgr.peers `findPeer` p) >>= \case
    Nothing ->
      logWarn mgr.config.logger ("Will not ping unknown peer " <> p.label)
    Just o
      | isEstablished o.handshake -> do
          n <- randomIO
          now <- liftIO getCurrentTime
          atomically (setPeerPing mgr.peers n now p)
          logDebug
            mgr.config.logger
            ("Sending ping " <> cs (show n) <> " to peer " <> p.label)
          MPing (Ping n) `sendMessage` p
      | otherwise ->
          logDebug
            mgr.config.logger
            ("Will not ping offline peer " <> p.label)

-- | Clean up after a peer process stops, and publish a disconnection event if the peer was online.
processPeerOffline :: (MonadIO m) => PeerMgr -> Child -> m ()
processPeerOffline mgr a = do
  atomically (findPeerAsync mgr.peers a) >>= \case
    Nothing -> logError mgr.config.logger "Disconnected unknown peer"
    Just o -> do
      if isEstablished o.handshake
        then do
          logError
            mgr.config.logger
            ("Disconnected peer " <> o.mailbox.label)
          managerEvent mgr (PeerDisconnected o.mailbox)
        else
          logError
            mgr.config.logger
            ("Could not connect to peer " <> o.mailbox.label)
      atomically (removePeer mgr.peers o.mailbox)
      logConnectedPeers mgr

-- | Publish a connection event for a peer that just completed the handshake.
announcePeer :: (MonadIO m) => PeerMgr -> Peer -> m ()
announcePeer mgr p = do
  logInfo mgr.config.logger ("Connected to peer " <> p.label)
  managerEvent mgr (PeerConnected p)
  logConnectedPeers mgr

-- | Take an address from the known addresses. If there are none, reload the static peers and seeds first.
getNewPeer :: (MonadIO m) => PeerMgr -> m (Maybe SockAddr)
getNewPeer mgr =
  g >>= \case
    Nothing -> loadPeers mgr >> g
    Just a -> return (Just a)
  where
    g =
      atomically $
        stateTVar
          mgr.addresses
          (first (listToMaybe . Set.elems) . Set.splitAt 1)

-- | Start a peer process for an address and send it our version, unless there
-- are no free slots or the address is already connected.
connectPeer :: (MonadUnliftIO m) => PeerMgr -> SockAddr -> m ()
connectPeer mgr sa = do
  n <- length <$> getOnlinePeers mgr
  atomically (findPeerAddress mgr.peers sa) >>= \case
    _
      | n >= mgr.config.maxPeers ->
          -- The connect loop may have queued this before the last free slot
          -- was taken.
          logDebug
            mgr.config.logger
            ("Not connecting to peer " <> cs (show sa) <> ": no free slots")
    Just _ ->
      logWarn mgr.config.logger ("Duplicate connection to peer " <> cs (show sa))
    Nothing -> do
      logInfo mgr.config.logger ("Connecting to peer " <> cs (show sa))
      nonce <- randomIO
      bb <- getBestBlock mgr
      now <- liftIO getCurrentTime
      let rmt = NetworkAddress (srv mgr.config.net) (sockToHostAddress sa)
          unix = floor (utcTimeToPOSIXSeconds now)
          ver = buildVersion mgr.config.net nonce bb mgr.config.address rmt unix
          text = cs (show sa)
      (inbox, mailbox) <- newMailbox
      let pc =
            PeerConfig
              { pub = mgr.config.pub,
                net = mgr.config.net,
                label = text,
                connect = mgr.config.connect sa,
                logger = withSource "Peer" mgr.config.logger
              }
      busy <- newTVarIO False
      pending <- newTVarIO 0
      let p = wrapPeer pc busy pending mailbox
      a <- withRunInIO $ \io ->
        mgr.supervisor `addChild` io (launch pc inbox p)
      MVersion ver `sendMessage` p
      atomically $
        insertPeer
          mgr.peers
          OnlinePeer
            { address = sa,
              handshake = AwaitingBoth,
              async = a,
              mailbox = p,
              nonce = nonce,
              pings = [],
              ping = Nothing,
              connected = now,
              tickled = now
            }
  where
    srv net
      | net.segWit = 8
      | otherwise = 0
    launch pc inbox p =
      withPeerLoop mgr p (\a -> link a >> peer pc p inbox)

-- | Run a loop in the background that asks the peer manager to check a peer at
-- random intervals between a quarter and a half of the timeout.
withPeerLoop :: (MonadUnliftIO m) => PeerMgr -> Peer -> (Async a -> m a) -> m a
withPeerLoop mgr p =
  withAsync . loopForever $ do
    let ms = floor (mgr.config.timeout * 1000 * 1000)
    r <- randomRIO (ms `div` 4, ms `div` 2)
    threadDelay r
    managerCheck mgr p

-- | Run a loop in the background that connects to a new peer every 0.1 to 5
-- seconds while there are free slots.
withConnectLoop :: (MonadUnliftIO m) => PeerMgr -> m a -> m a
withConnectLoop mgr act =
  withAsync go $ \a -> link a >> act
  where
    go = loopForever $ do
      l <- length <$> getOnlinePeers mgr
      when
        (l < mgr.config.maxPeers)
        (getNewPeer mgr >>= mapM_ (managerConnect mgr))
      threadDelay =<< randomRIO (100000 :: Int, 5000000)

-- | Upper bound on addresses remembered for future connections. Peers keep
-- relaying addresses for as long as the node runs, and with every connection
-- slot in use nothing ever removes them, so the pool must be bounded.
maxKnownAddresses :: Int
maxKnownAddresses = 4096

-- | Add an address to the known addresses, unless it is connected already or the pool is full.
newPeer :: (MonadIO m) => PeerMgr -> SockAddr -> m ()
newPeer mgr sa =
  atomically $
    findPeerAddress mgr.peers sa >>= \case
      Just _ -> return ()
      Nothing -> modifyTVar' mgr.addresses $ \known ->
        if Set.size known >= maxKnownAddresses
          then known
          else Set.insert sa known

-- | Record the round-trip time of a ping if the nonce matches the pending one.
gotPong :: TVar [OnlinePeer] -> Word64 -> UTCTime -> Peer -> STM ()
gotPong b nonce now p =
  findPeer b p >>= \case
    Just o@OnlinePeer {ping = Just (time, nonce')} | nonce' == nonce -> do
      let d = now `diffUTCTime` time
          o' = o {ping = Nothing, pings = sort (take 11 (d : o.pings))}
      insertPeer b o'
    _ -> return ()

-- | Remember the time and nonce of a ping sent to a peer.
setPeerPing :: TVar [OnlinePeer] -> Word64 -> UTCTime -> Peer -> STM ()
setPeerPing b nonce now p =
  modifyPeer b p $ \o -> o {ping = Just (now, nonce)}

-- | Find a peer in the list.
findPeer :: TVar [OnlinePeer] -> Peer -> STM (Maybe OnlinePeer)
findPeer b p =
  find ((== p) . (.mailbox))
    <$> readTVar b

-- | Add or replace a peer in the list, keeping it sorted.
insertPeer :: TVar [OnlinePeer] -> OnlinePeer -> STM ()
insertPeer b o =
  modifyTVar b $ \x -> sort . nub $ o : x

-- | Change a peer in the list, if it is there.
modifyPeer :: TVar [OnlinePeer] -> Peer -> (OnlinePeer -> OnlinePeer) -> STM ()
modifyPeer b p f =
  findPeer b p >>= \case
    Nothing -> return ()
    Just o -> insertPeer b $ f o

-- | Remove a peer from the list.
removePeer :: TVar [OnlinePeer] -> Peer -> STM ()
removePeer b p = modifyTVar b (filter ((/= p) . (.mailbox)))

-- | Find a peer by the thread that runs it.
findPeerAsync :: TVar [OnlinePeer] -> Async () -> STM (Maybe OnlinePeer)
findPeerAsync b a = find ((== a) . (.async)) <$> readTVar b

-- | Find a peer by its address.
findPeerAddress :: TVar [OnlinePeer] -> SockAddr -> STM (Maybe OnlinePeer)
findPeerAddress b a = find ((== a) . (.address)) <$> readTVar b

-- | Peers that completed the handshake, sorted so that those with the lowest
-- median ping come first.
getPeers :: (MonadIO m) => PeerMgr -> m [OnlinePeer]
getPeers = getConnectedPeers

-- | Information about a peer, or 'Nothing' if the peer manager does not know it.
-- The peer may still be in the middle of the handshake; check its
-- 'handshake'.
getOnlinePeer :: (MonadIO m) => PeerMgr -> Peer -> m (Maybe OnlinePeer)
getOnlinePeer mgr p = atomically (mgr.peers `findPeer` p)

-- | Ask the peer manager to check a peer.
managerCheck :: (MonadIO m) => PeerMgr -> Peer -> m ()
managerCheck mgr p = CheckPeer p `send` mgr.mailbox

-- | Ask the peer manager to connect to an address.
managerConnect :: (MonadIO m) => PeerMgr -> SockAddr -> m ()
managerConnect mgr sa = Connect sa `send` mgr.mailbox

-- | Tell the peer manager the height of the best block.
peerMgrBest :: (MonadIO m) => PeerMgr -> BlockHeight -> m ()
peerMgrBest mgr bh = ManagerBest bh `send` mgr.mailbox

-- | Tell the peer manager that a peer acknowledged our version.
peerMgrVerAck :: (MonadIO m) => PeerMgr -> Peer -> m ()
peerMgrVerAck mgr p = PeerVerAck p `send` mgr.mailbox

-- | Tell the peer manager that a peer sent its version.
peerMgrVersion :: (MonadIO m) => PeerMgr -> Peer -> Version -> m ()
peerMgrVersion mgr p ver = PeerVersion p ver `send` mgr.mailbox

-- | Tell the peer manager that a peer sent a ping, so that it answers with a pong.
peerMgrPing :: (MonadIO m) => PeerMgr -> Peer -> Word64 -> m ()
peerMgrPing mgr p nonce = PeerPing p nonce `send` mgr.mailbox

-- | Tell the peer manager that a peer answered a ping.
peerMgrPong :: (MonadIO m) => PeerMgr -> Peer -> Word64 -> m ()
peerMgrPong mgr p nonce = PeerPong p nonce `send` mgr.mailbox

-- | Tell the peer manager about addresses relayed by a peer. They are added to
-- the known addresses if discovery is on and they are routable.
peerMgrAddrs :: (MonadIO m) => PeerMgr -> Peer -> [NetworkAddress] -> m ()
peerMgrAddrs mgr p addrs = PeerAddrs p addrs `send` mgr.mailbox

-- | Wait until the peer manager has handled every message sent to it before.
peerMgrSync :: (MonadIO m) => PeerMgr -> m ()
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
