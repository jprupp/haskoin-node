{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Haskoin.Node.PeerMgr
  ( PeerMgrConfig (..),
    PeerEvent (..),
    OnlinePeer (..),
    PeerMgr,
    withPeerMgr,
    peerMgrBest,
    peerMgrVersion,
    peerMgrPing,
    peerMgrPong,
    peerMgrAddrs,
    peerMgrVerAck,
    getPeers,
    getOnlinePeer,
    ticklePeer,
    buildVersion,
    myVersion,
    toSockAddr,
    toHostService,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow
import Control.Monad
import Control.Monad.Logger
import Data.Bits ((.&.))
import Data.Function (on)
import Data.List (dropWhileEnd, elemIndex, find, nub, sort)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversions (cs)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word64)
import Haskoin
import Haskoin.Node.Peer
import NQE
import Network.Socket
import System.Random (randomIO, randomRIO)
import UnliftIO
import UnliftIO.Concurrent

data PeerMgrConfig = PeerMgrConfig
  { maxPeers :: !Int,
    peers :: ![String],
    discover :: !Bool,
    address :: !NetworkAddress,
    net :: !Network,
    pub :: !(Publisher PeerEvent),
    timeout :: !NominalDiffTime,
    maxPeerLife :: !NominalDiffTime,
    connect :: !(SockAddr -> WithConnection)
  }

data PeerMgr = PeerMgr
  { config :: !PeerMgrConfig,
    supervisor :: !Supervisor,
    mailbox :: !(Mailbox PeerMgrMessage),
    best :: !(TVar BlockHeight),
    addresses :: !(TVar (Set SockAddr)),
    peers :: !(TVar [OnlinePeer])
  }

data PeerMgrMessage
  = Connect !SockAddr
  | CheckPeer !Peer
  | PeerDied !Child
  | ManagerBest !BlockHeight
  | PeerVerAck !Peer
  | PeerVersion !Peer !Version
  | PeerPing !Peer !Word64
  | PeerPong !Peer !Word64
  | PeerAddrs !Peer ![NetworkAddress]

-- | Data structure representing an online peer.
data OnlinePeer = OnlinePeer
  { address :: !SockAddr,
    verack :: !Bool,
    online :: !Bool,
    version :: !(Maybe Version),
    async :: !(Async ()),
    mailbox :: !Peer,
    nonce :: !Word64,
    ping :: !(Maybe (UTCTime, Word64)),
    pings :: ![NominalDiffTime],
    connected :: !UTCTime,
    tickled :: !UTCTime
  }

instance Eq OnlinePeer where
  (==) = (==) `on` f
    where
      f OnlinePeer {mailbox = p} = p

instance Ord OnlinePeer where
  compare = compare `on` f
    where
      f OnlinePeer {pings = pings} = fromMaybe 60 (median pings)

withPeerMgr ::
  (MonadLoggerIO m, MonadUnliftIO m) =>
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

peerManager ::
  (MonadLoggerIO m, MonadUnliftIO m) => PeerMgr -> Inbox PeerMgrMessage -> m ()
peerManager mgr ibx = do
  $(logDebugS) "PeerMgr" "Getting best block..."
  putBestBlock mgr <=< receiveMatch ibx $ \case
    ManagerBest b -> Just b
    _ -> Nothing
  forever $ do
    $(logDebugS) "PeerMgr" "Awaiting event..."
    dispatch mgr =<< receive ibx

putBestBlock :: (MonadIO m) => PeerMgr -> BlockHeight -> m ()
putBestBlock mgr bb = atomically $ writeTVar mgr.best bb

getBestBlock :: (MonadIO m) => PeerMgr -> m BlockHeight
getBestBlock mgr = readTVarIO mgr.best

getNetwork :: PeerMgr -> Network
getNetwork mgr = mgr.config.net

loadPeers :: (MonadIO m) => PeerMgr -> m ()
loadPeers mgr = do
  loadStaticPeers mgr
  loadNetSeeds mgr

loadStaticPeers :: (MonadIO m) => PeerMgr -> m ()
loadStaticPeers mgr =
  mapM_ (newPeer mgr) . concat
    =<< mapM (liftIO . toSockAddr mgr.config.net) mgr.config.peers

loadNetSeeds :: (MonadIO m) => PeerMgr -> m ()
loadNetSeeds mgr =
  when mgr.config.discover $ do
    ss <-
      concat
        <$> mapM (liftIO . toSockAddr mgr.config.net) mgr.config.net.seeds
    mapM_ (newPeer mgr) ss

logConnectedPeers :: (MonadLoggerIO m) => PeerMgr -> m ()
logConnectedPeers mgr = do
  let m = mgr.config.maxPeers
  l <- length <$> getConnectedPeers mgr
  $(logInfoS)
    "PeerMgr"
    ("Peers connected: " <> cs (show l) <> "/" <> cs (show m))

getOnlinePeers :: (MonadIO m) => PeerMgr -> m [OnlinePeer]
getOnlinePeers mgr = readTVarIO mgr.peers

getConnectedPeers :: (MonadIO m) => PeerMgr -> m [OnlinePeer]
getConnectedPeers mgr = filter (.online) <$> getOnlinePeers mgr

managerEvent :: (MonadIO m) => PeerMgr -> PeerEvent -> m ()
managerEvent mgr e = publish e mgr.config.pub

dispatch ::
  (MonadLoggerIO m, MonadUnliftIO m) => PeerMgr -> PeerMgrMessage -> m ()
dispatch mgr (PeerVersion p v) = do
  $(logDebugS)
    "PeerMgr"
    ("Received peer " <> p.label <> " version " <> cs (show v))
  let b = mgr.peers
  atomically (setPeerVersion b p v) >>= \case
    Just o -> do
      when o.online (announcePeer mgr p)
      $(logDebugS)
        "PeerMgr"
        ("Sending version ack to peer " <> p.label)
      MVerAck `sendMessage` p
    Nothing -> do
      $(logWarnS)
        "PeerMgr"
        ("Version rejected for peer " <> p.label <> ": " <> cs (show v))
      killPeer p
dispatch mgr (PeerVerAck p) = do
  atomically (setPeerVerAck mgr.peers p) >>= \case
    Just o -> do
      $(logDebugS) "PeerMgr" ("Received version ack from peer " <> p.label)
      when o.online (announcePeer mgr p)
    Nothing -> do
      $(logWarnS)
        "PeerMgr"
        ("Received verack from unknown peer " <> p.label)
      killPeer p
dispatch mgr (PeerAddrs p nas)
  | mgr.config.discover = do
      let sas = map (hostToSockAddr . (.address)) nas
          len = length sas
      $(logDebugS)
        "PeerMgr"
        ("Received " <> cs (show len) <> " addresses from peer " <> p.label)
      forM_ sas (newPeer mgr)
  | otherwise =
      $(logDebugS) "PeerMgr" ("Ignoring addresses from peer " <> p.label)
dispatch mgr (PeerPong p n) = do
  $(logDebugS)
    "PeerMgr"
    ("Received pong " <> cs (show n) <> " from peer " <> p.label)
  now <- liftIO getCurrentTime
  atomically (gotPong mgr.peers n now p)
dispatch _mgr (PeerPing p n) = do
  $(logDebugS)
    "PeerMgr"
    ("Responding to ping " <> cs (show n) <> " from peer " <> p.label)
  MPong (Pong n) `sendMessage` p
dispatch mgr (ManagerBest h) = do
  $(logDebugS) "PeerMgr" ("Setting best block to " <> cs (show h))
  putBestBlock mgr h
dispatch mgr (Connect sa) = do
  connectPeer mgr sa
dispatch mgr (PeerDied a) = do
  processPeerOffline mgr a
dispatch mgr (CheckPeer p) = do
  $(logDebugS) "PeerMgr" ("Housekeeping for peer " <> p.label)
  checkPeer mgr p

ticklePeer :: (MonadLoggerIO m) => PeerMgr -> Peer -> m ()
ticklePeer m p = do
  $(logDebugS) "PeerMgr" ("Tickling peer " <> p.label)
  t <- liftIO getCurrentTime
  atomically (modifyPeer m.peers p (\o -> o {tickled = t}))

checkPeer :: (MonadLoggerIO m) => PeerMgr -> Peer -> m ()
checkPeer mgr p =
  atomically (mgr.peers `findPeer` p) >>= \case
    Just o -> do
      now <- liftIO getCurrentTime
      let expired = mgr.config.maxPeerLife `addUTCTime` o.connected
      let deadline = mgr.config.timeout `addUTCTime` o.tickled
      if
        | now > expired -> do
            $(logWarnS) "PeerMgr" ("Peer " <> p.label <> " is too old")
            killPeer p
        | now > deadline -> do
            $(logWarnS) "PeerMgr" ("Peer " <> p.label <> " timed out")
            killPeer p
        | isNothing o.ping -> sendPing mgr p
        | otherwise -> return ()
    _ -> return ()

sendPing :: (MonadLoggerIO m) => PeerMgr -> Peer -> m ()
sendPing mgr p = do
  atomically (mgr.peers `findPeer` p) >>= \case
    Nothing ->
      $(logWarnS) "PeerMgr" ("Will not ping unknown peer " <> p.label)
    Just o
      | o.online -> do
          n <- randomIO
          now <- liftIO getCurrentTime
          atomically (setPeerPing mgr.peers n now p)
          $(logDebugS)
            "PeerMgr"
            ("Sending ping " <> cs (show n) <> " to peer " <> p.label)
          MPing (Ping n) `sendMessage` p
      | otherwise ->
          $(logDebugS)
            "PeerMgr"
            ("Will not ping offline peer " <> p.label)

processPeerOffline :: (MonadLoggerIO m) => PeerMgr -> Child -> m ()
processPeerOffline mgr a = do
  atomically (findPeerAsync mgr.peers a) >>= \case
    Nothing -> $(logWarnS) "PeerMgr" "Disconnected unknown peer"
    Just o -> do
      if o.online
        then do
          $(logWarnS)
            "PeerMgr"
            ("Disconnected peer " <> o.mailbox.label)
          managerEvent mgr (PeerDisconnected o.mailbox)
        else
          $(logWarnS)
            "PeerMgr"
            ("Could not connect to peer " <> o.mailbox.label)
      atomically (removePeer mgr.peers o.mailbox)
      logConnectedPeers mgr

announcePeer :: (MonadLoggerIO m) => PeerMgr -> Peer -> m ()
announcePeer mgr p = do
  atomically (findPeer mgr.peers p) >>= \case
    Just OnlinePeer {online = True} -> do
      $(logInfoS) "PeerMgr" ("Connected to peer " <> p.label)
      managerEvent mgr (PeerConnected p)
      logConnectedPeers mgr
    Just OnlinePeer {online = False} ->
      return ()
    Nothing ->
      $(logWarnS) "PeerMgr" ("Not announcing disconnected peer " <> p.label)

getNewPeer :: (MonadIO m) => PeerMgr -> m (Maybe SockAddr)
getNewPeer mgr = do
  loadPeers mgr
  atomically . stateTVar mgr.addresses $
    first (listToMaybe . Set.elems) . Set.splitAt 1

connectPeer :: (MonadUnliftIO m, MonadLoggerIO m) => PeerMgr -> SockAddr -> m ()
connectPeer mgr sa = do
  atomically (findPeerAddress mgr.peers sa) >>= \case
    Just _ ->
      $(logWarnS) "PeerMgr" ("Duplicate connection to peer " <> cs (show sa))
    Nothing -> do
      $(logWarnS) "PeerMgr" ("Connecting to peer " <> cs (show sa))
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
                connect = mgr.config.connect sa
              }
      busy <- newTVarIO False
      let p = wrapPeer pc busy mailbox
      a <- withRunInIO $ \io ->
        mgr.supervisor `addChild` io (launch pc busy inbox p)
      MVersion ver `sendMessage` p
      atomically $
        insertPeer
          mgr.peers
          OnlinePeer
            { address = sa,
              verack = False,
              online = False,
              version = Nothing,
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
    launch pc busy inbox p =
      withPeerLoop mgr p (\a -> link a >> peer pc busy inbox)

withPeerLoop :: (MonadUnliftIO m) => PeerMgr -> Peer -> (Async a -> m a) -> m a
withPeerLoop mgr p =
  withAsync . forever $ do
    let timeout = mgr.config.timeout
        ms = floor (timeout * 1000 * 1000)
    r <- randomRIO (ms `div` 4, ms `div` 2)
    threadDelay r
    managerCheck mgr p

withConnectLoop :: (MonadUnliftIO m) => PeerMgr -> m a -> m a
withConnectLoop mgr act =
  withAsync go $ \a -> link a >> act
  where
    go = forever $ do
      l <- length <$> getOnlinePeers mgr
      when
        (l < mgr.config.maxPeers)
        (getNewPeer mgr >>= mapM_ (managerConnect mgr))
      threadDelay =<< randomRIO (10 ^ 5, 5 * 10 ^ 6)

newPeer :: (MonadIO m) => PeerMgr -> SockAddr -> m ()
newPeer mgr sa =
  atomically $
    findPeerAddress mgr.peers sa >>= \case
      Just _ -> return ()
      Nothing -> modifyTVar mgr.addresses $ Set.insert sa

gotPong :: TVar [OnlinePeer] -> Word64 -> UTCTime -> Peer -> STM ()
gotPong b nonce now p =
  findPeer b p >>= \case
    Just o@OnlinePeer {ping = Just (time, nonce')} | nonce' == nonce -> do
      let d = now `diffUTCTime` time
          o' = o {ping = Nothing, pings = sort (take 11 (d : o.pings))}
      insertPeer b o'
    _ -> return ()

setPeerPing :: TVar [OnlinePeer] -> Word64 -> UTCTime -> Peer -> STM ()
setPeerPing b nonce now p =
  modifyPeer b p $ \o -> o {ping = Just (now, nonce)}

setPeerVersion :: TVar [OnlinePeer] -> Peer -> Version -> STM (Maybe OnlinePeer)
setPeerVersion b p v
  | v.services .&. nodeNetwork == 0 = return Nothing
  | otherwise =
      readTVar b >>= \ops ->
        if any (\o -> v.nonce == o.nonce) ops
          then return Nothing -- peer is myself
          else
            findPeer b p >>= \case
              Nothing -> return Nothing -- peer not found
              Just o -> do
                let n = o {version = Just v, online = o.verack}
                insertPeer b n
                return (Just n)

setPeerVerAck :: TVar [OnlinePeer] -> Peer -> STM (Maybe OnlinePeer)
setPeerVerAck b p =
  findPeer b p >>= \case
    Just o -> do
      let o' = o {verack = True, online = isJust o.version}
      insertPeer b o'
      return (Just o')
    Nothing -> return Nothing

findPeer :: TVar [OnlinePeer] -> Peer -> STM (Maybe OnlinePeer)
findPeer b p =
  find ((== p) . (.mailbox))
    <$> readTVar b

insertPeer :: TVar [OnlinePeer] -> OnlinePeer -> STM ()
insertPeer b o =
  modifyTVar b $ \x -> sort . nub $ o : x

modifyPeer :: TVar [OnlinePeer] -> Peer -> (OnlinePeer -> OnlinePeer) -> STM ()
modifyPeer b p f =
  findPeer b p >>= \case
    Nothing -> return ()
    Just o -> insertPeer b $ f o

removePeer :: TVar [OnlinePeer] -> Peer -> STM ()
removePeer b p = modifyTVar b (filter ((/= p) . (.mailbox)))

findPeerAsync :: TVar [OnlinePeer] -> Async () -> STM (Maybe OnlinePeer)
findPeerAsync b a = find ((== a) . (.async)) <$> readTVar b

findPeerAddress :: TVar [OnlinePeer] -> SockAddr -> STM (Maybe OnlinePeer)
findPeerAddress b a = find ((== a) . (.address)) <$> readTVar b

getPeers :: (MonadIO m) => PeerMgr -> m [OnlinePeer]
getPeers = getConnectedPeers

getOnlinePeer :: (MonadIO m) => PeerMgr -> Peer -> m (Maybe OnlinePeer)
getOnlinePeer mgr p = atomically (mgr.peers `findPeer` p)

managerCheck :: (MonadIO m) => PeerMgr -> Peer -> m ()
managerCheck mgr p = CheckPeer p `send` mgr.mailbox

managerConnect :: (MonadIO m) => PeerMgr -> SockAddr -> m ()
managerConnect mgr sa = Connect sa `send` mgr.mailbox

peerMgrBest :: (MonadIO m) => PeerMgr -> BlockHeight -> m ()
peerMgrBest mgr bh = ManagerBest bh `send` mgr.mailbox

peerMgrVerAck :: (MonadIO m) => PeerMgr -> Peer -> m ()
peerMgrVerAck mgr p = PeerVerAck p `send` mgr.mailbox

peerMgrVersion :: (MonadIO m) => PeerMgr -> Peer -> Version -> m ()
peerMgrVersion mgr p ver = PeerVersion p ver `send` mgr.mailbox

peerMgrPing :: (MonadIO m) => PeerMgr -> Peer -> Word64 -> m ()
peerMgrPing mgr p nonce = PeerPing p nonce `send` mgr.mailbox

peerMgrPong :: (MonadIO m) => PeerMgr -> Peer -> Word64 -> m ()
peerMgrPong mgr p nonce = PeerPong p nonce `send` mgr.mailbox

peerMgrAddrs :: (MonadIO m) => PeerMgr -> Peer -> [NetworkAddress] -> m ()
peerMgrAddrs mgr p addrs = PeerAddrs p addrs `send` mgr.mailbox

toHostService :: String -> (Maybe String, Maybe String)
toHostService str =
  let host = case m6 of
        Just (x, _) -> Just x
        Nothing -> case takeWhile (/= ':') str of
          [] -> Nothing
          xs -> Just xs
      srv = case m6 of
        Just (_, y) -> s y
        Nothing -> s str
      s xs =
        case dropWhile (/= ':') xs of
          [] -> Nothing
          _ : ys -> Just ys
      m6 = case str of
        (x : xs)
          | x == '[' -> do
              i <- elemIndex ']' xs
              return $ second (drop 1) $ splitAt i xs
          | x == ':' -> do
              return (str, "")
        _ -> Nothing
   in (host, srv)

toSockAddr :: Network -> String -> IO [SockAddr]
toSockAddr net str =
  go `catch` e
  where
    go = fmap (map addrAddress) $ getAddrInfo Nothing host srv
    (host, srv) =
      second (<|> Just (show net.defaultPort)) $
        toHostService str
    e :: (Monad m) => SomeException -> m [SockAddr]
    e _ = return []

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

buildVersion ::
  Network ->
  Word64 ->
  BlockHeight ->
  NetworkAddress ->
  NetworkAddress ->
  Word64 ->
  Version
buildVersion net nonce height loc rmt time =
  Version
    { version = myVersion,
      services = loc.services,
      timestamp = time,
      addrRecv = rmt,
      addrSend = loc,
      nonce = nonce,
      userAgent = VarString net.userAgent,
      startHeight = height,
      relay = True
    }

myVersion :: Word32
myVersion = 70012
