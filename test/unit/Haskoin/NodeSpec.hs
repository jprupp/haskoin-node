{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Haskoin.NodeSpec (spec) where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM_, forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base64 (decodeBase64Lenient)
import Data.Default (def)
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Serialize (decode, get, put, runGet, runPut)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word8)
import Database.RocksDB qualified as R
import Haskoin
import Haskoin.Node
import Haskoin.Node.Address (peerHostAddress)
import Haskoin.Node.Test (eventually)
import NQE
import Network.Socket (PortNumber, SockAddr (..), tupleToHostAddress, tupleToHostAddress6)
import System.IO.Temp (withSystemTempDirectory)
import System.Random (randomIO)
import System.Timeout (timeout)
import Test.Hspec

data TestNode = TestNode
  { testMgr :: PeerMgr,
    testChain :: Chain,
    nodeEvents :: Inbox NodeEvent,
    testDB :: R.DB,
    -- | how many getheaders messages the mock peer has received
    getHeadersCount :: TVar Int,
    -- | every address the node tried to connect to, newest first
    attempts :: TVar [SockAddr]
  }

-- | What the mock peer sends back for each message: a message to serialize
-- for the test network, or raw bytes.
type React = Message -> [Either ByteString Message]

-- | How to set up a test node.
data TestOpts = TestOpts
  { react :: React,
    -- | runs on the database before the node starts
    setup :: R.DB -> IO (),
    timeout :: NominalDiffTime,
    -- | connect to seeds and relayed addresses
    discover :: Bool,
    -- | the static peers of the configuration
    staticPeers :: [String]
  }

defOpts :: [Block] -> TestOpts
defOpts blocks =
  TestOpts
    { react = map Right . mockPeerReact blocks,
      setup = \_ -> return (),
      timeout = 120,
      discover = False,
      -- a name for the mock peer, which lives in memory ('dummyPeerConnect'):
      -- a Unix socket, so that no address or port of the machine is named
      staticPeers = ["unix:mock-peer"]
    }

-- | Connect to a mock peer that reacts to messages like this.
dummyPeerConnect ::
  React ->
  TVar Int ->
  Network ->
  NetworkAddress ->
  SockAddr ->
  WithConnection
dummyPeerConnect react count net ad sa f = do
  r <- newInbox
  s <- newInbox
  let s' = inboxToMailbox s
  withAsync (go r s') $ \_ -> do
    let o = awaitForever (liftIO . (`send` r))
        i = forever (liftIO (receive s) >>= yield)
    f (Conduits i o) :: IO ()
  where
    go :: Inbox ByteString -> Mailbox ByteString -> IO ()
    go r s = do
      nonce <- randomIO
      now <- round <$> getPOSIXTime
      let rmt = NetworkAddress 0 (peerHostAddress sa)
          ver = buildVersion net nonce 0 ad rmt now
      runPut (putMessage net (MVersion ver)) `send` s
      runConduit $
        forever (liftIO (receive r) >>= yield)
          .| inc
          .| iterMC count_getheaders
          .| concatMapC react
          .| outc
          .| awaitForever (liftIO . (`send` s))
    outc = mapMC $ return . either id (runPut . putMessage net)
    count_getheaders = \case
      MGetHeaders _ -> atomically (modifyTVar' count (+ 1))
      _ -> return ()
    inc =
      forever $ do
        x <- takeCE 24 .| foldC
        y <- case decode x of
          Left _ -> error "Dummy peer not decode message header"
          Right (MessageHeader _ _ len _) ->
            takeCE (fromIntegral len) .| foldC
        case runGet (getMessage net) $ x `B.append` y of
          Left e ->
            error $
              "Dummy peer could not decode payload: " <> show e
          Right m -> yield m

mockPeerReact :: [Block] -> Message -> [Message]
mockPeerReact _ (MPing (Ping n)) = [MPong (Pong n)]
mockPeerReact _ (MVersion _) = [MVerAck]
mockPeerReact blocks (MGetHeaders (GetHeaders _ _hs _)) = [MHeaders (Headers hs')]
  where
    f b = (b.header, (VarInt . fromIntegral . length) b.txs)
    hs' = map f blocks
mockPeerReact blocks (MGetData (GetData ivs)) = mapMaybe f ivs
  where
    f (InvVector InvBlock h) = MBlock <$> find (l h) blocks
    f _ = Nothing
    l h b = headerHash b.header == BlockHash h
mockPeerReact _ _ = []

spec :: Spec
spec = do
  let net = bchRegTest
  describe "peer manager on test network" $ do
    it "builds a version message with the given height and time" $ do
      let local = NetworkAddress nodeNetwork (sockToHostAddress (SockAddrInet 0 0))
          remote = NetworkAddress 0 (sockToHostAddress (SockAddrInet 8333 0x08080808))
          v = buildVersion net 7 650000 local remote 1700000000
      v.version `shouldBe` 70012
      v.startHeight `shouldBe` 650000
      v.timestamp `shouldBe` 1700000000
  describe "order of peer events" $ do
    it "publishes a peer as connected before the messages it sends right after its handshake" $ do
      -- the mock peer sends a burst right behind its verack, which used to
      -- reach the application ahead of the announcement of the peer
      let burst = [Right (MOther "zz" (B.singleton i)) | i <- [1 .. 12]]
          react = \case
            MVersion _ -> Right MVerAck : burst
            MPing (Ping n) -> [Right (MPong (Pong n))]
            _ -> []
          early = \case
            MVersion _ -> True
            MVerAck -> True
            _ -> False
      withTestNodeOpts (defOpts allBlocks) {react = react} net "burst-after-handshake" $
        \TestNode {..} -> do
          let next = receive nodeEvents >>= judgeEvent
              judgeEvent = \case
                PeerEvent (PeerConnected _) -> return Nothing
                PeerEvent (PeerMessage _ m) | not (early m) -> return (Just (msgType m))
                _ -> next
          timeout (10 * 1000 * 1000) next `shouldReturn` Just Nothing
          payloads <- timeout (10 * 1000 * 1000) . mapM (const (otherPayload nodeEvents)) $ burst
          payloads `shouldBe` Just [B.singleton i | i <- [1 .. 12]]
  describe "untrusted peers" $ do
    it "disconnects a peer that sends more than 2000 headers"
      $ withTestNodeOpts
        (defOpts allBlocks) {react = reactWhenOnline [Right (manyHeaders net 2001)]}
        net
        "too-many-headers"
      $ \TestNode {..} -> do
        p <- waitForPeer nodeEvents
        waitForDisconnect nodeEvents p
        (.height) <$> chainGetBest testChain `shouldReturn` 0
    it "disconnects a peer that sends more than 1000 addresses" $ do
      let na = NetworkAddress 0 (sockToHostAddress (SockAddrInet 8333 0x08080808))
          addrs = MAddr (Addr (replicate 1001 (0, na)))
      withTestNodeOpts
        (defOpts allBlocks) {react = reactWhenOnline [Right addrs]}
        net
        "too-many-addrs"
        $ \TestNode {..} -> do
          p <- waitForPeer nodeEvents
          waitForDisconnect nodeEvents p
    it "disconnects a peer that sends the wrong network magic" $ do
      let bad = runPut (putMessage btc (MPing (Ping 1)))
      withTestNodeOpts
        (defOpts allBlocks) {react = reactWhenOnline [Left bad]}
        net
        "wrong-magic"
        $ \TestNode {..} -> do
          p <- waitForPeer nodeEvents
          waitForDisconnect nodeEvents p
    it "disconnects a peer that announces a tx message larger than 2 MiB" $ do
      -- Only the header is sent, with a length of 2 MiB plus one byte:
      -- 2 × 1024 × 1024 + 1 = 2097153. The node must refuse it there,
      -- without waiting for the payload.
      let oversized =
            runPut . put $
              MessageHeader net.magic MCTx 2097153 (checkSum32 B.empty)
      withTestNodeOpts
        (defOpts allBlocks) {react = reactWhenOnline [Left oversized]}
        net
        "oversized-tx"
        $ \TestNode {..} -> do
          p <- waitForPeer nodeEvents
          waitForDisconnect nodeEvents p
    it "disconnects a peer that sends a second verack"
      $ withTestNodeOpts
        (defOpts allBlocks) {react = reactWhenOnline [Right MVerAck]}
        net
        "duplicate-verack"
      $ \TestNode {..} -> do
        p <- waitForPeer nodeEvents
        waitForDisconnect nodeEvents p
    it "ignores headers sent before the handshake" $ do
      let hs = MHeaders (Headers [(b.header, VarInt 0) | b <- allBlocks])
          react = \case
            MVersion _ -> [Right hs, Right MVerAck]
            MPing (Ping n) -> [Right (MPong (Pong n))]
            _ -> []
      withTestNodeOpts (defOpts allBlocks) {react = react} net "early-headers" $
        \TestNode {..} -> do
          _ <- waitForPeer nodeEvents
          threadDelay (500 * 1000)
          (.height) <$> chainGetBest testChain `shouldReturn` 0
    it "disconnects a peer that chatters instead of answering a ping" $ do
      -- Answer the node's ping with a ping of our own, and its pong with
      -- another ping, so that the peer is never silent.
      let react = \case
            MVersion _ -> [Right MVerAck]
            MPing _ -> [Right (MPing (Ping 1))]
            MPong _ -> [Right (MPing (Ping 1))]
            -- end the header sync so its own timeout does not apply
            MGetHeaders _ -> [Right (MHeaders (Headers []))]
            _ -> []
      withTestNodeOpts
        (defOpts allBlocks) {react = react, timeout = 2}
        net
        "ignored-ping"
        $ \TestNode {..} -> do
          p <- waitForPeer nodeEvents
          waitForDisconnect nodeEvents p
    it "publishes no message from a peer that has not completed the handshake" $ do
      -- The mock peer sends its version, then a message instead of a verack.
      let react = \case
            MVersion _ -> [Right (MOther "zz" "early")]
            _ -> []
      withTestNodeOpts (defOpts allBlocks) {react = react} net "early-message" $
        \TestNode {..} -> do
          -- the version is the handshake itself, and is published
          receiveMatch nodeEvents $ \case
            PeerEvent (PeerMessage _ (MVersion _)) -> Just ()
            _ -> Nothing
          early <- timeout (2 * 1000 * 1000) . receiveMatch nodeEvents $ \case
            PeerEvent (PeerMessage _ (MOther "zz" _)) -> Just ()
            _ -> Nothing
          early `shouldBe` Nothing
    it "publishes the messages of a peer that is online" $ do
      let react = reactWhenOnline [Right (MOther "zz" "late")]
      withTestNodeOpts (defOpts allBlocks) {react = react} net "late-message" $
        \TestNode {..} -> do
          late <- timeout (10 * 1000 * 1000) . receiveMatch nodeEvents $ \case
            PeerEvent (PeerMessage _ (MOther "zz" payload)) -> Just payload
            _ -> Nothing
          late `shouldBe` Just "late"
    it "does not connect to a private address that an online peer relays" $ do
      let private = v4At (10, 1, 2, 3) 8333
          public = v4At (8, 8, 4, 4) 8333
          relay = MAddr (Addr [relayed private, relayed public])
      withTestNodeOpts
        (defOpts allBlocks) {react = reactWhenOnline [Right relay], discover = True}
        net
        "relay-private"
        $ \TestNode {..} -> do
          -- the public address shows that the relayed addresses were learned
          eventually 30 "public address tried" $ elem public <$> readTVarIO attempts
          -- the node tries a new address every 0.1 to 5 seconds
          threadDelay (6 * 1000 * 1000)
          readTVarIO attempts >>= (`shouldNotContain` [private])
    it "connects to at most one relayed address of each network group" $ do
      let sameGroup = [v4At (45, 1, 2, 3) p | p <- [1000 .. 1015]]
          others = [v4At (46, 1, 0, 1) 8333, v4At (47, 2, 0, 1) 8333]
          relay = MAddr (Addr (map relayed (sameGroup <> others)))
      withTestNodeOpts
        (defOpts allBlocks) {react = reactWhenOnline [Right relay], discover = True}
        net
        "relay-same-group"
        $ \TestNode {..} -> do
          eventually 30 "addresses of the other groups tried" $
            (\tried -> all (`elem` tried) others) <$> readTVarIO attempts
          threadDelay (6 * 1000 * 1000)
          tried <- readTVarIO attempts
          length (filter (`elem` sameGroup) tried) `shouldBe` 1
    it "does not connect to a DNS seed result that is not routable"
      $
      -- The seed of regtest is localhost, which resolves to loopback
      -- addresses. With no static peers, nothing else could be tried.
      withTestNodeOpts
        (defOpts allBlocks) {discover = True, staticPeers = []}
        net
        "seed-loopback"
      $ \TestNode {..} -> do
        -- the node tries a new address every 0.1 to 5 seconds
        threadDelay (6 * 1000 * 1000)
        readTVarIO attempts `shouldReturn` []
    it "does not connect to private addresses that peers relay" $ do
      let v4 a b c d p = SockAddrInet p (tupleToHostAddress (a, b, c, d))
          v6 t p = SockAddrInet6 p 0 (tupleToHostAddress6 t) 0
      isRoutable (v4 8 8 8 8 8333) `shouldBe` True
      isRoutable (v4 8 8 8 8 0) `shouldBe` False
      isRoutable (v4 127 0 0 1 8333) `shouldBe` False
      isRoutable (v4 10 1 2 3 8333) `shouldBe` False
      isRoutable (v4 172 16 0 1 8333) `shouldBe` False
      isRoutable (v4 172 32 0 1 8333) `shouldBe` True
      isRoutable (v4 192 168 1 1 8333) `shouldBe` False
      isRoutable (v4 169 254 1 1 8333) `shouldBe` False
      isRoutable (v4 100 64 0 1 8333) `shouldBe` False
      isRoutable (v4 224 0 0 1 8333) `shouldBe` False
      isRoutable (v6 (0x2a01, 0x4f8, 0, 0, 0, 0, 0, 1) 8333) `shouldBe` True
      isRoutable (v6 (0, 0, 0, 0, 0, 0, 0, 1) 8333) `shouldBe` False
      isRoutable (v6 (0xfd00, 0, 0, 0, 0, 0, 0, 1) 8333) `shouldBe` False
      isRoutable (v6 (0xfe80, 0, 0, 0, 0, 0, 0, 1) 8333) `shouldBe` False
      isRoutable (v6 (0, 0, 0, 0, 0, 0xffff, 0x7f00, 1) 8333) `shouldBe` False
      isRoutable (v6 (0, 0, 0, 0, 0, 0xffff, 0x0808, 0x0808) 8333) `shouldBe` True
      isRoutable (hostToSockAddr (sockToHostAddress (v4 127 0 0 1 8333)))
        `shouldBe` False
  describe "chain database" $ do
    it "leaves the default column family alone when purging" $ do
      -- The same key in both families: the purge must delete it only from
      -- the family that holds the chain.
      let k = B.pack [0x90, 1, 2, 3]
          setup db = do
            R.put db k "keep"
            forM_ (R.columnFamilies db) $ \cf -> R.putCF db cf k "stale"
      withTestNodeOpts (defOpts allBlocks) {setup = setup} net "purge-cf" $
        \TestNode {..} -> do
          R.get testDB k `shouldReturn` Just "keep"

-- | An IPv4 socket address.
v4At :: (Word8, Word8, Word8, Word8) -> PortNumber -> SockAddr
v4At a p = SockAddrInet p (tupleToHostAddress a)

-- | An address as a peer relays it in an addr message.
relayed :: SockAddr -> (Word32, NetworkAddress)
relayed sa = (0, NetworkAddress nodeNetwork (sockToHostAddress sa))

-- | Wait for the node to disconnect from this peer, failing after a while.
waitForDisconnect :: Inbox NodeEvent -> Peer -> IO ()
waitForDisconnect inbox p =
  timeout (20 * 1000 * 1000) disconnected >>= \case
    Just () -> return ()
    Nothing -> expectationFailure "peer was not disconnected"
  where
    disconnected =
      receiveMatch inbox $ \case
        PeerEvent (PeerDisconnected p') | p == p' -> Just ()
        _ -> Nothing

-- | Mock peer that answers the handshake and pings, and sends these each
-- time the node asks it for headers. The chain asks only once the peer is
-- announced as connected, so the messages cannot overtake the announcement.
-- Sent right after the verack, they raced with it: a peer that misbehaved at
-- once could die before the node handled its verack, and was then never
-- announced.
reactWhenOnline :: [Either ByteString Message] -> React
reactWhenOnline extra = \case
  MVersion _ -> [Right MVerAck]
  MGetHeaders _ -> extra
  MPing (Ping n) -> [Right (MPong (Pong n))]
  _ -> []

-- | Headers message with a valid chain of this many headers on top of
-- genesis.
manyHeaders :: Network -> Int -> Message
manyHeaders net n =
  MHeaders . Headers $
    map (,VarInt 0) (appendBlocks net 0 net.genesisHeader n)

-- | The payload of the next unknown message that the node publishes.
otherPayload :: Inbox NodeEvent -> IO ByteString
otherPayload events =
  receive events >>= \case
    PeerEvent (PeerMessage _ (MOther "zz" payload)) -> return payload
    _ -> otherPayload events

-- | Wait for a peer to be announced, failing after a while.
waitForPeer :: Inbox NodeEvent -> IO Peer
waitForPeer inbox =
  timeout (30 * 1000 * 1000) connected >>= \case
    Just p -> return p
    Nothing -> expectationFailure "no peer connected" >> error "unreachable"
  where
    connected =
      receiveMatch inbox $ \case
        PeerEvent (PeerConnected p) -> Just p
        _ -> Nothing

withLifted :: ((a -> IO b) -> IO b) -> (a -> IO b) -> IO b
withLifted w m = withRunInIO $ \io -> w (io . m)

withTestNodeOpts ::
  TestOpts -> Network -> String -> (TestNode -> IO a) -> IO a
withTestNodeOpts opts net str f =
  withSystemTempDirectory ("haskoin-node-test-" <> str <> "-") $ \w ->
    withPublisher $ \pub ->
      withSubscription pub $ \sub ->
        withLifted (R.withDBCF w cfg cols) $ \db -> do
          count <- newTVarIO 0
          tried <- newTVarIO []
          opts.setup db
          withNode (nodeConfig count tried pub db) $ \(Node mgr ch) ->
            f
              TestNode
                { testMgr = mgr,
                  testChain = ch,
                  nodeEvents = sub,
                  testDB = db,
                  getHeadersCount = count,
                  attempts = tried
                }
  where
    nodeConfig count tried pub db =
      NodeConfig
        { maxPeers = 20,
          db = db,
          cf = listToMaybe (R.columnFamilies db),
          peers = opts.staticPeers,
          discover = opts.discover,
          address = NetworkAddress 0 anywhere,
          net = net,
          pub = pub,
          timeout = opts.timeout,
          maxPeerLife = 48 * 3600,
          connect = \sa conn -> do
            atomically $ modifyTVar' tried (sa :)
            dummyPeerConnect
              opts.react
              count
              net
              (NetworkAddress nodeNetwork anywhere)
              sa
              conn,
          logger = Logger {level = LevelError, source = "", push = \_ -> return ()},
          permissions = mempty
        }
    anywhere = sockToHostAddress (SockAddrInet 0 0)
    cfg = def {R.createIfMissing = True, R.errorIfExists = True}
    cols = [("node", def)]

allBlocks :: [Block]
allBlocks =
  fromRight (error "Could not decode blocks") $
    runGet f (decodeBase64Lenient allBlocksBase64)
  where
    f = mapM (const get) [(1 :: Int) .. 15]

allBlocksBase64 :: ByteString
allBlocksBase64 =
  "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\
  \wT6Wn6LtY47B39Th44JKsZ8BGJCvc6wAAAAA"
