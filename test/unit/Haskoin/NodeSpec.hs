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
import Control.Monad (forM_, forever, replicateM)
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
import Database.RocksDB qualified as R
import Haskoin
import Haskoin.Node
import NQE
import Network.Socket (SockAddr (..), tupleToHostAddress, tupleToHostAddress6)
import System.Random (randomIO)
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

data TestNode = TestNode
  { testMgr :: PeerMgr,
    testChain :: Chain,
    nodeEvents :: Inbox NodeEvent,
    testDB :: R.DB,
    -- | how many getheaders messages the mock peer has received
    getHeadersCount :: TVar Int
  }

-- | What the mock peer sends back for each message: a message to serialize
-- for the test network, or raw bytes.
type React = Message -> [Either ByteString Message]

-- | How to set up a test node.
data TestOpts = TestOpts
  { react :: React,
    -- | runs on the database before the node starts
    setup :: R.DB -> IO (),
    timeout :: NominalDiffTime
  }

defOpts :: [Block] -> TestOpts
defOpts blocks =
  TestOpts
    { react = map Right . mockPeerReact blocks,
      setup = \_ -> return (),
      timeout = 120
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
    let o = awaitForever (`send` r)
        i = forever (receive s >>= yield)
    f (Conduits i o) :: IO ()
  where
    go :: Inbox ByteString -> Mailbox ByteString -> IO ()
    go r s = do
      nonce <- randomIO
      now <- round <$> getPOSIXTime
      let rmt = NetworkAddress 0 (sockToHostAddress sa)
          ver = buildVersion net nonce 0 ad rmt now
      runPut (putMessage net (MVersion ver)) `send` s
      runConduit $
        forever (receive r >>= yield)
          .| inc
          .| iterMC count_getheaders
          .| concatMapC react
          .| outc
          .| awaitForever (`send` s)
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
    it "connects to a peer" $
      withTestNode net "connect-one-peer" $ \TestNode {..} -> do
        p <- waitForPeer nodeEvents
        Just o <- getOnlinePeer testMgr p
        fmap (.version) o.version `shouldSatisfy` maybe False (>= 70002)
    it "downloads some blocks" $
      withTestNode net "get-blocks" $ \TestNode {..} -> do
        let h1 =
              "3094ed3592a06f3d8e099eed2d9c1192329944f5df4a48acb29e08f12cfbb660"
            h2 =
              "0c89955fc5c9f98ecc71954f167b938138c90c6a094c4737f2e901669d26763f"
        p <- waitForPeer nodeEvents
        pbs <- getBlocks net 10 p [h1, h2]
        (b1, b2) <- case pbs of
          Just [b1, b2] -> return (b1, b2)
          _ -> fail ("Expected two blocks, got " <> show pbs)
        headerHash b1.header `shouldBe` h1
        headerHash b2.header `shouldBe` h2
        let ths b = map txHash b.txs
            testMerkle b = buildMerkleRoot (ths b) `shouldBe` Right b.header.merkle
        testMerkle b1
        testMerkle b2
    it "downloads a block larger than 2 MiB" $ do
      -- On Bitcoin Cash a block message may be up to twice the block size,
      -- while every other message is limited to 2 MiB (2097152 bytes).
      let script = B.replicate 3000000 0x6a
          tx = Tx 1 [TxIn nullOutPoint B.empty maxBound] [TxOut 0 script] [] 0
          big = case allBlocks of
            firstBlock : _ -> Block firstBlock.header [tx]
            [] -> error "no test blocks"
      withTestNodeBlocks [big] net "big-block" $ \TestNode {..} -> do
        p <- waitForPeer nodeEvents
        pbs <- getBlocks net 10 p [headerHash big.header]
        let scriptSizes =
              [[B.length o.script | o <- t.outputs] | b <- concat pbs, t <- b.txs]
        scriptSizes `shouldBe` [[3000000]]
    it "builds a version message with the given height and time" $ do
      let local = NetworkAddress nodeNetwork (sockToHostAddress (SockAddrInet 0 0))
          remote = NetworkAddress 0 (sockToHostAddress (SockAddrInet 8333 0x08080808))
          v = buildVersion net 7 650000 local remote 1700000000
      v.version `shouldBe` 70012
      v.startHeight `shouldBe` 650000
      v.timestamp `shouldBe` 1700000000
  describe "untrusted peers" $ do
    it "imports a full batch of 2000 headers"
      $ withTestNodeOpts
        (defOpts allBlocks) {react = reactAfterVerAck [Right (manyHeaders net 2000)]}
        net
        "full-batch"
      $ \TestNode {..} -> do
        bn <-
          receiveMatch nodeEvents $ \case
            ChainEvent (ChainBestBlock bn) | bn.height > 0 -> Just bn
            _ -> Nothing
        bn.height `shouldBe` 2000
    it "disconnects a peer that sends more than 2000 headers"
      $ withTestNodeOpts
        (defOpts allBlocks) {react = reactAfterVerAck [Right (manyHeaders net 2001)]}
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
        (defOpts allBlocks) {react = reactAfterVerAck [Right addrs]}
        net
        "too-many-addrs"
        $ \TestNode {..} -> do
          p <- waitForPeer nodeEvents
          waitForDisconnect nodeEvents p
    it "disconnects a peer that sends the wrong network magic" $ do
      let bad = runPut (putMessage btc (MPing (Ping 1)))
      withTestNodeOpts
        (defOpts allBlocks) {react = reactAfterVerAck [Left bad]}
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
        (defOpts allBlocks) {react = reactAfterVerAck [Left oversized]}
        net
        "oversized-tx"
        $ \TestNode {..} -> do
          p <- waitForPeer nodeEvents
          waitForDisconnect nodeEvents p
    it "disconnects a peer that sends a second verack"
      $ withTestNodeOpts
        (defOpts allBlocks) {react = reactAfterVerAck [Right MVerAck]}
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
  describe "chain on test network" $ do
    it "stops asking a peer that has no more headers" $
      -- The peer answers getheaders with an empty headers message, as a
      -- peer does when the node already has its whole chain. The node must
      -- take that as the end of the sync instead of asking again.
      withTestNodeBlocks [] net "no-headers" $ \TestNode {..} -> do
        _ <- waitForPeer nodeEvents
        atomically $ readTVar getHeadersCount >>= checkSTM . (>= 1)
        threadDelay (1000 * 1000)
        readTVarIO getHeadersCount `shouldReturn` 1
    it "reports the headers as synced when the best header has the minimum chain work" $ do
      -- Every regtest header has a work of 2. Genesis and one header on top
      -- of it have 2 + 2 = 4, which is the minimum here.
      fresh <- freshHeader net
      let net4 = net {minimumChainWork = 4}
      withTestNodeBlocks [Block fresh []] net4 "min-work-reached" $
        \TestNode {..} -> do
          receiveMatch nodeEvents $ \case
            ChainEvent (ChainBestBlock bn) | bn.height == 1 -> Just ()
            _ -> Nothing
          chainSync testChain
          chainIsSynced testChain `shouldReturn` True
    it "does not report the headers as synced below the minimum chain work" $ do
      -- Same chain with a work of 2 + 2 = 4, one short of a minimum of 5.
      fresh <- freshHeader net
      let net5 = net {minimumChainWork = 5}
      withTestNodeBlocks [Block fresh []] net5 "min-work-missing" $
        \TestNode {..} -> do
          receiveMatch nodeEvents $ \case
            ChainEvent (ChainBestBlock bn) | bn.height == 1 -> Just ()
            _ -> Nothing
          chainSync testChain
          chainIsSynced testChain `shouldReturn` False
    it "syncs some headers" $
      withTestNode net "connect-sync" $ \TestNode {..} -> do
        let bh =
              "3bfa0c6da615fc45aa44ddea6854ac19d16f3ca167e0e21ac2cc262a49c9b002"
            ah =
              "7dc835a78a55fa76f9184dc4f6663a73e418c7afec789c5ae25e432fd7fc8467"
        bn <-
          receiveMatch nodeEvents $ \case
            ChainEvent (ChainBestBlock bn)
              | bn.height > 0 -> Just bn
            _ -> Nothing
        bb <- chainGetBest testChain
        bb.height `shouldSatisfy` (== 15)
        an <-
          maybe (error "No ancestor found") return
            =<< chainGetAncestor testChain 10 bn
        headerHash bn.header `shouldBe` bh
        headerHash an.header `shouldBe` ah
    it "downloads some block parents" $
      withTestNode net "parents" $ \TestNode {..} -> do
        let hs =
              [ "52e886df7b166d961ac2d3d2d561d806325d51a609dc0a5d9d5fcb65d47906d7",
                "2537a081b9e2b24d217fac2886f387758cb3aa4e4956b3be7ed229bafbb71b0f",
                "7c72f306215a296f9714320a497b1f2cb5f9b99f162d7e04333c243fac9a54d8"
              ]
        [_, bn] <-
          replicateM 2 $
            receiveMatch nodeEvents $ \case
              ChainEvent (ChainBestBlock bn) -> Just bn
              _ -> Nothing
        bn.height `shouldBe` 15
        ps <- chainGetParents testChain 12 bn
        length ps `shouldBe` 3
        forM_ (zip ps hs) $ \(p, h) ->
          headerHash p.header `shouldBe` h

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

-- | Mock peer that answers the handshake and pings, and sends these right
-- after its version ack.
reactAfterVerAck :: [Either ByteString Message] -> React
reactAfterVerAck extra = \case
  MVersion _ -> Right MVerAck : extra
  MPing (Ping n) -> [Right (MPong (Pong n))]
  _ -> []

-- | Headers message with a valid chain of this many headers on top of
-- genesis.
manyHeaders :: Network -> Int -> Message
manyHeaders net n =
  MHeaders . Headers $
    map (,VarInt 0) (appendBlocks net 0 net.genesisHeader n)

-- | Valid header on top of genesis that is dated now. The node only counts
-- its headers as synced when the best one is less than two hours old, and
-- the headers of the other fixtures are years old.
freshHeader :: Network -> IO BlockHeader
freshHeader net = do
  now <- round <$> getPOSIXTime
  let genesis = net.genesisHeader
  return $
    mineBlock
      net
      0
      genesis {version = 4, prev = headerHash genesis, timestamp = now}

waitForPeer :: Inbox NodeEvent -> IO Peer
waitForPeer inbox =
  receiveMatch inbox $ \case
    PeerEvent (PeerConnected p) -> Just p
    _ -> Nothing

withLifted :: ((a -> IO b) -> IO b) -> (a -> IO b) -> IO b
withLifted w m = withRunInIO $ \io -> w (io . m)

withTestNode ::
  Network -> String -> (TestNode -> IO a) -> IO a
withTestNode = withTestNodeBlocks allBlocks

-- | Like 'withTestNode', with the mock peer serving these blocks.
withTestNodeBlocks ::
  [Block] -> Network -> String -> (TestNode -> IO a) -> IO a
withTestNodeBlocks blocks = withTestNodeOpts (defOpts blocks)

withTestNodeOpts ::
  TestOpts -> Network -> String -> (TestNode -> IO a) -> IO a
withTestNodeOpts opts net str f =
  withSystemTempDirectory ("haskoin-node-test-" <> str <> "-") $ \w ->
    withPublisher $ \pub ->
      withSubscription pub $ \sub ->
        withLifted (R.withDBCF w cfg cols) $ \db -> do
          count <- newTVarIO 0
          opts.setup db
          withNode (nodeConfig count pub db) $ \(Node mgr ch) ->
            f
              TestNode
                { testMgr = mgr,
                  testChain = ch,
                  nodeEvents = sub,
                  testDB = db,
                  getHeadersCount = count
                }
  where
    nodeConfig count pub db =
      NodeConfig
        { maxPeers = 20,
          db = db,
          cf = listToMaybe (R.columnFamilies db),
          -- IPv4, so that the mock peer is reachable without IPv6
          peers = ["127.0.0.1:17486"],
          discover = False,
          address = NetworkAddress 0 anywhere,
          net = net,
          pub = pub,
          timeout = opts.timeout,
          maxPeerLife = 48 * 3600,
          connect =
            dummyPeerConnect
              opts.react
              count
              net
              (NetworkAddress nodeNetwork anywhere),
          logger = Logger {level = LevelError, source = "", push = \_ -> return ()}
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
