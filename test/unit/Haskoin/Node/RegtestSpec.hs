-- | Test the library against Bitcoin Core and Bitcoin Cash Node in regtest
-- mode. Nodes run in Podman containers built from @test/regtest/Containerfile@,
-- through podman-test ("Regtest.Podman"), and the node under test reaches
-- them through Unix sockets, as the peers @unix:PATH@. The tests are
-- skipped when Podman cannot run, or when @HASKOIN_NODE_SKIP_REGTEST@ or
-- @PODMAN_TEST_SKIP@ is set.
--
-- They are part of the @spec@ suite, so that @stack test@ and @haskell-dev
-- test@ run every test. Each node implementation has two groups at the top
-- of the module, its chain and its one peer, which @haskell-dev@ (of
-- jprupp/tools) runs each in a process of its own, at the same time as the
-- others (@haskell-dev.toml@): each container is in a pod of its own with
-- its own sockets, so they never meet. The image is built by the first test
-- that needs it, once for every process of the machine.
--
-- These tests replace the mock peers that once stood in for an honest
-- node: what a node does, a real one shows. A peer that hangs is a node
-- stopped with @SIGSTOP@ ('freezeNode'). The tests of peers that break the
-- rules, which no real node does, keep their scripted peers in memory.
module Haskoin.Node.RegtestSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (finally)
import Control.Monad (forM_, replicateM_, void, when)
import Data.ByteString.Char8 qualified as C8
import Data.List (intercalate, isInfixOf, isPrefixOf, tails)
import Data.Maybe (isJust, mapMaybe)
import Data.Serialize (encode)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Haskoin
import Haskoin.Network.Protocol qualified as P
import Haskoin.Node
import Haskoin.Node qualified as NC (NodeConfig (..))
import Haskoin.Node.Peer.Remote (Facts (..), newRemote, predict)
import Haskoin.Node.Test
import Network.Socket (SockAddr (..))
import Regtest.Podman
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = do
  skip <- runIO (isJust <$> lookupEnv "HASKOIN_NODE_SKIP_REGTEST")
  usable <- if skip then return (Left "HASKOIN_NODE_SKIP_REGTEST set") else runIO podmanUsable
  case usable of
    Left why -> skipped why
    Right () -> regtests
  where
    skipped why = describe "Regtest" $ it "runs the nodes in Podman" $ pendingWith why

-- | Node connected to the given nodes in the container. The timeout is long
-- so that the stall recovery poll does not hide announcement handling bugs.
withRegtestNode :: Container -> [Int] -> (TestNode -> IO a) -> IO a
withRegtestNode c is f = do
  addrs <- mapM (hostAddress c) is
  withTestNode
    (implNet c.impl)
    addrs
    withConnection
    (setTimeout 600)
    f

-- | Wait until our best header is the tip of node number @i@.
waitForTip :: Container -> Int -> TestNode -> IO BlockNode
waitForTip c i t = do
  tip <- fromString <$> cli c i ["getbestblockhash"]
  waitForBest 60 t ((== tip) . headerHash . (.header))

blockHashAt :: Container -> Int -> Int -> IO BlockHash
blockHashAt c i h = fromString <$> cli c i ["getblockhash", show h]

-- | Node 0 with blocks mined so that it leaves initial block download.
withMinedNode :: Impl -> Int -> (Container -> IO a) -> IO a
withMinedNode impl n f =
  withContainer impl $ \c -> do
    startNode c 0 []
    _ <- mine c 0 n
    f c

-- | The tests of each node implementation, two groups at the top of the
-- module.
regtests :: Spec
regtests = forM_ [BitcoinCore, BitcoinCashNode] $ \impl -> do
  describe (implName impl) (chainTests impl)
  describe (implName impl <> ", one peer") (peerTests impl)

-- | How the node under test follows the chain of the nodes.
chainTests :: Impl -> Spec
chainTests impl = do
  let net = implNet impl

  it "completes the handshake" $ withMinedNode impl 1 $ \c ->
    withRegtestNode c [0] $ \t -> do
      p <- waitForPeer t
      Just o <- getOnlinePeer t.node.peerMgr p
      Just v <- return o.version
      v.version `shouldSatisfy` (>= 70012)
      let VarString ua = v.userAgent
          expected = case impl of
            BitcoinCore -> "Satoshi"
            BitcoinCashNode -> "Bitcoin Cash Node"
      show ua `shouldSatisfy` (expected `isInfixOf`)

  it "syncs a chain longer than one header batch" $
    withMinedNode impl 2100 $ \c ->
      withRegtestNode c [0] $ \t -> do
        bn <- receiveWithin 120 "synced" t.events $ \case
          ChainEvent (ChainSynced bn) -> Just bn
          _ -> Nothing
        bn.height `shouldBe` 2100
        void $ waitForTip c 0 t

  it "follows blocks mined one at a time" $ withMinedNode impl 1 $ \c ->
    withRegtestNode c [0] $ \t -> do
      _ <- waitForTip c 0 t
      replicateM_ 5 $ mine c 0 1 >> waitForTip c 0 t

  it "follows bursts of blocks without stalling" $
    withMinedNode impl 1 $ \c ->
      withRegtestNode c [0] $ \t -> do
        _ <- waitForTip c 0 t
        replicateM_ 3 $ mine c 0 99 >> waitForTip c 0 t

  it "follows a burst longer than a header batch" $
    withMinedNode impl 1 $ \c ->
      withRegtestNode c [0] $ \t -> do
        _ <- waitForTip c 0 t
        _ <- mine c 0 2500
        bn <- waitForTip c 0 t
        bn.height `shouldBe` 2501

  it "follows a reorganization" $ withMinedNode impl 10 $ \c ->
    withRegtestNode c [0] $ \t -> do
      old <- waitForTip c 0 t
      h8 <- cli c 0 ["getblockhash", "8"]
      void $ cli c 0 ["invalidateblock", h8]
      _ <- mineTo c 0 5 otherAddress
      new <- waitForTip c 0 t
      new.height `shouldBe` 12
      chainBlockMain t.node.chain (headerHash old.header) `shouldReturn` False
      split <- chainGetSplitBlock t.node.chain old new
      split.height `shouldBe` 7

  it "follows a reorganization found in a burst" $
    withMinedNode impl 120 $ \c ->
      withRegtestNode c [0] $ \t -> do
        _ <- waitForTip c 0 t
        h <- cli c 0 ["getblockhash", "70"]
        void $ cli c 0 ["invalidateblock", h]
        _ <- mineTo c 0 99 otherAddress
        new <- waitForTip c 0 t
        new.height `shouldBe` 168

  it "downloads blocks" $ withMinedNode impl 5 $ \c ->
    withRegtestNode c [0] $ \t -> do
      p <- waitForPeer t
      _ <- waitForTip c 0 t
      hs <- mapM (blockHashAt c 0) [1 .. 5]
      Just bs <- getBlocks net 10 p hs
      map (headerHash . (.header)) bs `shouldBe` hs
      forM_ bs $ \b ->
        buildMerkleRoot (map txHash b.txs) `shouldBe` Right b.header.merkle
      let unknown = headerHash (mkHeaders net 9 net.genesisHeader 0 1 !! 0)
      getBlocks net 10 p [unknown] `shouldReturn` Nothing

  it "downloads announced transactions" $ withContainer impl $ \c -> do
    startNode c 0 []
    -- Bitcoin Cash Node also loads a default wallet, so name ours
    let wallet args = cli c 0 ("-rpcwallet=w" : args)
    void $ cli c 0 ["createwallet", "w"]
    addr <- wallet ["getnewaddress"]
    void $ mineTo c 0 101 addr
    withRegtestNode c [0] $ \t -> do
      p <- waitForPeer t
      _ <- waitForTip c 0 t
      txid <- fromString <$> wallet ["sendtoaddress", mineAddress, "1"]
      receiveWithin 60 "transaction announcement" t.events $ \case
        PeerEvent (PeerMessage p' (MInv (Inv ivs)))
          | p == p' && any ((== txid) . TxHash . (.invHash)) ivs -> Just ()
        _ -> Nothing
      Just [tx] <- getTxs net 10 p [txid]
      txHash tx `shouldBe` txid

  it "pings a peer" $ withMinedNode impl 1 $ \c ->
    withRegtestNode c [0] $ \t -> do
      p <- waitForPeer t
      pingPeer 10 p `shouldReturn` True

  it "reconnects to a restarted peer and catches up" $
    withMinedNode impl 5 $ \c ->
      withRegtestNode c [0] $ \t -> do
        p <- waitForPeer t
        _ <- waitForTip c 0 t
        stopNode c 0
        waitForDisconnect t p
        startNode c 0 []
        _ <- mine c 0 3
        _ <- waitForPeer t
        bn <- waitForTip c 0 t
        bn.height `shouldBe` 8

  it "keeps following the chain when a peer goes away" $
    withMinedNode impl 5 $ \c -> do
      startNode c 1 ["-addnode=" <> nodeAddress c 0]
      eventually 60 "nodes in sync" $
        (== "5") <$> cli c 1 ["getblockcount"]
      withRegtestNode c [0, 1] $ \t -> do
        _ <- waitForPeer t
        _ <- waitForPeer t
        _ <- waitForTip c 0 t
        stopNode c 0
        _ <- mine c 1 5
        bn <- waitForTip c 1 t
        bn.height `shouldBe` 10

  it "follows bursts announced by several peers" $
    withMinedNode impl 1 $ \c -> do
      startNode c 1 ["-addnode=" <> nodeAddress c 0]
      withRegtestNode c [0, 1] $ \t -> do
        _ <- waitForPeer t
        _ <- waitForPeer t
        _ <- waitForTip c 0 t
        replicateM_ 3 $ mine c 0 99 >> waitForTip c 0 t
        eventually 60 "nodes in sync" $
          (== "298") <$> cli c 1 ["getblockcount"]
        expectNothingWithin 1 "peer disconnection" t.events $ \case
          PeerEvent (PeerDisconnected _) -> Just ()
          _ -> Nothing

-- | The commands of the messages that the first peer sent, up to its
-- verack, as the node heard them.
handshakeHeard :: TestNode -> IO [MessageCommand]
handshakeHeard t = go []
  where
    go acc =
      receiveWithin 30 "the handshake of the peer" t.events Just >>= \case
        PeerEvent (PeerMessage _ m)
          | MVerAck <- m -> return (reverse (msgType m : acc))
          | otherwise -> go (msgType m : acc)
        _ -> go acc

-- | The commands up to the first verack.
upToVerAck :: [MessageCommand] -> [MessageCommand]
upToVerAck cmds = case break (== MCVerAck) cmds of
  (early, v : _) -> early <> [v]
  (early, []) -> early

isRefused :: Judgement -> Bool
isRefused = \case
  Refused _ _ -> True
  _ -> False

-- | The bytes of @getheaders@ messages that node 0 has had from its peers,
-- from what @getpeerinfo@ says it received. A count that does not grow
-- means no new request.
headerRequests :: Container -> IO Int
headerRequests c = do
  info <- cli c 0 ["getpeerinfo"]
  let received = drop 1 (splitOn "\"bytesrecv_per_msg\"" info)
      counts = [count r | r <- received]
  return (sum counts)
  where
    count section = case [rest | rest <- tails section, "\"getheaders\":" `isPrefixOf` rest] of
      rest : _ -> read (takeWhile (`elem` ['0' .. '9']) (dropWhile (== ' ') (drop 13 rest)))
      [] -> 0
    splitOn sep s = case breakOn sep s of
      (first, Just rest) -> first : splitOn sep rest
      (first, Nothing) -> [first]
    breakOn sep s = go "" s
      where
        go acc rest
          | sep `isPrefixOf` rest = (reverse acc, Just (drop (length sep) rest))
          | otherwise = case rest of
              x : xs -> go (x : acc) xs
              [] -> (reverse acc, Nothing)

-- | Outputs of 1000 satoshis to so many addresses, as @sendmany@ takes them:
-- a JSON object from address to amount. The addresses hash numbers that
-- differ from one transaction to the next, so no two are alike; nobody can
-- spend them, which a test does not need.
manyOutputs :: Network -> Int -> Int -> String
manyOutputs net k n =
  "{" <> intercalate "," [show (addressOf i) <> ":0.00001" | i <- [1 .. n]] <> "}"
  where
    addressOf i =
      maybe (error "No address") T.unpack $
        addrToText net (PubKeyAddress (ripemd160 (C8.pack (show (k, i)))))

-- | Mine one block on node 0, and give its hash.
lastMined :: Container -> IO String
lastMined c =
  mine c 0 1 >>= \case
    [h] -> return h
    hs -> fail ("Mined " <> show (length hs) <> " blocks, not one")

-- | How the node under test treats one peer, and how the model of the
-- peer agrees with it ("Haskoin.Node.Peer.Remote"). A group of its own,
-- so that it runs at the same time as the chain tests.
peerTests :: Impl -> Spec
peerTests impl = do
  let net = implNet impl

  -- The model of the node ("Haskoin.Node.Peer.Remote") is read from the
  -- upstream source; these tests check it against the node itself.
  it "hears the handshake that the model predicts" $ withMinedNode impl 1 $ \c ->
    withRegtestNode c [0] $ \t -> do
      heard <- handshakeHeard t
      let zero = NetworkAddress 0 (sockToHostAddress (SockAddrInet 0 0))
          ours = buildVersion net 0 0 zero zero 0
          facts = Facts {now = 0, best = genesisNode net, known = mempty}
          model = newRemote net Set.empty AddrIPv4 True 0 0
          predicted = mapMaybe sent (fst (predict facts (MVersion ours) model))
          sent e = case e.action of
            P.Send m -> Just (msgType m)
            _ -> Nothing
      heard `shouldBe` upToVerAck predicted

  it "asks for the mempool only where the node allows it" $
    withMinedNode impl 1 $ \c ->
      withRegtestNode c [0] $ \t -> do
        p <- waitForPeer t
        j <- judgeMessage p MMempool
        -- Bitcoin Core offers no Bloom filters by default, and closes the
        -- connection of a peer that asks for its mempool without them;
        -- Bitcoin Cash Node offers them
        case impl of
          BitcoinCore -> j `shouldSatisfy` isRefused
          BitcoinCashNode -> j `shouldBe` Allowed
        MMempool `sendMessage` p
        pingPeer 10 p `shouldReturn` True

  it "asks a node that whitelisted it for its mempool" $ withContainer impl $ \c -> do
    startNode c 0 ["-whitelist=mempool@0.0.0.0/0"]
    _ <- mine c 0 1
    addr <- hostAddress c 0
    let granted cfg = cfg {NC.permissions = Set.singleton PermMempool}
    withTestNode net [addr] withConnection (granted . setTimeout 600) $ \t -> do
      p <- waitForPeer t
      judgeMessage p MMempool `shouldReturn` Allowed
      MMempool `sendMessage` p
      pingPeer 10 p `shouldReturn` True

  it "downloads a block as the node keeps it, witness and all" $ withContainer impl $ \c -> do
    startNode c 0 []
    let wallet args = cli c 0 ("-rpcwallet=w" : args)
    void $ cli c 0 ["createwallet", "w"]
    -- on Bitcoin Core a new address of the wallet is a SegWit one, so the
    -- payment to it is spent with a witness later; here it is the input
    -- of the payment that has one, from a coinbase paid to such an address
    addr <- wallet ["getnewaddress"]
    void $ mineTo c 0 101 addr
    void $ wallet ["sendtoaddress", addr, "1"]
    h <- lastMined c
    raw <- cli c 0 ["getblock", h, "0"]
    withRegtestNode c [0] $ \t -> do
      p <- waitForPeer t
      _ <- waitForTip c 0 t
      Just [b] <- getBlocks net 10 p [fromString h]
      encodeHex (encode b) `shouldBe` T.strip (T.pack raw)
      length b.txs `shouldBe` 2
      when (impl == BitcoinCore) $
        any (any (not . null) . (.witness)) b.txs `shouldBe` True

  it "gives nothing for blocks or pings that a frozen peer leaves unanswered" $
    withMinedNode impl 1 $ \c ->
      withRegtestNode c [0] $ \t -> do
        p <- waitForPeer t
        _ <- waitForTip c 0 t
        h <- blockHashAt c 0 1
        freezeNode c 0
        flip finally (thawNode c 0) $ do
          getBlocks net 2 p [h] `shouldReturn` Nothing
          pingPeer 2 p `shouldReturn` False

  it "drops a peer that stops answering" $ withMinedNode impl 1 $ \c -> do
    addr <- hostAddress c 0
    withTestNode net [addr] withConnection (setTimeout 3) $ \t -> do
      p <- waitForPeer t
      freezeNode c 0
      waitForDisconnect t p `finally` thawNode c 0

  it "drops a peer connected for longer than the most it allows" $
    withMinedNode impl 1 $ \c -> do
      addr <- hostAddress c 0
      withTestNode net [addr] withConnection (setMaxPeerLife 2 . setTimeout 4) $ \t -> do
        p <- waitForPeer t
        waitForDisconnect t p

  it "answers the pings of the node" $ withMinedNode impl 1 $ \c ->
    withRegtestNode c [0] $ \t -> do
      _ <- waitForPeer t
      void $ cli c 0 ["ping"]
      -- the node shows the time of a ping only once it has its pong
      eventually 30 "the time of a ping of the node" $
        ("\"pingtime\"" `isInfixOf`) <$> cli c 0 ["getpeerinfo"]

  it "refuses a node that does not keep the whole chain" $ withContainer impl $ \c -> do
    -- a pruned node offers NODE_NETWORK_LIMITED and not NODE_NETWORK
    startNode c 0 ["-prune=550"]
    _ <- mine c 0 1
    addr <- hostAddress c 0
    attempts <- newTVarIO (0 :: Int)
    let counted sa g = atomically (modifyTVar' attempts (+ 1)) >> withConnection sa g
    withTestNode net [addr] counted (setTimeout 600) $ \t -> do
      eventually 30 "a connection" $ (>= 1) <$> readTVarIO attempts
      expectNothingWithin 5 "peer connection" t.events $ \case
        PeerEvent (PeerConnected _) -> Just ()
        _ -> Nothing

  it "waits before trying again a peer that failed" $ withContainer impl $ \c -> do
    -- node 0 is not started: its socket is there, and nothing answers
    -- behind it
    addr <- hostAddress c 0
    times <- newTVarIO []
    let timed sa g = do
          now <- getCurrentTime
          atomically (modifyTVar' times (now :))
          withConnection sa g
    withTestNode net [addr] timed id $ \_ -> do
      eventually 20 "two attempts" $ (>= 2) . length <$> readTVarIO times
      -- the first failure waits two seconds; the connect loop alone could
      -- try again within a tenth of a second
      newestFirst <- readTVarIO times
      case reverse newestFirst of
        first : second : _ -> (second `diffUTCTime` first) `shouldSatisfy` (>= 1.9)
        _ -> expectationFailure "fewer than two attempts"

  it "says the headers are synced only with the minimum chain work" $
    -- every regtest header has a work of 2: genesis and the block on top
    -- of it have 4
    withMinedNode impl 1 $ \c -> do
      addr <- hostAddress c 0
      forM_ [(4, True), (5, False)] $ \(least, synced) ->
        withTestNode net {minimumChainWork = least} [addr] withConnection (setTimeout 600) $ \t -> do
          _ <- waitForTip c 0 t
          chainSync t.node.chain
          chainIsSynced t.node.chain `shouldReturn` synced

  it "gives the parents of a block" $ withMinedNode impl 15 $ \c ->
    withRegtestNode c [0] $ \t -> do
      bn <- waitForTip c 0 t
      ps <- chainGetParents t.node.chain 12 bn
      hs <- mapM (blockHashAt c 0) [12, 13, 14]
      map (headerHash . (.header)) ps `shouldBe` hs

  it "stops asking for headers once it has them all" $ withMinedNode impl 5 $ \c ->
    withRegtestNode c [0] $ \t -> do
      _ <- waitForTip c 0 t
      threadDelay 1000000
      asked <- headerRequests c
      threadDelay 3000000
      headerRequests c `shouldReturn` asked

  when (impl == BitcoinCashNode) $
    it "downloads a block larger than 2 MiB" $
      withContainer impl $ \c -> do
        -- on Bitcoin Cash a block message may be up to twice the block size,
        -- while every other message is limited to 2 MiB (2097152 bytes)
        startNode c 0 []
        let wallet args = cli c 0 ("-rpcwallet=w" : args)
        void $ cli c 0 ["createwallet", "w"]
        addr <- wallet ["getnewaddress"]
        void $ mineTo c 0 140 addr
        -- 40 transactions of 1800 outputs each, about 61 kB each, under the
        -- largest standard transaction and the largest argument of a command
        forM_ [1 .. 40 :: Int] $ \k ->
          void $ wallet ["sendmany", "", manyOutputs net k 1800]
        h <- lastMined c
        withRegtestNode c [0] $ \t -> do
          p <- waitForPeer t
          _ <- waitForTip c 0 t
          Just [b] <- getBlocks net 30 p [fromString h]
          C8.length (encode b) `shouldSatisfy` (> 2097152)
          length b.txs `shouldBe` 41
