-- | Test the library against Bitcoin Core and Bitcoin Cash Node in regtest
-- mode. Nodes run in Podman containers built from the Containerfile in this
-- directory. Set HASKOIN_NODE_SKIP_REGTEST to skip.
module Main (main) where

import Control.Monad (forM_, replicateM_, void)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.String (fromString)
import Haskoin
import Haskoin.Node
import Haskoin.Node.Test
import Regtest.Podman
import System.Environment (lookupEnv)
import Test.Hspec

main :: IO ()
main = do
  skip <- isJust <$> lookupEnv "HASKOIN_NODE_SKIP_REGTEST"
  available <- podmanAvailable
  if
    | skip -> hspec $ it "regtest" $ pendingWith "HASKOIN_NODE_SKIP_REGTEST set"
    | not available -> hspec $ it "regtest" $ pendingWith "podman not available"
    | otherwise -> buildImage >> hspec spec

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

spec :: Spec
spec = forM_ [BitcoinCore, BitcoinCashNode] $ \impl ->
  describe (implName impl) $ do
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
