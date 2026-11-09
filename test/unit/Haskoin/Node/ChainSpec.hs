{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Header chain process driven by fake peers.
module Haskoin.Node.ChainSpec (spec) where

import Control.Monad (forM_, forever, void)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Database.RocksDB (DB)
import Haskoin
import Haskoin.Node
import Haskoin.Node.Peer.Lock (Lock (..))
import Haskoin.Node.Test
import NQE
import Test.HUnit (assertFailure)
import Test.Hspec hiding (pending)
import UnliftIO

data ChainTest = ChainTest
  { chain :: !Chain,
    events :: !(Inbox ChainEvent),
    peerPub :: !(Publisher PeerEvent),
    net :: !Network
  }

-- | Peer whose outgoing messages are captured instead of sent.
data FakePeer = FakePeer
  { peer :: !Peer,
    inbox :: !(Inbox PeerMessage)
  }

withChainTest :: Network -> NominalDiffTime -> (ChainTest -> IO a) -> IO a
withChainTest net to f = withTestDB $ \db -> withChainOn net to db f

withChainOn ::
  Network -> NominalDiffTime -> DB -> (ChainTest -> IO a) -> IO a
withChainOn net to db f =
  withTestLogger $ \logger ->
    withPublisher $ \pub ->
      withSubscription pub $ \events ->
        withPublisher $ \peerPub ->
          withChain (ChainConfig db Nothing net pub to logger) $ \chain ->
            f ChainTest {chain, events, peerPub, net}

newFakePeer :: ChainTest -> Text -> IO FakePeer
newFakePeer ct label = do
  (inbox, mailbox) <- newMailbox
  busy <- newTVarIO Free
  pending <- newTVarIO 0
  return
    FakePeer
      { peer = Peer {mailbox, pub = ct.peerPub, label, busy, pending},
        inbox
      }

connectPeer :: ChainTest -> Text -> IO FakePeer
connectPeer ct label = do
  fp <- newFakePeer ct label
  chainPeerConnected ct.chain fp.peer
  return fp

describeMessage :: PeerMessage -> String
describeMessage KillPeer = "disconnection"
describeMessage (SendMessage m) = show (commandToString (msgType m))

-- | Next header request sent to the peer, skipping 'MSendHeaders'.
expectGetHeaders :: FakePeer -> IO GetHeaders
expectGetHeaders fp =
  within 10 ("header request to " <> show fp.peer) go
  where
    go =
      receive fp.inbox >>= \case
        SendMessage (MGetHeaders g) -> return g
        SendMessage MSendHeaders -> go
        m -> assertFailure $ "Expected header request, got " <> describeMessage m

expectSendHeaders :: FakePeer -> IO ()
expectSendHeaders fp =
  receiveWithin 10 "sendheaders" fp.inbox $ \case
    SendMessage MSendHeaders -> Just ()
    _ -> Nothing

expectKilled :: FakePeer -> IO ()
expectKilled fp =
  receiveWithin 10 ("disconnection of " <> show fp.peer) fp.inbox $ \case
    KillPeer -> Just ()
    _ -> Nothing

-- | Fail if the peer is asked for headers or disconnected in time window.
expectNoRequest :: Double -> FakePeer -> IO ()
expectNoRequest s fp =
  expectNothingWithin s ("request to " <> show fp.peer) fp.inbox $ \case
    SendMessage (MGetHeaders _) -> Just ()
    KillPeer -> Just ()
    _ -> Nothing

sendHeaders :: ChainTest -> FakePeer -> [BlockHeader] -> IO ()
sendHeaders ct fp = chainHeaders ct.chain fp.peer

-- | Wait for a header request and answer it from the provided chain.
answer :: ChainTest -> FakePeer -> [BlockHeader] -> IO [BlockHeader]
answer ct fp hs = do
  g <- expectGetHeaders fp
  let r = serveHeaders ct.net hs g
  sendHeaders ct fp r
  return r

-- | Answer all header requests from the chain in the variable. Counts the
-- requests.
withServer ::
  ChainTest -> FakePeer -> TVar [BlockHeader] -> (TVar Int -> IO a) -> IO a
withServer ct fp var f = do
  count <- newTVarIO 0
  let go = forever $ do
        g <- receiveMatch fp.inbox $ \case
          SendMessage (MGetHeaders g) -> Just g
          _ -> Nothing
        atomically $ modifyTVar count (+ 1)
        hs <- readTVarIO var
        sendHeaders ct fp (serveHeaders ct.net hs g)
  withAsync go $ \a -> link a >> f count

waitHeight :: ChainTest -> BlockHeight -> IO BlockNode
waitHeight ct h = do
  eventually 30 ("best height " <> show h) $
    (== h) . (.height) <$> chainGetBest ct.chain
  chainGetBest ct.chain

bestHash :: ChainTest -> IO BlockHash
bestHash ct = headerHash . (.header) <$> chainGetBest ct.chain

-- | Sync a fresh peer to the provided chain.
syncPeer :: ChainTest -> Text -> [BlockHeader] -> IO FakePeer
syncPeer ct label hs = do
  fp <- connectPeer ct label
  _ <- answer ct fp hs
  _ <- waitHeight ct (fromIntegral (length hs))
  expectSendHeaders fp
  return fp

-- | The header with the first nonce for which its hash is above its target.
-- Only its proof of work is wrong: its bits, time and parent are those of a
-- valid header.
invalidPow :: BlockHeader -> BlockHeader
invalidPow h =
  case [h' | n <- [0 ..], let h' = withNonce n, blockPOW (headerHash h') > target] of
    h' : _ -> h'
    [] -> h
  where
    (target, _) = decodeCompact h.bits
    withNonce n =
      BlockHeader
        { version = h.version,
          prev = h.prev,
          merkle = h.merkle,
          timestamp = h.timestamp,
          bits = h.bits,
          nonce = n
        }

spec :: Spec
spec = forM_ [btcRegTest, bchRegTest] $ \net ->
  describe ("header chain on " <> net.name) $ do
    t0 <- runIO $ subtract 6000 <$> nowTimestamp
    let main = mkHeaders net 0 net.genesisHeader t0 4500
        short = take 20 main
        hashAt i = headerHash (main !! (i - 1))
        nodeHash = headerHash . (.header)
        fast = withChainTest net 1
        slow = withChainTest net 600

    describe "initial sync" $ do
      it "requests headers in batches until the peer runs out" $ slow $ \ct -> do
        a <- connectPeer ct "a"
        g1 <- expectGetHeaders a
        take 1 g1.locator `shouldBe` [headerHash net.genesisHeader]
        sendHeaders ct a (serveHeaders net main g1)
        g2 <- expectGetHeaders a
        take 1 g2.locator `shouldBe` [hashAt 2000]
        sendHeaders ct a (serveHeaders net main g2)
        g3 <- expectGetHeaders a
        take 1 g3.locator `shouldBe` [hashAt 4000]
        r3 <- return (serveHeaders net main g3)
        length r3 `shouldBe` 500
        sendHeaders ct a r3
        expectSendHeaders a
        expectNoRequest 0.5 a
        best <- waitHeight ct 4500
        nodeHash best `shouldBe` hashAt 4500

      it "publishes best block and synced events once" $ slow $ \ct -> do
        a <- connectPeer ct "a"
        var <- newTVarIO main
        withServer ct a var $ \count -> do
          bn <- receiveWithin 30 "synced event" ct.events $ \case
            ChainSynced bn -> Just bn
            _ -> Nothing
          bn.height `shouldBe` 4500
          chainIsSynced ct.chain `shouldReturn` True
          readTVarIO count `shouldReturn` 3
          expectNothingWithin 0.5 "second synced event" ct.events $ \case
            ChainSynced _ -> Just ()
            _ -> Nothing

      it "does not consider old headers synced" $ slow $ \ct -> do
        let old = mkHeaders net 0 net.genesisHeader 1500000000 10
        _ <- syncPeer ct "a" old
        chainIsSynced ct.chain `shouldReturn` False

      it "stops asking a peer that sends no headers" $ slow $ \ct -> do
        a <- connectPeer ct "a"
        _ <- expectGetHeaders a
        sendHeaders ct a []
        expectSendHeaders a
        expectNoRequest 1 a

      it "syncs against every peer that connects" $ slow $ \ct -> do
        _ <- syncPeer ct "a" (take 10 main)
        b <- connectPeer ct "b"
        _ <- answer ct b (take 30 main)
        void $ waitHeight ct 30

      it "ignores duplicate connection events" $ slow $ \ct -> do
        a <- connectPeer ct "a"
        chainPeerConnected ct.chain a.peer
        _ <- answer ct a short
        expectSendHeaders a
        expectNoRequest 1 a

      it "keeps headers across restarts" $ withTestDB $ \db -> do
        withChainOn net 600 db $ \ct -> void $ syncPeer ct "a" short
        withChainOn net 600 db $ \ct -> do
          bn <- receiveWithin 10 "best block" ct.events $ \case
            ChainBestBlock bn -> Just bn
            _ -> Nothing
          bn.height `shouldBe` 20
          bestHash ct `shouldReturn` hashAt 20

    describe "announcements" $ do
      it "requests headers when an unknown block is announced by inventory" $
        slow $ \ct -> do
          a <- syncPeer ct "a" (take 10 main)
          chainBlocksAnnounced ct.chain a.peer [hashAt 109]
          _ <- answer ct a (take 109 main)
          void $ waitHeight ct 109

      it "ignores inventory announcements of known blocks" $ slow $ \ct -> do
        a <- syncPeer ct "a" (take 10 main)
        chainBlocksAnnounced ct.chain a.peer [hashAt 5, hashAt 10]
        expectNoRequest 1 a

      it "imports announced headers that connect" $ slow $ \ct -> do
        a <- syncPeer ct "a" (take 10 main)
        sendHeaders ct a [main !! 10]
        _ <- waitHeight ct 11
        expectNoRequest 0.5 a

      it "requests headers when announced headers do not connect" $
        slow $ \ct -> do
          a <- syncPeer ct "a" (take 10 main)
          sendHeaders ct a (take 5 (drop 49 main))
          _ <- answer ct a (take 54 main)
          void $ waitHeight ct 54

      it "continues syncing a peer that sends a full batch unsolicited" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          _ <- expectGetHeaders a
          sendHeaders ct b (take 2000 main)
          _ <- waitHeight ct 2000
          sendHeaders ct a []
          g <- expectGetHeaders b
          take 1 g.locator `shouldBe` [hashAt 2000]

      it "asks a peer that announces an unknown block while another peer syncs" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          -- a holds the turn and never answers
          _ <- expectGetHeaders a
          chainBlocksAnnounced ct.chain b.peer [hashAt 100]
          g <- expectGetHeaders b
          take 1 g.locator `shouldBe` [headerHash net.genesisHeader]

      it "asks an announcing peer only once until it answers" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          _ <- expectGetHeaders a
          chainBlocksAnnounced ct.chain b.peer [hashAt 100]
          _ <- expectGetHeaders b
          chainBlocksAnnounced ct.chain b.peer [hashAt 101]
          sendHeaders ct b (take 5 (drop 49 main))
          expectNoRequest 1 b
          -- headers that connect answer the request
          sendHeaders ct b (take 5 main)
          _ <- waitHeight ct 5
          chainBlocksAnnounced ct.chain b.peer [hashAt 102]
          void $ expectGetHeaders b

      it "keeps syncing peer when another peer announces headers" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          g <- expectGetHeaders a
          sendHeaders ct b (take 5 main)
          _ <- waitHeight ct 5
          sendHeaders ct a (serveHeaders net short g)
          _ <- waitHeight ct 20
          expectSendHeaders a
          -- b gets its turn once a is done
          void $ answer ct b (take 5 main)

      it "ignores messages from peers that are not connected" $
        slow $ \ct -> do
          x <- newFakePeer ct "x"
          sendHeaders ct x (take 5 main)
          chainBlocksAnnounced ct.chain x.peer [hashAt 5]
          expectNoRequest 1 x
          (.height) <$> chainGetBest ct.chain `shouldReturn` 0

    describe "reorganizations" $ do
      let fork n = mkHeaders net 1 (main !! 14) (t0 + 100) n
      it "keeps the first chain it saw when a fork has the same work" $ slow $ \ct -> do
        a <- syncPeer ct "a" short
        -- five headers on block 15 reach height 20, with the same work
        sendHeaders ct a (fork 5)
        let tip = headerHash (last (fork 5))
        eventually 10 "fork stored" $
          (/= Nothing) <$> chainGetBlock ct.chain tip
        bestHash ct `shouldReturn` hashAt 20

      it "switches to a fork with more work" $ slow $ \ct -> do
        a <- syncPeer ct "a" short
        sendHeaders ct a (fork 10)
        best <- waitHeight ct 25
        nodeHash best `shouldBe` headerHash (last (fork 10))
        chainBlockMain ct.chain (hashAt 20) `shouldReturn` False
        chainBlockMain ct.chain (hashAt 15) `shouldReturn` True
        Just old <- chainGetBlock ct.chain (hashAt 20)
        split <- chainGetSplitBlock ct.chain old best
        nodeHash split `shouldBe` hashAt 15

      it "stays on the chain with more work" $ slow $ \ct -> do
        a <- syncPeer ct "a" short
        sendHeaders ct a (fork 3)
        let tip = headerHash (last (fork 3))
        eventually 10 "fork stored" $
          (/= Nothing) <$> chainGetBlock ct.chain tip
        bestHash ct `shouldReturn` hashAt 20
        chainBlockMain ct.chain tip `shouldReturn` False

    describe "misbehaving peers" $ do
      it "disconnects a peer that sends headers with invalid proof of work" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          _ <- expectGetHeaders a
          sendHeaders ct a [invalidPow (main !! 0)]
          expectKilled a

      it "publishes the best block when a batch fails after valid headers" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          _ <- expectGetHeaders a
          sendHeaders ct a (take 5 main <> [invalidPow (main !! 5)])
          expectKilled a
          bn <- receiveWithin 10 "best block at height 5" ct.events $ \case
            ChainBestBlock bn | bn.height == 5 -> Just bn
            _ -> Nothing
          nodeHash bn `shouldBe` hashAt 5

      it "ends the turn of a peer whose full batch brings no new header" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          _ <- answer ct a main
          _ <- waitHeight ct 2000
          -- asked for the headers after 2000, a sends the first 2000 again
          _ <- expectGetHeaders a
          sendHeaders ct a (take 2000 main)
          expectSendHeaders a
          g <- expectGetHeaders b
          take 1 g.locator `shouldBe` [hashAt 2000]

      it "disconnects a peer that sends headers that do not form a chain" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          _ <- expectGetHeaders a
          sendHeaders ct a [main !! 0, main !! 2]
          expectKilled a

      it "syncs from another peer after disconnecting a misbehaving one" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          _ <- expectGetHeaders a
          sendHeaders ct a [invalidPow (main !! 0)]
          expectKilled a
          _ <- answer ct b short
          void $ waitHeight ct 20

      it "disconnects an unresponsive syncing peer and moves on" $
        fast $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          _ <- expectGetHeaders a
          expectKilled a
          _ <- answer ct b short
          void $ waitHeight ct 20

      it "moves on when the syncing peer disconnects" $ slow $ \ct -> do
        a <- connectPeer ct "a"
        b <- connectPeer ct "b"
        _ <- expectGetHeaders a
        chainPeerDisconnected ct.chain a.peer
        _ <- answer ct b short
        void $ waitHeight ct 20

      it "never asks a peer for headers after it disconnects" $
        slow $ \ct -> do
          a <- connectPeer ct "a"
          b <- connectPeer ct "b"
          _ <- expectGetHeaders a
          -- queue a again while it is syncing
          chainBlocksAnnounced ct.chain a.peer [hashAt 5]
          chainPeerDisconnected ct.chain a.peer
          _ <- answer ct b short
          _ <- waitHeight ct 20
          expectNoRequest 1 a

      it "waits for a peer reserved by someone else" $ fast $ \ct -> do
        other <- newOwner "other"
        a <- newFakePeer ct "a"
        True <- reservePeer other a.peer
        chainPeerConnected ct.chain a.peer
        expectNoRequest 1 a
        True <- releasePeer other a.peer
        void $ answer ct a short

      it "keeps its reservation when others try to release the peer" $
        slow $ \ct -> do
          other <- newOwner "other"
          a <- connectPeer ct "a"
          _ <- expectGetHeaders a
          False <- releasePeer other a.peer
          setFree a.peer
          fmap ownerName <$> peerHolder a.peer `shouldReturn` Just "Chain"
          False <- reservePeer other a.peer
          sendHeaders ct a []
          eventually 10 "peer released" $ (== Nothing) . fmap ownerName <$> peerHolder a.peer

    describe "stall recovery" $ do
      it "asks peers for headers when nothing happens for a while" $
        fast $ \ct -> do
          a <- syncPeer ct "a" (take 10 main)
          -- the peer finds blocks but never announces them
          var <- newTVarIO (take 30 main)
          withServer ct a var $ \_ -> void (waitHeight ct 30)

      it "recovers when the syncing peer stops responding" $ fast $ \ct -> do
        a <- connectPeer ct "a"
        _ <- expectGetHeaders a
        expectKilled a
        chainPeerDisconnected ct.chain a.peer
        b <- connectPeer ct "b"
        void $ answer ct b short

    describe "queries" $ do
      it "answers questions about the chain" $ slow $ \ct -> do
        _ <- syncPeer ct "a" short
        tip <- chainGetBest ct.chain
        Just anc <- chainGetAncestor ct.chain 10 tip
        nodeHash anc `shouldBe` hashAt 10
        ps <- chainGetParents ct.chain 15 tip
        map (.height) ps `shouldBe` [15 .. 19]
        map nodeHash ps `shouldBe` map hashAt [15 .. 19]
        chainGetBlock ct.chain (headerHash (main !! 100)) `shouldReturn` Nothing
        chainBlockMain ct.chain (headerHash (main !! 100)) `shouldReturn` False
        chainBlockMain ct.chain (hashAt 20) `shouldReturn` True
        Just n5 <- chainGetBlock ct.chain (hashAt 5)
        split <- chainGetSplitBlock ct.chain n5 tip
        nodeHash split `shouldBe` hashAt 5
