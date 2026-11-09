-- | Peer process talking to a scripted remote over an in-memory connection.
module Haskoin.Node.PeerSpec (spec) where

import Control.Concurrent.Async (ExceptionInLinkedThread (..))
import Control.Monad (forM_)
import Data.ByteString qualified as B
import Data.Serialize (putWord32le, runPut)
import Data.Word (Word32)
import Haskoin
import Haskoin.Node
import Haskoin.Node.Test
import NQE
import Test.HUnit (assertFailure)
import Test.Hspec hiding (pending)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (..), ioProperty)
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

data PeerTest = PeerTest
  { peer :: !Peer,
    events :: !(Inbox PeerEvent),
    remote :: !Remote,
    thread :: !(Async ())
  }

-- | Run a peer process connected to a remote that the test drives.
withPeerTest :: Network -> (PeerTest -> IO a) -> IO a
withPeerTest net f = do
  rv <- newEmptyMVar
  let script r = putMVar rv r >> forever_wait
      forever_wait = threadDelay maxBound >> forever_wait
  withTestLogger $ \logger ->
    withPublisher $ \pub ->
      withSubscription pub $ \events -> do
        (inbox, mailbox) <- newMailbox
        busy <- newTVarIO False
        pending <- newTVarIO 0
        let cfg =
              PeerConfig
                { pub = pub,
                  net = net,
                  label = "remote",
                  connect = memoryConnection net script,
                  logger = logger
                }
            p = wrapPeer cfg busy pending mailbox
        withAsync (peer cfg p inbox) $ \thread -> do
          remote <- within 5 "connection" (readMVar rv)
          f PeerTest {peer = p, events, remote, thread}

-- | Next message received by the peer process from the remote.
received :: PeerTest -> IO Message
received t =
  receiveWithin 5 "message from remote" t.events $ \case
    PeerMessage p m | p == t.peer -> Just m
    _ -> Nothing

-- | The peer process must stop in time with an error that satisfies the
-- predicate.
expectFailure :: PeerTest -> (PeerException -> Bool) -> IO ()
expectFailure t ok =
  within 5 "peer process to stop" (waitCatch t.thread) >>= \case
    Right () -> assertFailure "Peer process stopped without error"
    Left e -> case peerException e of
      Just pe | ok pe -> return ()
      _ -> assertFailure ("Unexpected error: " <> show e)
  where
    peerException e =
      case fromException e of
        Just (ExceptionInLinkedThread _ inner) -> fromException inner
        Nothing -> fromException e

-- | The peer process must stop in time without error.
expectStopped :: PeerTest -> IO ()
expectStopped t =
  within 5 "peer process to stop" (waitCatch t.thread) >>= \case
    Right () -> return ()
    Left e -> assertFailure ("Unexpected error: " <> show e)

expectRunning :: PeerTest -> IO ()
expectRunning t =
  poll t.thread >>= \case
    Nothing -> return ()
    Just r -> assertFailure $ "Peer process stopped: " <> show r

-- | Chop a byte string into pieces of the given sizes.
chunks :: [Int] -> B.ByteString -> [B.ByteString]
chunks _ b | B.null b = []
chunks [] b = [b]
chunks (n : ns) b = let (x, y) = B.splitAt (max 1 n) b in x : chunks ns y

-- | Valid message with the payload length field replaced.
withLength :: Word32 -> B.ByteString -> B.ByteString
withLength len bs = B.take 16 bs <> runPut (putWord32le len) <> B.drop 20 bs

spec :: Spec
spec = do
  let net = btcRegTest
  t0 <- runIO nowTimestamp
  let blocks = mkBlocks net 0 net.genesisHeader t0 5
      b0 = blocks !! 0
      bhash :: Block -> BlockHash
      bhash b = headerHash b.header
      block_iv :: Block -> InvVector
      block_iv b = InvVector InvWitnessBlock (bhash b).get
  describe "peer message framing" $ do
    it "parses messages split across arbitrary chunks" $
      withPeerTest net $ \t -> do
        let bs = encodeMessage net (MPing (Ping 42))
        forM_ (chunks (repeat 1) bs) (remoteSendRaw t.remote)
        received t `shouldReturn` MPing (Ping 42)

    prop "parses messages regardless of how they are chunked" $
      \(sizes :: [Positive Int]) -> ioProperty $
        withPeerTest net $ \t -> do
          let msgs = [MPing (Ping 1), MBlock b0, MPong (Pong 2)]
              bs = B.concat (map (encodeMessage net) msgs)
          forM_ (chunks (map getPositive sizes) bs) (remoteSendRaw t.remote)
          mapM (const (received t)) msgs `shouldReturn` msgs

    it "parses several messages sent at once" $
      withPeerTest net $ \t -> do
        let msgs = [MPing (Ping 1), MVerAck, MPing (Ping 2)]
        remoteSendRaw t.remote (B.concat (map (encodeMessage net) msgs))
        mapM (const (received t)) msgs `shouldReturn` msgs

    it "accepts unknown commands" $ withPeerTest net $ \t -> do
      remoteSend t.remote (MOther "xyzzy" "\1\2\3")
      remoteSend t.remote (MPing (Ping 7))
      received t `shouldReturn` MOther "xyzzy" "\1\2\3"
      received t `shouldReturn` MPing (Ping 7)

    it "sends messages to the remote" $ withPeerTest net $ \t -> do
      MPing (Ping 9) `sendMessage` t.peer
      remoteReceiveMatch 5 "ping" t.remote Just `shouldReturn` MPing (Ping 9)

  describe "peer protocol violations" $ do
    it "disconnects on wrong network magic" $ withPeerTest net $ \t -> do
      remoteSendRaw t.remote (encodeMessage btc (MPing (Ping 1)))
      expectFailure t $ \case
        WrongNetwork m -> m == btc.magic
        _ -> False

    it "disconnects on oversized message" $ withPeerTest net $ \t -> do
      -- send only the header: the peer must not wait for the payload
      let bs = encodeMessage net (MBlock b0)
      remoteSendRaw t.remote (B.take 24 (withLength (33 * 1024 * 1024) bs))
      expectFailure t $ \case
        MessageTooLarge MCBlock _ -> True
        _ -> False

    it "disconnects on bad checksum" $ withPeerTest net $ \t -> do
      let bs = encodeMessage net (MPing (Ping 1))
          bad = B.take 20 bs <> "\0\0\0\0" <> B.drop 24 bs
      remoteSendRaw t.remote bad
      expectFailure t $ \case
        InvalidPayload MCPing _ -> True
        _ -> False

    it "disconnects on malformed payload" $ withPeerTest net $ \t -> do
      -- a ping payload is eight bytes, send three with a valid checksum
      let payload = "\1\2\3"
          bs = encodeMessage net (MOther "ping" payload)
      remoteSendRaw t.remote bs
      expectFailure t $ \case
        InvalidPayload MCPing _ -> True
        _ -> False

    it "disconnects when the remote closes the connection" $
      withPeerTest net $ \t -> do
        remoteClose t.remote
        expectFailure t $ \case
          ConnectionClosed -> True
          _ -> False

    it "disconnects when told to" $ withPeerTest net $ \t -> do
      expectRunning t
      killPeer t.peer
      expectStopped t
      eventually 5 "connection closed" (readTVarIO t.remote.closed)

  describe "peer data requests" $ do
    let serve :: PeerTest -> (Message -> [Message]) -> IO a -> IO a
        serve t f =
          withAsync (remote_loop t f) . const
        remote_loop :: PeerTest -> (Message -> [Message]) -> IO ()
        remote_loop t f =
          remoteReceive t.remote >>= \case
            Nothing -> return ()
            Just m -> mapM_ (remoteSend t.remote) (f m) >> remote_loop t f
        pong (MPing (Ping n)) = [MPong (Pong n)]
        pong _ = []
        honest = \case
          MGetData (GetData ivs) ->
            [MBlock b | iv <- ivs, b <- blocks, block_iv b == iv]
          m -> pong m

    it "downloads blocks" $ withPeerTest net $ \t -> serve t honest $ do
      r <- getBlocks net 5 t.peer (map bhash (take 3 blocks))
      r `shouldBe` Just (take 3 blocks)

    it "ignores unrelated messages while downloading" $
      withPeerTest net $ \t -> do
        let noisy = \case
              MGetData (GetData ivs) ->
                concat
                  [ [MBlock b, MPing (Ping 5), MInv (Inv [block_iv b])]
                  | iv <- ivs,
                    b <- blocks,
                    block_iv b == iv
                  ]
              m -> pong m
        serve t noisy $ do
          r <- getBlocks net 5 t.peer (map bhash (take 3 blocks))
          r `shouldBe` Just (take 3 blocks)

    it "fails when a requested block is not found" $
      withPeerTest net $ \t -> do
        let f = \case
              MGetData (GetData (iv : _)) -> [MNotFound (NotFound [iv])]
              m -> pong m
        serve t f $
          getBlocks net 5 t.peer [bhash b0] `shouldReturn` Nothing

    it "times out when the peer does not answer" $
      withPeerTest net $ \t ->
        getBlocks net 1 t.peer [bhash b0] `shouldReturn` Nothing

    it "downloads transactions" $ withPeerTest net $ \t -> do
      let txs = concatMap (.txs) blocks
          tx_iv tx = InvVector InvWitnessTx (txHash tx).get
          f = \case
            MGetData (GetData ivs) ->
              [MTx tx | iv <- ivs, tx <- txs, tx_iv tx == iv]
            m -> pong m
      serve t f $
        getTxs net 5 t.peer (map txHash (take 2 txs))
          `shouldReturn` Just (take 2 txs)

    it "pings the peer" $ withPeerTest net $ \t -> do
      serve t pong $ pingPeer 5 t.peer `shouldReturn` True

    it "reports an unanswered ping" $ withPeerTest net $ \t ->
      pingPeer 1 t.peer `shouldReturn` False
