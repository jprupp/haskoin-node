-- | Full node connected to scripted remote peers over in-memory connections.
module Haskoin.Node.PeerMgrSpec (spec) where

import Control.Monad (forever, void, when)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Haskoin
import Haskoin.Node
import Haskoin.Node.Test
import Network.Socket (SockAddr (..))
import System.Random (randomIO)
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

-- | Node connected to a single remote running the given script. Each
-- connection attempt runs the script again with the attempt number.
withScriptedNode ::
  Network ->
  (NodeConfig -> NodeConfig) ->
  (Int -> Remote -> IO ()) ->
  (TestNode -> IO a) ->
  IO a
withScriptedNode net modify script f = do
  attempts <- newTVarIO (0 :: Int)
  let connect :: SockAddr -> WithConnection
      connect _ g = do
        n <- atomically $ stateTVar attempts (\n -> (n + 1, n + 1))
        memoryConnection net (script n) g
  withTestNode net ["127.0.0.1:1"] connect modify f

-- | Remote that sends the version and then runs the handler on each
-- message.
handshakeThen :: Version -> (Message -> IO ()) -> Remote -> IO ()
handshakeThen v react r = do
  remoteSend r (MVersion v)
  let go =
        remoteReceive r >>= \case
          Nothing -> return ()
          Just m -> react m >> go
  go

noPeerConnectedWithin :: Double -> TestNode -> IO ()
noPeerConnectedWithin s t =
  expectNothingWithin s "peer connection" t.events $ \case
    PeerEvent (PeerConnected _) -> Just ()
    _ -> Nothing

spec :: Spec
spec = do
  let net = bchRegTest
  t0 <- runIO $ subtract 3000 <$> nowTimestamp
  let main = mkBlocks net 0 net.genesisHeader t0 2500

  describe "node handshake" $ do
    it "connects to a full node" $ do
      blocks <- newTVarIO []
      withScriptedNode net id (const (honestRemote blocks)) $ \t -> do
        p <- waitForPeer t
        Just o <- getOnlinePeer t.node.peerMgr p
        fmap (.version) o.version `shouldBe` Just (fromIntegral myVersion)
        o.online `shouldBe` True
        map (.mailbox) <$> getPeers t.node.peerMgr `shouldReturn` [p]

    it "rejects a peer that is not a full node" $ do
      v <- remoteVersion net 0 =<< randomIO
      withScriptedNode net id (\_ -> handshakeThen v (const (return ()))) $
        noPeerConnectedWithin 2

    it "rejects a connection to itself" $ do
      -- reply with the nonce from our own version message
      let script _ r =
            remoteReceive r >>= \case
              Just (MVersion ours) -> do
                v <- remoteVersion net nodeNetwork ours.nonce
                remoteSend r (MVersion v)
                remoteSend r MVerAck
                void (remoteReceive r)
              _ -> return ()
      withScriptedNode net id script $ noPeerConnectedWithin 2

    it "disconnects a peer that sends its version twice" $ do
      v <- remoteVersion net nodeNetwork =<< randomIO
      let script n r = handshakeThen v (react n r) r
          react n r = \case
            MVersion _ -> do
              remoteSend r MVerAck
              when (n == 1) $ remoteSend r (MVersion v)
            _ -> return ()
      withScriptedNode net id script $ \t -> do
        p <- waitForPeer t
        waitForDisconnect t p

    it "disconnects a peer that does not complete the handshake" $ do
      remotes <- newEmptyMVar
      let script _ r = do
            putMVar remotes r
            void (remoteReceive r) -- our version
            void (remoteReceive r) -- wait until closed
      withScriptedNode net (setTimeout 1) script $ \t -> do
        r <- within 10 "connection" (readMVar remotes)
        eventually 10 "connection closed" (readTVarIO r.closed)
        noPeerConnectedWithin 0.5 t

    it "disconnects a peer that chatters without completing the handshake" $ do
      remotes <- newEmptyMVar
      let script _ r = do
            void (tryPutMVar remotes r)
            -- never sends a version, but keeps talking
            forever $ remoteSend r (MPing (Ping 1)) >> threadDelay 200000
      withScriptedNode net (setTimeout 1) script $ \t -> do
        r <- within 10 "connection" (readMVar remotes)
        eventually 10 "connection closed" (readTVarIO r.closed)
        noPeerConnectedWithin 0.5 t

    it "waits before retrying a peer that failed" $ do
      times <- newTVarIO []
      let script _ _ = getCurrentTime >>= \now -> atomically (modifyTVar' times (now :))
      withScriptedNode net id script $ \_ -> do
        eventually 20 "two attempts" $ (>= 2) . length <$> readTVarIO times
        -- the first failure waits two seconds; the connect loop alone could
        -- retry within a tenth of a second
        oldestFirst <- reverse <$> readTVarIO times
        case oldestFirst of
          first : second : _ ->
            (second `diffUTCTime` first) `shouldSatisfy` (>= 1.9)
          _ -> expectationFailure "fewer than two attempts"

  describe "node peer maintenance" $ do
    it "answers pings" $ do
      blocks <- newTVarIO []
      pongs <- newEmptyMVar
      let script _ r = do
            v <- remoteVersion net nodeNetwork =<< randomIO
            remoteSend r (MVersion v)
            remoteSend r (MPing (Ping 1234))
            let go =
                  remoteReceive r >>= \case
                    Nothing -> return ()
                    Just (MPong (Pong n)) -> putMVar pongs n >> go
                    Just m -> respondHonestly r blocks m >> go
            go
      withScriptedNode net id script $ \_ ->
        within 10 "pong" (takeMVar pongs) `shouldReturn` 1234

    it "disconnects a peer that stops responding" $ do
      v <- remoteVersion net nodeNetwork =<< randomIO
      let react r = \case
            MVersion _ -> remoteSend r MVerAck
            _ -> return () -- never answers pings
          script _ r = handshakeThen v (react r) r
      withScriptedNode net (setTimeout 2) script $ \t -> do
        p <- waitForPeer t
        waitForDisconnect t p

    it "disconnects a peer that has been connected for too long" $ do
      blocks <- newTVarIO []
      withScriptedNode
        net
        (setMaxPeerLife 1 . setTimeout 2)
        (const (honestRemote blocks))
        $ \t -> do
          p <- waitForPeer t
          waitForDisconnect t p

    it "reconnects after a peer disconnects" $ do
      blocks <- newTVarIO []
      let script n r
            | n == 1 = do
                v <- remoteVersion net nodeNetwork =<< randomIO
                remoteSend r (MVersion v)
                Just (MVersion _) <- remoteReceive r
                remoteSend r MVerAck
                Just MVerAck <- remoteReceive r
                -- The chain asks for headers once the peer is announced, so
                -- closing after that request cannot beat the announcement.
                remoteReceiveMatch 10 "header request" r $ \case
                  MGetHeaders _ -> Just ()
                  _ -> Nothing
                return () -- close the connection
            | otherwise = honestRemote blocks r
      withScriptedNode net id script $ \t -> do
        p1 <- waitForPeer t
        waitForDisconnect t p1
        p2 <- waitForPeer t
        p2 `shouldNotBe` p1

  describe "node header sync" $ do
    it "syncs headers from a peer" $ do
      blocks <- newTVarIO main
      withScriptedNode net id (const (honestRemote blocks)) $ \t -> do
        bn <- receiveWithin 30 "synced" t.events $ \case
          ChainEvent (ChainSynced bn) -> Just bn
          _ -> Nothing
        headerHash bn.header `shouldBe` headerHash (last main).header

    it "follows blocks announced by inventory" $ do
      blocks <- newTVarIO (take 10 main)
      remotes <- newEmptyMVar
      let script _ r = putMVar remotes r >> honestRemote blocks r
      withScriptedNode net id script $ \t -> do
        r <- within 10 "remote" (readMVar remotes)
        _ <- waitForBest 30 t ((== 10) . (.height))
        atomically $ writeTVar blocks (take 109 main)
        let tip = headerHash (main !! 108).header
        remoteSend r (MInv (Inv [InvVector InvBlock tip.get]))
        void $ waitForBest 30 t ((== 109) . (.height))

    it "follows blocks announced by headers that do not connect" $ do
      blocks <- newTVarIO (take 10 main)
      remotes <- newEmptyMVar
      let script _ r = putMVar remotes r >> honestRemote blocks r
      withScriptedNode net id script $ \t -> do
        r <- within 10 "remote" (readMVar remotes)
        p <- waitForPeer t
        _ <- waitForBest 30 t ((== 10) . (.height))
        atomically $ writeTVar blocks (take 60 main)
        remoteSend r (headersMessage [(main !! 59).header])
        _ <- waitForBest 30 t ((== 60) . (.height))
        expectNothingWithin 0.5 "disconnection" t.events $ \case
          PeerEvent (PeerDisconnected p') | p == p' -> Just ()
          _ -> Nothing

    it "downloads blocks from a peer" $ do
      blocks <- newTVarIO main
      withScriptedNode net id (const (honestRemote blocks)) $ \t -> do
        p <- waitForPeer t
        let want = take 3 (drop 100 main)
        getBlocks net 10 p (map (headerHash . (.header)) want)
          `shouldReturn` Just want
        let unknown = case mkBlocks net 7 net.genesisHeader t0 1 of
              b : _ -> headerHash b.header
              [] -> error "no block"
        getBlocks net 10 p [unknown] `shouldReturn` Nothing
