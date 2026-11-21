-- | Full node connected to scripted remote peers over in-memory connections.
module Haskoin.Node.PeerMgrSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Haskoin
import Haskoin.Node
import Haskoin.Node.Test
import Network.Socket (SockAddr (..))
import System.Random (randomIO)
import Test.Hspec

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
  -- the remote lives in memory; its name is a Unix socket, so that no
  -- address or port of the machine is named
  withTestNode net ["unix:scripted-peer"] connect modify f

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

  describe "node handshake" $ do
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
