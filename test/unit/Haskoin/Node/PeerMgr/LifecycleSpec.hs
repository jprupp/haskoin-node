module Haskoin.Node.PeerMgr.LifecycleSpec (spec) where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Haskoin (Version, btcRegTest, nodeNetwork)
import Haskoin.Node (buildVersion)
import Haskoin.Node.PeerMgr.Handshake
import Haskoin.Node.PeerMgr.Lifecycle
import Haskoin.Node.Test (zeroAddress)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

-- | Seconds after the connection opened.
at :: Double -> UTCTime
at s = realToFrac s `addUTCTime` t0

version :: Version
version = buildVersion btcRegTest 7 0 (zeroAddress nodeNetwork) (zeroAddress 0) 0

-- | A ten second timeout and a hundred second maximum life.
limits :: Limits
limits = Limits {timeout = 10, maxLife = 100}

-- | Feed inputs to a new connection, collecting the actions.
run :: [PeerInput] -> (PeerState, [PeerAction])
run = feed (newPeerState t0)

-- | Feed inputs to a connection, collecting the actions.
feed :: PeerState -> [PeerInput] -> (PeerState, [PeerAction])
feed st0 = foldl' go (st0, [])
  where
    go (st, acc) i = let (st', as) = stepPeer limits i st in (st', acc <> as)

-- | A connection that completed the handshake.
online :: [PeerInput]
online = [GotHandshake (GotVersion version), GotHandshake GotVerAck]

-- | A random input.
newtype Input = Input PeerInput
  deriving stock (Show)

instance Arbitrary Input where
  arbitrary =
    Input
      <$> oneof
        [ Heard <$> time,
          GotHandshake <$> elements [GotVersion version, GotVerAck],
          Refused <$> elements [NotFullNode, ConnectedToSelf],
          GotPong <$> time <*> nonce,
          Tick <$> time <*> nonce
        ]
    where
      time = at <$> choose (0, 200)
      nonce = chooseBoundedIntegral (1, 3)

spec :: Spec
spec = describe "connection lifecycle" $ do
  describe "handshake" $ do
    it "goes online and announces the peer" $ do
      let (st, actions) = run online
      isOnline st `shouldBe` True
      st.announced `shouldBe` True
      stageVersion st `shouldBe` Just version
      actions `shouldBe` [ReplyVerAck, Announce]

    it "accepts the verack first" $
      snd (run [GotHandshake GotVerAck, GotHandshake (GotVersion version)])
        `shouldBe` [ReplyVerAck, Announce]

    it "disconnects a peer that repeats a handshake message once online" $
      snd (run (online <> [GotHandshake GotVerAck]))
        `shouldBe` [ReplyVerAck, Announce, Disconnect (Rejected DuplicateVerAck)]

    it "disconnects a peer whose version was refused" $ do
      let (st, actions) = run [Refused ConnectedToSelf]
      actions `shouldBe` [Disconnect (Rejected ConnectedToSelf)]
      st.stage `shouldBe` Disconnecting (Rejected ConnectedToSelf)

    it "disconnects a peer that does not complete the handshake in time" $
      -- chatter keeps the peer from being silent, but not from timing out
      snd (run [GotHandshake GotVerAck, Heard (at 9), Tick (at 11) 1])
        `shouldBe` [Disconnect HandshakeTimeout]

  describe "pings" $ do
    it "pings an online peer on a tick" $ do
      let (st, actions) = run (online <> [Tick (at 1) 5])
      drop 2 actions `shouldBe` [SendPing 5]
      pendingPing st `shouldBe` Just (at 1, 5)

    it "pings once until the pong arrives" $
      drop 2 (snd (run (online <> [Tick (at 1) 5, Tick (at 2) 6])))
        `shouldBe` [SendPing 5]

    it "records the round trip of a matching pong" $ do
      let (st, _) = run (online <> [Tick (at 1) 5, GotPong (at 1.5) 5])
      pendingPing st `shouldBe` Nothing
      st.pings `shouldBe` [0.5]

    it "ignores a pong with another nonce" $ do
      let (st, _) = run (online <> [Tick (at 1) 5, GotPong (at 1.5) 6])
      pendingPing st `shouldBe` Just (at 1, 5)
      st.pings `shouldBe` []

    it "keeps the last eleven round trips" $ do
      -- the pong also counts as hearing from the peer
      let ping i = [Tick (at i) 1, GotPong (at (i + i / 100)) 1, Heard (at (i + i / 100))]
          (st, _) = run (online <> concatMap ping [1 .. 15])
      -- in milliseconds, so that rounding in the times does not matter
      map (\d -> round (d * 1000) :: Int) st.pings `shouldBe` [150, 140 .. 50]

    it "disconnects a peer that does not answer a ping in time" $
      -- other traffic does not make up for a missing pong
      drop 3 (snd (run (online <> [Tick (at 1) 5, Heard (at 10), Tick (at 12) 6])))
        `shouldBe` [Disconnect NoPong]

  describe "timers" $ do
    it "disconnects a silent peer" $
      drop 2 (snd (run (online <> [Tick (at 11) 1])))
        `shouldBe` [Disconnect Silent]

    it "rotates a peer connected for too long" $
      drop 2 (snd (run (online <> [Heard (at 100), Tick (at 101) 1])))
        `shouldBe` [Disconnect TooOld]

    it "rotates before checking anything else" $
      snd (run [Tick (at 101) 1]) `shouldBe` [Disconnect TooOld]

  describe "properties" $ do
    prop "ignores everything once disconnecting" $ \(ins :: [Input]) ->
      let inputs = [i | Input i <- ins]
       in let (st, _) = run [Refused NotFullNode]
           in snd (feed st inputs) === []

    prop "announces a peer at most once, and remembers it" $ \(ins :: [Input]) ->
      let inputs = [i | Input i <- ins]
       in let (st, actions) = run inputs
              announces = length (filter (== Announce) actions)
           in announces <= 1 .&&. st.announced === (announces == 1)

    prop "disconnects at most once" $ \(ins :: [Input]) ->
      let inputs = [i | Input i <- ins]
       in let isDisconnect = \case
                Disconnect _ -> True
                _ -> False
           in length (filter isDisconnect (snd (run inputs))) <= 1

    prop "is online exactly when announced and not disconnecting" $ \(ins :: [Input]) ->
      let inputs = [i | Input i <- ins]
       in let (st, _) = run inputs
              disconnecting = case st.stage of
                Disconnecting _ -> True
                _ -> False
           in isOnline st === (st.announced && not disconnecting)
