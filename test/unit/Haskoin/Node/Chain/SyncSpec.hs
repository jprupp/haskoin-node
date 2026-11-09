module Haskoin.Node.Chain.SyncSpec (spec) where

import Data.List (nub)
import Data.Maybe (isNothing)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Haskoin (BlockHeader (..), BlockNode (..), Network (..), btcRegTest, headerHash)
import Haskoin.Node.Chain.Sync
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

at :: Int -> UTCTime
at n = fromIntegral n `addUTCTime` t0

-- | Transitions applied to a few peers.
data Op
  = Connect Int
  | Disconnect Int
  | Request Int
  | Start Int
  | Refresh Int
  | Finish Int
  | Drop Int
  | Poll
  | Ask Int
  | Answer Int
  deriving stock (Show)

instance Arbitrary Op where
  arbitrary =
    oneof
      [ Connect <$> peer,
        Disconnect <$> peer,
        Request <$> peer,
        Start <$> peer,
        Refresh <$> peer,
        Finish <$> peer,
        Drop <$> peer,
        pure Poll,
        Ask <$> peer,
        Answer <$> peer
      ]
    where
      peer = chooseInt (1, 4)

apply :: SyncState Int -> Op -> SyncState Int
apply s = \case
  Connect p -> peerConnected p s
  Disconnect p -> peerDisconnected p s
  Request p -> requestSync p s
  Start p -> startSync (at 1) (at 1000) p s
  Refresh p -> refreshSync (at 2) p s
  Finish p -> finishSync p s
  Drop p -> dropPeer p s
  Poll -> pollPeers (at 3) s
  Ask p -> askHeaders (at 4) p s
  Answer p -> gotHeaders p s

states :: [Op] -> [SyncState Int]
states = scanl apply (emptySync t0)

run :: [Op] -> SyncState Int
run = foldl' apply (emptySync t0)

statuses :: SyncState Int -> [PeerStatus]
statuses s = [st | p <- [1 .. 4], Just st <- [peerStatus p s]]

isSyncingStatus :: PeerStatus -> Bool
isSyncingStatus = \case
  Syncing _ _ -> True
  _ -> False

spec :: Spec
spec = describe "header sync state" $ do
  it "queues peers in the order they connect" $
    queuedPeers (run [Connect 1, Connect 2, Connect 3]) `shouldBe` [1, 2, 3]

  it "syncs against one peer at a time" $ do
    let s = run [Connect 1, Connect 2, Start 1, Start 2]
    syncingPeer s `shouldBe` Just (1, at 1)
    queuedPeers s `shouldBe` [2]

  it "only starts syncing against queued peers" $
    syncingPeer (run [Connect 1, Start 1, Finish 1, Start 1]) `shouldBe` Nothing

  it "makes a peer idle when it finishes" $
    peerStatus 1 (run [Connect 1, Start 1, Finish 1]) `shouldBe` Just Idle

  it "queues a peer again if it announced blocks while syncing" $ do
    let s = run [Connect 1, Connect 2, Start 1, Request 1, Finish 1]
    queuedPeers s `shouldBe` [2, 1]

  it "queues an idle peer that announces blocks" $
    queuedPeers (run [Connect 1, Start 1, Finish 1, Request 1]) `shouldBe` [1]

  it "queues every idle peer when polling" $
    queuedPeers (run [Connect 1, Connect 2, Start 1, Finish 1, Start 2, Finish 2, Poll])
      `shouldBe` [1, 2]

  it "never queues a dropped peer again" $ do
    let s = run [Connect 1, Drop 1, Request 1, Poll, Connect 1]
    peerStatus 1 s `shouldBe` Just Dropped
    queuedPeers s `shouldBe` []
    connectedPeers s `shouldBe` []

  it "forgets disconnected peers" $
    peerStatus 1 (run [Connect 1, Start 1, Disconnect 1]) `shouldBe` Nothing

  it "records activity when requesting headers" $ do
    lastActivity (run [Connect 1, Start 1]) `shouldBe` at 1
    lastActivity (run [Connect 1, Start 1, Refresh 1]) `shouldBe` at 2
    lastActivity (touchSync (at 9) (emptySync t0) :: SyncState Int) `shouldBe` at 9

  describe "synced" $ do
    let net = btcRegTest
        tipAt t w =
          BlockNode
            { header = net.genesisHeader {timestamp = t},
              height = 1,
              work = w,
              skip = headerHash net.genesisHeader
            }
        now = at 0
        recent = tipAt (floor (utcTimeToPOSIXSeconds now) - 60) 1
        idle = run [Connect 1, Start 1, Finish 1]
    it "is synced with a recent tip, enough work and nothing to do" $
      isSynced net now recent idle `shouldBe` True
    it "is not synced with an old tip" $
      isSynced net (maxTipAge `addUTCTime` now) recent idle `shouldBe` False
    it "is not synced below the minimum chain work" $
      isSynced net {minimumChainWork = 2} now recent idle `shouldBe` False
    it "is not synced while a peer is queued or syncing" $ do
      isSynced net now recent (run [Connect 1]) `shouldBe` False
      isSynced net now recent (run [Connect 1, Start 1]) `shouldBe` False
    it "is synced with a tip exactly two hours old, and not one second older" $ do
      -- two hours = 7200 seconds, the most a block may be ahead of the clock
      let nowSecs = floor (utcTimeToPOSIXSeconds now)
      isSynced net now (tipAt (nowSecs - 7200) 1) idle `shouldBe` True
      isSynced net now (tipAt (nowSecs - 7201) 1) idle `shouldBe` False

  describe "deadline of a turn" $ do
    let net = btcRegTest
        bestAt t =
          BlockNode
            { header = net.genesisHeader {timestamp = t},
              height = 1,
              work = 1,
              skip = headerHash net.genesisHeader
            }
        tipSecs = floor (utcTimeToPOSIXSeconds t0)
    it "is 15 minutes plus 1 ms for each block expected since the best block" $
      -- 6,000,000 s / 600 s per block = 10,000 blocks x 1 ms = 10 s,
      -- plus 900 s = 910 s
      syncDeadline net (at 6000000) (bestAt tipSecs) `shouldBe` at (6000000 + 910)
    it "is 15 minutes when the best block is not older than now" $
      syncDeadline net t0 (bestAt (tipSecs + 60)) `shouldBe` at 900
    it "has passed only after the deadline" $ do
      let s = run [Connect 1, Connect 2, Start 1]
      syncTimedOut (at 1000) s `shouldBe` False
      syncTimedOut (at 1001) s `shouldBe` True
    it "does not end the turn when no other peer could take over" $ do
      syncTimedOut (at 1001) (run [Connect 1, Start 1]) `shouldBe` False
      syncTimedOut (at 1001) (run [Connect 1, Connect 2, Start 1, Drop 2]) `shouldBe` False

  describe "requests in flight" $ do
    it "does not ask a peer again within two minutes of asking it" $ do
      let s = askHeaders (at 0) 1 (run [Connect 1])
      mayAsk (at 120) 1 s `shouldBe` False
      mayAsk (at 121) 1 s `shouldBe` True
    it "asks a peer again once it answered" $
      mayAsk (at 1) 1 (gotHeaders 1 (askHeaders (at 0) 1 (run [Connect 1]))) `shouldBe` True
    it "keeps track of each peer separately" $
      mayAsk (at 1) 2 (askHeaders (at 0) 1 (run [Connect 1, Connect 2])) `shouldBe` True
    it "forgets the requests of a peer that disconnects" $
      mayAsk (at 1) 1 (run [Connect 1, Ask 1, Disconnect 1, Connect 1]) `shouldBe` True

  prop "never syncs against more than one peer" $ \ops ->
    all ((<= 1) . length . filter isSyncingStatus . statuses) (states ops)

  prop "lists each peer once" $ \ops ->
    all (\s -> let ps = [p | p <- [1 .. 4], Just _ <- [peerStatus p s]] in ps == nub ps) (states ops)

  prop "never queues or syncs a dropped peer until it reconnects" $
    \ops more -> forAll (chooseInt (1, 4)) $ \p ->
      let connected = run (ops <> [Connect p])
          s = foldl' apply (dropPeer p connected) (filter (not . disconnects p) more)
       in peerStatus p s === Just Dropped
            .&&. p `notElem` queuedPeers s
            .&&. fmap fst (syncingPeer s) =/= Just p
            .&&. p `notElem` connectedPeers s

  prop "forgets a peer after it disconnects" $ \ops (p :: Int) ->
    isNothing (peerStatus p (run (ops <> [Disconnect p])))
  where
    disconnects p (Disconnect q) = p == q
    disconnects _ _ = False
