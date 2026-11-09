module Haskoin.Node.Chain.SyncSpec (spec) where

import Data.List (nub)
import Data.Maybe (isNothing)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
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
        pure Poll
      ]
    where
      peer = chooseInt (1, 4)

apply :: SyncState Int -> Op -> SyncState Int
apply s = \case
  Connect p -> peerConnected p s
  Disconnect p -> peerDisconnected p s
  Request p -> requestSync p s
  Start p -> startSync (at 1) p s
  Refresh p -> refreshSync (at 2) p s
  Finish p -> finishSync p s
  Drop p -> dropPeer p s
  Poll -> pollPeers (at 3) s

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
