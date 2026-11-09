module Haskoin.Node.PeerMgr.AddressesSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime)
import Haskoin.Node.PeerMgr.Addresses
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

later :: NominalDiffTime -> UTCTime
later d = d `addUTCTime` t0

book :: [Int] -> AddressBook Int
book = foldl' (flip (addAddress Discovered)) emptyBook

-- | Take an address that must be available.
taken :: UTCTime -> AddressBook Int -> (Int, AddressBook Int)
taken now b = case takeAddress now b of
  Just x -> x
  Nothing -> error "no address to take"

-- | Take an address, end its connection, and return the new book.
cycleWith :: Outcome -> Int -> AddressBook Int -> AddressBook Int
cycleWith outcome a b = case takeAddress t0 b of
  Just (a', b') | a == a' -> finishAddress t0 outcome a b'
  _ -> error "address not taken"

-- | When an address can be tried again, if it is waiting.
retryAt :: Int -> AddressBook Int -> Maybe UTCTime
retryAt a b = case addressState a b of
  Just (Waiting t _) -> Just t
  _ -> Nothing

-- | Transitions applied to a few addresses at increasing times.
data Op
  = Add Source Int
  | Take NominalDiffTime
  | Give Int
  | Connected Int
  | Finish NominalDiffTime Outcome Int
  deriving stock (Show)

instance Arbitrary Op where
  arbitrary =
    oneof
      [ Add <$> elements [Configured, Discovered] <*> addr,
        Take <$> time,
        Give <$> addr,
        Connected <$> addr,
        Finish <$> time <*> elements [Disconnected, Failed, Misbehaved] <*> addr
      ]
    where
      addr = chooseInt (1, 4)
      time = fromIntegral <$> chooseInt (0, 100000)

apply :: AddressBook Int -> Op -> AddressBook Int
apply b = \case
  Add src a -> addAddress src a b
  Take d -> maybe b snd (takeAddress (later d) b)
  Give a -> returnAddress a b
  Connected a -> markConnected a b
  Finish d o a -> finishAddress (later d) o a b

spec :: Spec
spec = describe "address book" $ do
  it "hands out each address once" $ do
    let (a, b) = taken t0 (book [1])
    a `shouldBe` 1
    addressState 1 b `shouldBe` Just InUse
    takeAddress t0 b `shouldSatisfy` isNothing

  it "takes back an address that was not used" $ do
    let (_, b) = taken t0 (book [1])
    addressState 1 (returnAddress 1 b) `shouldBe` Just Ready

  it "waits briefly later a normal disconnection" $ do
    let b = cycleWith Disconnected 1 (book [1])
    retryAt 1 b `shouldBe` Just (later retryDelay)
    takeAddress t0 b `shouldSatisfy` isNothing
    takeAddress (later retryDelay) b `shouldSatisfy` isJust

  it "doubles the wait later each failure, up to a limit" $ do
    let waits = map (retryAt 1) (drop 1 (iterate (cycleFailed 1) (book [1])))
        cycleFailed a b = finishAddress t0 Failed a (snd (taken (later 1000000) b))
    take 4 waits `shouldBe` map (Just . later) [2, 4, 8, 16]
    waits !! 20 `shouldBe` Just (later maxBackoff)

  it "forgets failures once the peer comes online" $ do
    let failed = cycleWith Failed 1 (book [1])
        (_, b) = taken (later 1000) failed
        b' = finishAddress t0 Failed 1 (markConnected 1 b)
    retryAt 1 b' `shouldBe` Just (later 2)

  it "bans a discovered address that misbehaved" $
    retryAt 1 (cycleWith Misbehaved 1 (book [1])) `shouldBe` Just (later banTime)

  it "never bans a configured address" $ do
    let b = addAddress Configured 1 (book [1])
    retryAt 1 (cycleWith Misbehaved 1 b) `shouldBe` Just (later maxBackoff)

  it "keeps a bounded number of discovered addresses" $ do
    let b = book [1 .. maxDiscovered + 10]
        b' = addAddress Configured (maxDiscovered + 20) b
    length (knownAddresses b) `shouldBe` maxDiscovered
    length (knownAddresses b') `shouldBe` maxDiscovered + 1

  describe "loading" $ do
    it "loads when the book is empty" $
      needsReload t0 (emptyBook :: AddressBook Int) `shouldBe` True

    it "does not load while an address can be tried" $
      needsReload t0 (book [1]) `shouldBe` False

    it "does not load again too soon" $ do
      -- banned, so that nothing can be tried for a long time
      let b = markLoaded t0 (cycleWith Misbehaved 1 (book [1]))
      needsReload (later 1) b `shouldBe` False
      needsReload (later reloadInterval) b `shouldBe` True

  prop "only hands out addresses that can be tried" $ \(ops :: [Op]) (d :: Int) ->
    let b = foldl' apply emptyBook ops
        now = later (fromIntegral (abs d))
     in case takeAddress now b of
          Nothing -> property True
          Just (a, b') ->
            let ok = case addressState a b of
                  Just Ready -> True
                  Just (Waiting t _) -> t <= now
                  _ -> False
             in ok .&&. addressState a b' === Just InUse

  prop "never forgets an address" $ \(ops :: [Op]) ->
    let b = foldl' apply emptyBook ops
        added = [a | Add _ a <- ops]
     in all (\a -> isJust (addressState a b)) added
