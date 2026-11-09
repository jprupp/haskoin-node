module Haskoin.Node.PeerMgr.AddressesSpec (spec) where

import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime)
import Data.Word (Word64)
import Haskoin.Node.PeerMgr.Addresses
import Network.Socket (SockAddr (..), tupleToHostAddress)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

later :: NominalDiffTime -> UTCTime
later d = d `addUTCTime` t0

book :: [Int] -> AddressBook Int
book = foldl' (flip (addAddress Discovered)) emptyBook

-- | Take an address, with any address allowed.
takeAny :: (Ord a) => UTCTime -> AddressBook a -> Maybe (a, AddressBook a)
takeAny now = takeAddress now 0 (const True)

-- | Take an address that must be available.
taken :: UTCTime -> AddressBook Int -> (Int, AddressBook Int)
taken now b = case takeAny now b of
  Just x -> x
  Nothing -> error "no address to take"

-- | Take a given address, end its connection, and return the new book. Only
-- that address may be taken.
cycleWith :: Outcome -> Int -> AddressBook Int -> AddressBook Int
cycleWith outcome a b = case takeAddress t0 0 (== a) b of
  Just (a', b') | a == a' -> finishAddress t0 outcome a b'
  _ -> error "address not taken"

-- | Every address that some random number makes 'takeAddress' pick, sorted.
picks :: (Ord a) => (a -> Bool) -> AddressBook a -> [a]
picks allowed b =
  sort (nub (mapMaybe (fmap fst . (\r -> takeAddress t0 r allowed b)) [0 .. 99]))

-- | When an address can be tried again, if it is waiting.
retryAt :: Int -> AddressBook Int -> Maybe UTCTime
retryAt a b = case addressState a b of
  Just (Waiting t _) -> Just t
  _ -> Nothing

-- | Transitions applied to a few addresses at increasing times.
data Op
  = Add Source Int
  | Take NominalDiffTime Word64
  | Give Int
  | Connected Int
  | Finish NominalDiffTime Outcome Int
  deriving stock (Show)

instance Arbitrary Op where
  arbitrary =
    oneof
      [ Add <$> elements [Configured, Seeded, Discovered] <*> addr,
        Take <$> time <*> arbitrary,
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
  Take d r -> maybe b snd (takeAddress (later d) r (const True) b)
  Give a -> returnAddress a b
  Connected a -> markConnected a b
  Finish d o a -> finishAddress (later d) o a b

spec :: Spec
spec = describe "address book" $ do
  it "hands out each address once" $ do
    let (a, b) = taken t0 (book [1])
    a `shouldBe` 1
    addressState 1 b `shouldBe` Just InUse
    takeAny t0 b `shouldSatisfy` isNothing

  it "takes back an address that was not used" $ do
    let (_, b) = taken t0 (book [1])
    addressState 1 (returnAddress 1 b) `shouldBe` Just Ready

  it "waits briefly later a normal disconnection" $ do
    let b = cycleWith Disconnected 1 (book [1])
    retryAt 1 b `shouldBe` Just (later retryDelay)
    takeAny t0 b `shouldSatisfy` isNothing
    takeAny (later retryDelay) b `shouldSatisfy` isJust

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
    length (knownAddresses b) `shouldBe` 4096
    length (knownAddresses b') `shouldBe` 4096 + 1

  it "keeps a bounded number of seed addresses" $ do
    let b = foldl' (flip (addAddress Seeded)) emptyBook [1 .. 4106 :: Int]
    length (knownAddresses b) `shouldBe` 4096

  it "learns a new seed address when gossip has filled the book" $ do
    let b = addAddress Seeded 99999 (book [1 .. 4096])
    addressState 99999 b `shouldBe` Just Ready

  it "replaces a gossiped address that failed when the book is full" $ do
    let b = addAddress Discovered 5000 (cycleWith Failed 7 (book [1 .. 4096]))
    addressState 5000 b `shouldBe` Just Ready
    addressState 7 b `shouldBe` Nothing
    length (knownAddresses b) `shouldBe` 4096

  it "keeps gossiped addresses that have not failed when the book is full" $ do
    let b = addAddress Discovered 5000 (book [1 .. 4096])
    addressState 5000 b `shouldBe` Nothing
    length (knownAddresses b) `shouldBe` 4096

  describe "choice" $ do
    it "takes a configured address before any other" $ do
      let b = addAddress Configured 9 (book [1 .. 5])
      fmap fst (takeAddress t0 3 (const True) b) `shouldBe` Just 9

    it "takes only addresses that the predicate allows" $
      picks even (book [1 .. 10]) `shouldBe` [2, 4, 6, 8, 10]

    it "takes nothing when the predicate allows no address" $
      takeAddress t0 0 (const False) (book [1 .. 10]) `shouldSatisfy` isNothing

    it "can take any address, whatever its port" $ do
      -- 20 addresses on port 8333 and 8 on one host with lower ports. Taking
      -- them in the order of the map would always start with the low ports.
      let v4 a p = SockAddrInet p (tupleToHostAddress a)
          honest = [v4 (8, 8, 8, fromIntegral i) 8333 | i <- [1 .. 20 :: Int]]
          attacker = [v4 (45, 1, 2, 3) p | p <- [1000 .. 1007]]
          b = foldl' (flip (addAddress Discovered)) emptyBook (honest <> attacker)
      picks (const True) b `shouldBe` sort (honest <> attacker)

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
     in case takeAny now b of
          Nothing -> property True
          Just (a, b') ->
            let ok = case addressState a b of
                  Just Ready -> True
                  Just (Waiting t _) -> t <= now
                  _ -> False
             in ok .&&. addressState a b' === Just InUse

  -- Four addresses never fill the book, so none has to make room.
  prop "never forgets an address while the book has room" $ \(ops :: [Op]) ->
    let b = foldl' apply emptyBook ops
        added = [a | Add _ a <- ops]
     in all (\a -> isJust (addressState a b)) added

  prop "never keeps more than 4096 gossiped or 4096 seed addresses" $
    forAll (listOf manyAddresses) $ \ops ->
      let b = foldl' apply (book [1 .. 4096]) ops
          from src = [a | a <- knownAddresses b, addressSource a b == Just src]
       in length (from Discovered) <= 4096 .&&. length (from Seeded) <= 4096
  where
    -- Operations on 6000 addresses, on top of a book that is full already.
    manyAddresses =
      oneof
        [ Add <$> elements [Seeded, Discovered] <*> chooseInt (1, 6000),
          Take <$> (fromIntegral <$> chooseInt (0, 100000)) <*> arbitrary,
          Finish
            <$> (fromIntegral <$> chooseInt (0, 100000))
            <*> elements [Disconnected, Failed, Misbehaved]
            <*> chooseInt (1, 6000)
        ]
