module Haskoin.Node.Peer.LockSpec (spec) where

import Haskoin.Node.Peer.Lock
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | An attempt to reserve or release, by one of a few owners.
data Op = Acquire Int | Release Int
  deriving stock (Show)

instance Arbitrary Op where
  arbitrary = oneof [Acquire <$> owner, Release <$> owner]
    where
      owner = chooseInt (1, 3)

-- | Apply an operation, keeping the lock unchanged when it does not apply.
apply :: Lock Int -> Op -> Lock Int
apply l = \case
  Acquire o -> maybe l id (acquire o l)
  Release o -> maybe l id (release o l)

spec :: Spec
spec = describe "peer lock" $ do
  it "lets an owner reserve a free peer" $
    acquire 1 Free `shouldBe` Just (HeldBy (1 :: Int))

  it "refuses to reserve a held peer, even for its holder" $ do
    acquire 2 (HeldBy (1 :: Int)) `shouldBe` Nothing
    acquire 1 (HeldBy (1 :: Int)) `shouldBe` Nothing

  it "lets the holder release the peer" $
    release 1 (HeldBy (1 :: Int)) `shouldBe` Just Free

  it "refuses to release for anyone but the holder" $ do
    release 2 (HeldBy (1 :: Int)) `shouldBe` Nothing
    release 1 (Free :: Lock Int) `shouldBe` Nothing

  prop "only the last owner to reserve can release" $ \(ops :: [Op]) ->
    let final = foldl' apply Free ops
        model = foldl' step Nothing ops
        step Nothing (Acquire o) = Just o
        step (Just h) (Release o) | h == o = Nothing
        step m _ = m
     in holder final === model

  describe "owners" $ do
    it "are all different" $ do
      a <- newOwner "a"
      b <- newOwner "a"
      a `shouldNotBe` b
      a `shouldBe` a

    it "share the legacy owner" $
      legacyOwner `shouldBe` legacyOwner

    it "keep their names" $ do
      a <- newOwner "chain"
      ownerName a `shouldBe` "chain"
      ownerName legacyOwner `shouldBe` "legacy"
