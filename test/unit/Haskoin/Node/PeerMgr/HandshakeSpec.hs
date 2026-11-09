module Haskoin.Node.PeerMgr.HandshakeSpec (spec) where

import Control.Monad (foldM)
import Data.Word (Word64)
import Haskoin
import Haskoin.Node (buildVersion, myVersion)
import Haskoin.Node.PeerMgr.Handshake
import Haskoin.Node.Test (zeroAddress)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- | Version of a full node with nonce 7.
version :: Version
version = versionWith nodeNetwork

versionWith :: Word64 -> Version
versionWith services =
  buildVersion btcRegTest 7 0 (zeroAddress services) (zeroAddress 0) 0

-- | Run events from the start of the handshake, collecting actions.
run :: [HandshakeEvent] -> Either HandshakeError (Handshake, [HandshakeAction])
run = foldM step (AwaitingBoth, [])
  where
    step (h, acc) e = fmap (acc <>) <$> stepHandshake h e

-- | Random handshake messages: 'True' is a version, 'False' a verack.
toEvents :: [Bool] -> [HandshakeEvent]
toEvents = map (\b -> if b then GotVersion version else GotVerAck)

-- | At most one version and at most one verack, in any order: every sequence
-- a well-behaved peer can send.
validEvents :: Gen [Bool]
validEvents = do
  msgs <- sublistOf [True, False]
  elements [msgs, reverse msgs]

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

spec :: Spec
spec = describe "version handshake" $ do
  it "completes when the version comes first" $
    run [GotVersion version, GotVerAck]
      `shouldBe` Right (Established version, [SendVerAck, AnnounceConnected])

  it "completes when the verack comes first" $
    run [GotVerAck, GotVersion version]
      `shouldBe` Right (Established version, [SendVerAck, AnnounceConnected])

  it "waits for the verack after the version" $
    run [GotVersion version] `shouldBe` Right (AwaitingVerAck version, [SendVerAck])

  it "rejects a repeated verack" $ do
    run [GotVerAck, GotVerAck] `shouldBe` Left DuplicateVerAck
    run [GotVersion version, GotVerAck, GotVerAck]
      `shouldBe` Left DuplicateVerAck

  it "rejects a repeated version" $ do
    run [GotVersion version, GotVersion version] `shouldBe` Left DuplicateVersion
    run [GotVersion version, GotVerAck, GotVersion version]
      `shouldBe` Left DuplicateVersion

  prop "fails exactly when a message is repeated" $ \bs ->
    let repeated = count True bs > 1 || count False bs > 1
     in either (const True) (const False) (run (toEvents bs)) === repeated

  prop "acknowledges the version once" $
    forAll validEvents $ \bs ->
      fmap (count SendVerAck . snd) (run (toEvents bs)) === Right (count True bs)

  prop "announces the peer once, when both messages arrived" $
    forAll validEvents $ \bs ->
      let both = length bs == 2
       in fmap (\(h, as) -> (isEstablished h, count AnnounceConnected as)) (run (toEvents bs))
            === Right (both, if both then 1 else 0)

  prop "knows the version of the peer once received" $
    forAll validEvents $ \bs ->
      fmap (handshakeVersion . fst) (run (toEvents bs))
        === Right (if True `elem` bs then Just version else Nothing)

  prop "knows whether the peer acknowledged our version" $
    forAll validEvents $ \bs ->
      fmap (hasVerAck . fst) (run (toEvents bs)) === Right (False `elem` bs)

  describe "version checks" $ do
    it "accepts a full node" $
      checkVersion [1, 2] version `shouldBe` Right version
    it "rejects a peer that does not serve blocks" $
      checkVersion [] (versionWith 0) `shouldBe` Left NotFullNode
    it "rejects a connection to ourselves" $
      checkVersion [7] version `shouldBe` Left ConnectedToSelf
    it "uses our protocol version" $
      version.version `shouldBe` fromIntegral myVersion
