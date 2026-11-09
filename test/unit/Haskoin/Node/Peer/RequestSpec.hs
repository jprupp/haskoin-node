module Haskoin.Node.Peer.RequestSpec (spec) where

import Haskoin
import Haskoin.Node.Peer.Request
import Haskoin.Node.Test (mkBlocks)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

blocks :: [Block]
blocks = mkBlocks btcRegTest 0 btcRegTest.genesisHeader 1700000000 5

blockInv :: Block -> InvVector
blockInv b = InvVector InvWitnessBlock (headerHash b.header).get

txInv :: Tx -> InvVector
txInv tx = InvVector InvWitnessTx (txHash tx).get

-- | Feed messages to a request for the given items.
feed :: [InvVector] -> [Message] -> RequestStep
feed ivs = foldl' step (newRequest 42 ivs)
  where
    step (Continue r) m = stepRequest r m
    step done _ = done

-- | Messages that have nothing to do with the request.
noise :: Gen Message
noise =
  elements
    [ MPing (Ping 1),
      MPong (Pong 1),
      MVerAck,
      MSendHeaders,
      MInv (Inv [blockInv (blocks !! 4)]),
      MNotFound (NotFound [txInv (coinbaseOf (blocks !! 4))])
    ]

coinbaseOf :: Block -> Tx
coinbaseOf b = case b.txs of
  tx : _ -> tx
  [] -> error "block without transactions"

spec :: Spec
spec = describe "data request" $ do
  let want = take 3 blocks
      ivs = map blockInv want
      done = Complete (map Right want)

  it "completes when all items arrive in order" $
    feed ivs (map MBlock want) `shouldBe` done

  it "completes immediately when nothing is requested" $
    newRequest 42 [] `shouldBe` Complete []

  it "receives transactions" $ do
    let txs = map coinbaseOf want
    feed (map txInv txs) (map MTx txs) `shouldBe` Complete (map Left txs)

  it "waits for missing items" $
    case feed ivs (map MBlock (take 2 want)) of
      Continue _ -> return ()
      other -> expectationFailure ("Expected to continue, got " <> show other)

  it "fails when items arrive out of order" $
    feed ivs [MBlock (want !! 1)] `shouldBe` Failed "block out of order"

  it "fails when a requested item is not found" $
    feed ivs [MBlock (head' want), MNotFound (NotFound [ivs !! 2])]
      `shouldBe` Failed "item not found"

  it "fails when the pong arrives before all items" $
    feed ivs [MBlock (head' want), MPong (Pong 42)]
      `shouldBe` Failed "incomplete response"

  it "ignores a pong for another ping" $
    feed ivs (MPong (Pong 7) : map MBlock want) `shouldBe` done

  it "does not accept a transaction for a block request" $
    case feed ivs [MTx (coinbaseOf (head' want))] of
      Continue _ -> return ()
      other -> expectationFailure ("Expected to continue, got " <> show other)

  prop "ignores unrelated messages between items" $
    forAll (vectorOf 3 (listOf noise)) $ \gaps ->
      let msgs = concat (zipWith (\gap b -> gap <> [MBlock b]) gaps want)
       in feed ivs msgs === done
  where
    head' xs = case xs of
      x : _ -> x
      [] -> error "empty"
