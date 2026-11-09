-- | Whether a block holds the transactions its header commits to, against
-- blocks mined by Bitcoin Core 32.1 and copies of them changed the ways a
-- peer could change them.
module Haskoin.Node.BlockSpec (spec) where

import Data.ByteString qualified as B
import Data.Serialize (encode)
import Haskoin
import Haskoin.Node
import Haskoin.Node.Test (corePreSegWitBlock, coreSegWitBlock)
import Test.Hspec

-- | The same block with the witness of every transaction removed.
stripWitness :: Block -> Block
stripWitness b = b {txs = [tx {witness = []} | tx <- b.txs]}

-- | The same block with the last byte of the last witness item of its
-- second transaction changed. Only the witness changes, so the txid and the
-- Merkle root stay the same.
alterWitness :: Block -> Block
alterWitness b = case b.txs of
  cb : tx : rest -> b {txs = cb : tx {witness = map (map flipLast) tx.witness} : rest}
  _ -> b
  where
    flipLast item
      | B.null item = item
      | otherwise = B.init item <> B.singleton (B.last item + 1)

-- | A node at the given height, standing for the parent of a block.
parentAt :: Network -> BlockHeight -> BlockNode
parentAt net h =
  BlockNode
    { header = net.genesisHeader,
      height = h,
      work = 0,
      skip = headerHash net.genesisHeader
    }

-- | A transaction that is not a coinbase, with an output script of the
-- given length. Without witness it is 60 bytes plus the script: 4 version
-- + 1 input count + 41 input + 1 output count + 9 output value and script
-- length + 4 lock time = 60.
nonCoinbase :: Int -> Tx
nonCoinbase n =
  Tx
    { version = 1,
      inputs =
        [ TxIn
            { outpoint = OutPoint {hash = spent, index = 0},
              script = B.empty,
              sequence = 0
            }
        ],
      outputs = [TxOut {value = 1, script = B.replicate n 0x51}],
      witness = [],
      locktime = 0
    }
  where
    spent = "1111111111111111111111111111111111111111111111111111111111111111"

-- | A block with the given transactions under a header that commits to them.
blockOf :: [Tx] -> Block
blockOf txs =
  Block
    { header =
        btcRegTest.genesisHeader
          { merkle = either error id (buildMerkleRoot (map txHash txs))
          },
      txs = txs
    }

spec :: Spec
spec = do
  describe "Bitcoin Core blocks used as fixtures" $ do
    it "are the blocks that Bitcoin Core printed" $ do
      headerHash corePreSegWitBlock.header
        `shouldBe` "55e949faadd10136287852156f9078c4ba8cadd1b602697ff4202b395bd71d75"
      headerHash coreSegWitBlock.header
        `shouldBe` "30f221284bb2468933c8a37992771939bf4257cebfb4191f7c0fab5703a62efb"
      map txHash coreSegWitBlock.txs
        `shouldBe` [ "7581bf22701c376882dac3d3d70a86e9341a35eb32bcd797f47619907f54a81e",
                     "9d304fed9ba3333c3cf6c23739376e5b740b3c181ebd08578dab63dc6401d23d",
                     "8ba9b11d4a6a205d5dd4625c71f2fb7292c08926a87c5f622bf38c09980f49c4"
                   ]
      map txWitnessHash coreSegWitBlock.txs
        `shouldBe` [ "67188a253a1f92310c43db8ebc98cb5b3b6cb26668710402f2362e29193b2758",
                     "a6b593b3a88dba0efcfe44d125eaf1c8413be672519300d001458197256a158e",
                     "8ba9b11d4a6a205d5dd4625c71f2fb7292c08926a87c5f622bf38c09980f49c4"
                   ]

  describe "blockMutation" $ do
    it "accepts a SegWit block that Bitcoin Core mined" $
      blockMutation SegWitActive coreSegWitBlock `shouldBe` Nothing

    it "accepts a block mined before SegWit with a commitment but no coinbase witness" $
      blockMutation SegWitInactive corePreSegWitBlock `shouldBe` Nothing

    it "would refuse that block if SegWit applied to it, so the height must be known" $
      -- Core's miner adds the commitment before activation, but not the
      -- 32-byte value in the coinbase witness that goes with it.
      blockMutation SegWitActive corePreSegWitBlock `shouldBe` Just BadWitnessNonceSize

    it "refuses a block whose transactions do not hash to the Merkle root" $ do
      -- Bitcoin Core: bad-txnmrklroot
      let b = coreSegWitBlock {txs = take 2 coreSegWitBlock.txs}
      blockMutation SegWitActive b `shouldBe` Just BadMerkleRoot

    it "refuses a block that repeats its last transaction to keep the Merkle root" $ do
      -- CVE-2012-2459. Bitcoin Core: bad-txns-duplicate. Three transactions
      -- and the same three with the last one repeated give the same root,
      -- because an odd level pairs its last node with itself.
      let b = coreSegWitBlock {txs = coreSegWitBlock.txs <> drop 2 coreSegWitBlock.txs}
      length b.txs `shouldBe` 4
      blockMutation SegWitActive b `shouldBe` Just DuplicateTransactions

    it "refuses a block without transactions" $ do
      let b = coreSegWitBlock {txs = []}
      blockMutation SegWitActive b `shouldBe` Just BadMerkleRoot

    it "refuses a SegWit block whose witness data was stripped" $
      -- The coinbase loses its 32-byte reserved value first.
      blockMutation SegWitActive (stripWitness coreSegWitBlock)
        `shouldBe` Just BadWitnessNonceSize

    it "refuses a SegWit block whose witness data was changed" $
      -- Bitcoin Core gave bad-witness-merkle-match for such a block.
      blockMutation SegWitActive (alterWitness coreSegWitBlock)
        `shouldBe` Just BadWitnessCommitment

    it "refuses witness data in a block for which SegWit is not active" $
      -- Bitcoin Core: unexpected-witness
      blockMutation SegWitInactive coreSegWitBlock `shouldBe` Just UnexpectedWitness

    it "does not check the witness data of a block whose height is unknown" $
      blockMutation WitnessUnknown (alterWitness coreSegWitBlock) `shouldBe` Nothing

    it "still checks the Merkle root of a block whose height is unknown" $ do
      let b = coreSegWitBlock {txs = take 2 coreSegWitBlock.txs}
      blockMutation WitnessUnknown b `shouldBe` Just BadMerkleRoot

    it "refuses a block without a coinbase that holds a 64-byte transaction" $ do
      -- 60 + 4 script bytes = 64
      B.length (encode (nonCoinbase 4)) `shouldBe` 64
      blockMutation SegWitActive (blockOf [nonCoinbase 4])
        `shouldBe` Just SixtyFourByteTransaction

    it "leaves other blocks without a coinbase to block validation" $ do
      -- 60 + 5 script bytes = 65
      B.length (encode (nonCoinbase 5)) `shouldBe` 65
      blockMutation SegWitActive (blockOf [nonCoinbase 5]) `shouldBe` Nothing

  describe "witnessCheck" $ do
    it "applies the witness rule on Bitcoin from block 481824" $ do
      witnessCheck btc (Just (parentAt btc 481822)) `shouldBe` SegWitInactive
      witnessCheck btc (Just (parentAt btc 481823)) `shouldBe` SegWitActive

    it "applies the witness rule on testnet3 from block 834624" $ do
      witnessCheck btcTest (Just (parentAt btcTest 834622)) `shouldBe` SegWitInactive
      witnessCheck btcTest (Just (parentAt btcTest 834623)) `shouldBe` SegWitActive

    it "applies the witness rule on testnet4 and regtest from the first block" $ do
      witnessCheck btcTest4 (Just (parentAt btcTest4 0)) `shouldBe` SegWitActive
      witnessCheck btcRegTest (Just (parentAt btcRegTest 0)) `shouldBe` SegWitActive

    it "leaves the witness unchecked when the parent is unknown" $
      witnessCheck btc Nothing `shouldBe` WitnessUnknown

    it "allows no witness data on Bitcoin Cash, whatever the height" $ do
      witnessCheck bch Nothing `shouldBe` SegWitInactive
      witnessCheck bch (Just (parentAt bch 800000)) `shouldBe` SegWitInactive
