-- |
-- Module      : Haskoin.Node.Block
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Whether the transactions of a block are the ones its header commits to.
--
-- A block on the wire is a header and a list of transactions. The proof of
-- work covers the header only. The header commits to the transactions
-- through its Merkle root, and since SegWit the coinbase commits to their
-- witness data. A peer can send a real header, with real proof of work,
-- together with transactions of its choice. Nothing else stops it.
--
-- 'blockMutation' is the check that catches this. It is a port of
-- @IsBlockMutated@ in Bitcoin Core 32.1 (@src/validation.cpp@), which Core
-- runs on every block a peer sends before it does anything else with it. A
-- block that fails it is called mutated.
--
-- = Why it works this way
--
-- * __The same check as Bitcoin Core.__ The rules, their order and the
--   reasons are those of @CheckMerkleRoot@, @IsBlockMutated@ and
--   @CheckWitnessMalleation@. A stricter check would refuse valid blocks
--   that some peer serves. A laxer one would let a peer change what a block
--   holds without changing its hash.
--
-- * __Not block validation.__ The check says nothing about whether the
--   transactions are valid, spend existing coins, or pay the right subsidy.
--   It only says that the block is the one the header names. A node that
--   needs more must validate blocks itself.
--
-- * __The header stays good.__ A mutated block does not make its header
--   invalid. The same header with the right transactions is still a valid
--   block (CVE-2012-2459). The node blames the peer that sent the mutated
--   block and leaves the header chain alone.
--
-- * __SegWit depends on the height.__ Core checks the witness commitment
--   only for a block at or above the SegWit activation height of the
--   network. Before it, miners already put commitments in coinbases without
--   the matching witness data, and those blocks are valid. So the witness
--   rule needs the height of the parent block, which 'witnessCheck' turns
--   into a 'WitnessCheck'. When the parent is not known, Core does not check
--   the witness either, and neither does this module.
module Haskoin.Node.Block
  ( BlockMutation (..),
    WitnessCheck (..),
    blockMutation,
    witnessCheck,
    segWitHeight,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize (encode)
import Haskoin

-- | Why a block is not the one its header commits to. The names follow the
-- reasons that Bitcoin Core 32.1 gives.
data BlockMutation
  = -- | the transactions do not hash to the Merkle root of the header, or
    -- there are none (@bad-txnmrklroot@)
    BadMerkleRoot
  | -- | the Merkle tree hashes two identical nodes together, so the list of
    -- transactions repeats some to keep the root (@bad-txns-duplicate@,
    -- CVE-2012-2459)
    DuplicateTransactions
  | -- | the first transaction is not a coinbase and some transaction is 64
    -- bytes long without its witness, so it may be an inner node of the
    -- Merkle tree posing as a transaction
    SixtyFourByteTransaction
  | -- | the coinbase commits to witness data, but its witness is not a single
    -- item of 32 bytes (@bad-witness-nonce-size@)
    BadWitnessNonceSize
  | -- | the witness data does not hash to the commitment in the coinbase
    -- (@bad-witness-merkle-match@)
    BadWitnessCommitment
  | -- | a transaction carries witness data, but the block does not commit to
    -- any (@unexpected-witness@)
    UnexpectedWitness
  deriving stock (Eq, Show)

-- | Which witness rule applies to a block.
data WitnessCheck
  = -- | SegWit is active for the block. If the coinbase has a witness
    -- commitment, the witness data must match it. If not, no transaction may
    -- have witness data.
    SegWitActive
  | -- | SegWit is not active for the block, or the network has no SegWit. No
    -- transaction may have witness data.
    SegWitInactive
  | -- | The height of the block is not known, so its witness data is not
    -- checked. Its Merkle root still is.
    WitnessUnknown
  deriving stock (Eq, Show)

-- | Height of the first block whose witness data is checked, on networks
-- with SegWit. The values are @consensus.SegwitHeight@ in the chain
-- parameters of Bitcoin Core 32.1 (@src/kernel/chainparams.cpp@). Networks
-- without SegWit, and networks this module does not know, give 'Nothing'.
segWitHeight :: Network -> Maybe BlockHeight
segWitHeight net
  | not net.segWit = Nothing
  | otherwise = case net.name of
      "btc" -> Just 481824
      "btctest" -> Just 834624
      "btctest4" -> Just 1
      "btcreg" -> Just 0
      _ -> Nothing

-- | The witness rule for a block, given the header of its parent if the node
-- has it. This is @DeploymentActiveAfter(prev_block, DEPLOYMENT_SEGWIT)@ in
-- Bitcoin Core: SegWit is active when the height of the block, one more than
-- its parent's, is at least 'segWitHeight'.
witnessCheck :: Network -> Maybe BlockNode -> WitnessCheck
witnessCheck net parent
  | not net.segWit = SegWitInactive
  | otherwise = case (parent, segWitHeight net) of
      (Just p, Just h)
        | p.height + 1 >= h -> SegWitActive
        | otherwise -> SegWitInactive
      _ -> WitnessUnknown

-- | Why a block is not the one its header commits to, or 'Nothing' if it is.
--
-- The checks come in the order of @IsBlockMutated@ in Bitcoin Core 32.1:
--
-- 1. The transaction ids hash to the Merkle root of the header
--    ('BadMerkleRoot'), and no level of the tree hashes two identical nodes
--    together ('DuplicateTransactions').
--
-- 2. If the first transaction is not a coinbase, no transaction is 64 bytes
--    long without its witness ('SixtyFourByteTransaction'). Such a block is
--    invalid anyway, and the witness is not looked at.
--
-- 3. The witness rule that 'WitnessCheck' names.
--
-- A block without transactions has no Merkle root and fails the first
-- check. Bitcoin Core refuses it as @bad-blk-length@.
blockMutation :: WitnessCheck -> Block -> Maybe BlockMutation
blockMutation rule b = case b.txs of
  [] -> Just BadMerkleRoot
  first : rest
    | root /= Just b.header.merkle -> Just BadMerkleRoot
    | mutated -> Just DuplicateTransactions
    | not (isCoinbase first) ->
        if any ((== 64) . strippedSize) b.txs
          then Just SixtyFourByteTransaction
          else Nothing
    | otherwise -> witnessMutation rule first rest
  where
    txids = map txHash b.txs
    -- haskoin-core gives the root of a tree that is not mutated, and refuses
    -- to build a mutated one. Only then is the root built a second time, to
    -- tell a wrong root from a mutated tree, in Bitcoin Core's order.
    (root, mutated) = case buildMerkleRoot txids of
      Right r -> (Just r, False)
      Left _ -> (merkleRoot txids, True)

-- | The witness part of the check, @CheckWitnessMalleation@ in Bitcoin Core,
-- given the coinbase and the other transactions of the block.
witnessMutation :: WitnessCheck -> Tx -> [Tx] -> Maybe BlockMutation
witnessMutation rule coinbase rest = case rule of
  WitnessUnknown -> Nothing
  SegWitInactive -> noWitness
  SegWitActive -> case witnessCommitment coinbase of
    Nothing -> noWitness
    Just commitment -> case coinbaseStack of
      [reserved]
        | B.length reserved == 32 ->
            if committed reserved == Just commitment
              then Nothing
              else Just BadWitnessCommitment
      _ -> Just BadWitnessNonceSize
  where
    noWitness
      | any hasWitness (coinbase : rest) = Just UnexpectedWitness
      | otherwise = Nothing
    -- The witness stack of the only input of the coinbase.
    coinbaseStack = case coinbase.witness of
      stack : _ -> stack
      [] -> []
    -- The witness id of the coinbase counts as zero.
    zero = "0000000000000000000000000000000000000000000000000000000000000000"
    witnessRoot = merkleRoot (zero : map txWitnessHash rest)
    committed reserved = do
      root <- witnessRoot
      return (encode (doubleSHA256 (encode root <> reserved)))

-- | The 32 bytes of the last witness commitment among the outputs of a
-- coinbase, as @GetWitnessCommitmentIndex@ in Bitcoin Core finds it: an
-- output script of at least 38 bytes that starts with @OP_RETURN@, a push of
-- 36 bytes, and the tag @aa21a9ed@.
witnessCommitment :: Tx -> Maybe ByteString
witnessCommitment tx =
  case filter isCommitment (map (.script) tx.outputs) of
    [] -> Nothing
    scripts -> Just (B.take 32 (B.drop 6 (last scripts)))
  where
    isCommitment s =
      B.length s >= 38 && B.take 6 s == B.pack [0x6a, 0x24, 0xaa, 0x21, 0xa9, 0xed]

-- | The root of the Merkle tree over the given leaves, whether or not the
-- tree is mutated, as @ComputeMerkleRoot@ in Bitcoin Core computes it.
-- 'Nothing' for no leaves.
merkleRoot :: [TxHash] -> Maybe Hash256
merkleRoot leaves = calcHash (calcTreeHeight (length leaves)) 0 leaves

-- | Whether a transaction is a coinbase, as @IsCoinBase@ in Bitcoin Core
-- tells: it has one input, and that input spends the null outpoint.
isCoinbase :: Tx -> Bool
isCoinbase tx = case tx.inputs of
  [i] -> i.outpoint == OutPoint {hash = zeroTx, index = 0xffffffff}
  _ -> False
  where
    zeroTx = "0000000000000000000000000000000000000000000000000000000000000000"

-- | Size of a transaction serialized without its witness.
strippedSize :: Tx -> Int
strippedSize tx = B.length (encode tx {witness = []})
