-- |
-- Module      : Haskoin.Node.Chain.Database
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Storage of block headers in RocksDB, for "Haskoin.Node.Chain".
--
-- Headers are stored as 'BlockNode' values in the configured column family, or
-- the default one, under keys starting with byte @0x90@. The best block is
-- stored under key @0x91@ and a data version under key @0x92@. If the data
-- version does not match, keys @0x90@ and @0x91@ are deleted when the chain
-- starts. Applications that share the database must not use these prefixes.
--
-- = Why it works this way
--
-- * __Headers are stored, not kept in memory.__ Storing them lets a restart
--   resume where it stopped instead of downloading the whole chain again.
--
-- * __Through haskoin-core's interface.__ The header functions of
--   haskoin-core, such as 'connectBlocks' and 'getAncestor', run in any monad
--   with a 'BlockHeaders' instance. 'runHeaders' runs them against the
--   database, so validation and storage are not reimplemented here. The monad
--   that carries the database is a 'ReaderT', and it stays inside this
--   module: everything else calls 'runHeaders' and works in plain 'IO'.
--
-- * __Separate from syncing.__ This module only knows how headers are laid
--   out on disk. The chain process decides what to store and when.
module Haskoin.Node.Chain.Database
  ( ChainDB (..),
    initChainDB,
    runHeaders,
  )
where

import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.ByteString qualified as B
import Data.Maybe (isNothing)
import Data.Serialize (Serialize (..), getWord8, putWord8)
import Data.Word (Word32)
import Database.RocksDB (ColumnFamily, DB)
import Database.RocksDB qualified as R
import Database.RocksDB.Query
  ( Key,
    KeyValue,
    insert,
    insertCF,
    insertOp,
    insertOpCF,
    retrieveCommon,
    writeBatch,
  )
import Haskoin hiding (Key)

-- | Where headers are stored: a database and an optional column family.
data ChainDB = ChainDB
  { -- | database handle
    db :: !DB,
    -- | column family, or the default one if 'Nothing'
    cf :: !(Maybe ColumnFamily)
  }

-- | Key of the data version.
data VersionKey = VersionKey
  deriving stock (Eq, Show)

instance Serialize VersionKey where
  get = getWord8 >>= guard . (== 0x92) >> return VersionKey
  put VersionKey = putWord8 0x92

instance Key VersionKey

instance KeyValue VersionKey Word32

-- | Key of a block header.
newtype BlockHeaderKey = BlockHeaderKey BlockHash
  deriving stock (Eq, Show)

instance Serialize BlockHeaderKey where
  get = getWord8 >>= guard . (== 0x90) >> BlockHeaderKey <$> get
  put (BlockHeaderKey bh) = putWord8 0x90 >> put bh

instance KeyValue BlockHeaderKey BlockNode

-- | Key of the best block.
data BestBlockKey = BestBlockKey
  deriving stock (Eq, Show)

instance Serialize BestBlockKey where
  get = getWord8 >>= guard . (== 0x91) >> return BestBlockKey
  put BestBlockKey = putWord8 0x91

instance KeyValue BestBlockKey BlockNode

-- | Run header functions of haskoin-core against the database.
runHeaders ::
  ChainDB -> (forall n. (BlockHeaders n) => n a) -> IO a
runHeaders c action = (runReaderT action c)

-- | Read and write headers in the database.
instance BlockHeaders (ReaderT ChainDB IO) where
  addBlockHeader bn = ReaderT $ \c -> insertCommon c (nodeKey bn) bn
  getBlockHeader bh =
    ReaderT $ \c -> retrieveCommon c.db c.cf (BlockHeaderKey bh)
  getBestBlockHeader = ReaderT $ \c ->
    (retrieveCommon c.db c.cf BestBlockKey) >>= \case
      Nothing -> error "Could not get best block from database"
      Just b -> return b
  setBestBlockHeader bn = ReaderT $ \c -> insertCommon c BestBlockKey bn
  addBlockHeaders bns =
    ReaderT $ \c -> writeBatch c.db $ map (op c) bns
    where
      op c bn = case c.cf of
        Nothing -> insertOp (nodeKey bn) bn
        Just cf -> insertOpCF cf (nodeKey bn) bn

nodeKey :: BlockNode -> BlockHeaderKey
nodeKey bn = BlockHeaderKey (headerHash bn.header)

insertCommon ::
  (KeyValue k v, Serialize k, Serialize v) =>
  ChainDB ->
  k ->
  v ->
  IO ()
insertCommon c k v = case c.cf of
  Nothing -> insert c.db k v
  Just cf -> insertCF c.db cf k v

-- | Version of the data layout.
dataVersion :: Word32
dataVersion = 1

-- | Prepare the database for a network. If its data version is not the
-- current one, headers and best block are deleted first. An empty database
-- gets the genesis block of the network as its best block.
initChainDB :: Network -> ChainDB -> IO ()
initChainDB net c = do
  ver <- retrieveCommon c.db c.cf VersionKey
  unless (ver == Just dataVersion) $ purgeChainDB c
  insertCommon c VersionKey dataVersion
  best <- retrieveCommon c.db c.cf BestBlockKey
  when (isNothing (best :: Maybe BlockNode)) $ runHeaders c $ do
    addBlockHeader (genesisNode net)
    setBestBlockHeader (genesisNode net)

-- | Delete headers and best block from the column family that holds the
-- chain. Keys are deleted one at a time as the iterator walks them, so that
-- memory use does not grow with the size of the database.
purgeChainDB :: ChainDB -> IO ()
purgeChainDB c = withIter $ \it -> do
  R.iterSeek it (B.singleton 0x90)
  let go =
        R.iterKey it >>= \case
          Just k | B.take 1 k `elem` [B.singleton 0x90, B.singleton 0x91] -> do
            case c.cf of
              Nothing -> R.delete c.db k
              Just cf -> R.deleteCF c.db cf k
            R.iterNext it
            go
          _ -> return ()
  go
  where
    withIter = case c.cf of
      Nothing -> R.withIter c.db
      Just cf -> R.withIterCF c.db cf
