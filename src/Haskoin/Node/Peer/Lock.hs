-- |
-- Module      : Haskoin.Node.Peer.Lock
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Exclusive use of a peer, as an explicit state machine.
--
-- Some conversations with a peer span many messages, such as downloading a
-- long run of headers or blocks. Two tasks holding the same conversation with
-- one peer would confuse each other's replies, so a task reserves the peer
-- first. The header chain does this, and so do applications that download
-- blocks, such as Haskoin Store.
--
-- = Why it works this way
--
-- * __Only the holder can release.__ The lock used to be a flag that anyone
--   could clear. A task that released a peer it did not hold, by mistake or
--   after its own reservation had already ended, would free the peer in the
--   middle of another task's conversation. Here a peer is 'Free' or held by an
--   'Owner', and 'release' only succeeds for the holder.
--
-- * __Pure and polymorphic.__ The transitions are plain functions over any
--   owner type with equality, so the rules can be tested with integers.
--   "Haskoin.Node.Peer" keeps a 'Lock' 'Owner' in each 'Haskoin.Node.Peer.Peer'
--   and applies them atomically.
module Haskoin.Node.Peer.Lock
  ( Lock (..),
    acquire,
    release,
    holder,
    Owner,
    newOwner,
    legacyOwner,
    ownerName,
  )
where

import Data.Function (on)
import Data.Text (Text)
import Data.Unique (Unique, newUnique)

-- | Whether a peer is reserved, and by whom.
data Lock o
  = -- | nobody is using the peer for a long conversation
    Free
  | -- | the owner is using the peer
    HeldBy !o
  deriving stock (Eq, Show)

-- | Reserve a free peer. 'Nothing' if it is held, even by the same owner:
-- reserving twice is a mistake that would hide a missing release.
acquire :: o -> Lock o -> Maybe (Lock o)
acquire o = \case
  Free -> Just (HeldBy o)
  HeldBy _ -> Nothing

-- | Release a peer. 'Nothing' unless the owner holds it.
release :: (Eq o) => o -> Lock o -> Maybe (Lock o)
release o = \case
  HeldBy o' | o == o' -> Just Free
  _ -> Nothing

-- | Who holds the peer, if anyone.
holder :: Lock o -> Maybe o
holder = \case
  Free -> Nothing
  HeldBy o -> Just o

-- | A task that reserves peers. Owners are compared by identity, and named
-- for logs.
data Owner
  = Owner !Unique !Text
  | -- | the owner behind the deprecated 'Haskoin.Node.Peer.setBusy' and
    -- 'Haskoin.Node.Peer.setFree'
    Legacy

instance Eq Owner where
  (==) = (==) `on` key
    where
      key (Owner u _) = Just u
      key Legacy = Nothing

instance Show Owner where
  show = show . ownerName

-- | A new owner with a name for logs. Each call gives a different owner.
newOwner :: Text -> IO Owner
newOwner name = (`Owner` name) <$> newUnique

-- | The single owner shared by every caller of the deprecated
-- 'Haskoin.Node.Peer.setBusy' and 'Haskoin.Node.Peer.setFree'.
legacyOwner :: Owner
legacyOwner = Legacy

-- | Name of an owner.
ownerName :: Owner -> Text
ownerName = \case
  Owner _ name -> name
  Legacy -> "legacy"
