-- |
-- Module      : Haskoin.Node.PeerMgr.Addresses
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- The addresses that the peer manager connects to, as an explicit state
-- machine.
--
-- Every known address is 'Ready' to be tried, 'InUse' by a connection, or
-- 'Waiting' until a given time. How long an address waits after a connection
-- ends depends on how it ended ('Outcome').
--
-- = Why it works this way
--
-- * __Failing addresses back off.__ The node used to try a static peer that
--   refused connections again within seconds, forever, and an address it had
--   just disconnected for misbehaving could be connected again at once. Here a
--   failed address waits twice as long after each failure, up to
--   'maxBackoff', and a misbehaving one waits 'banTime'.
--
-- * __Configured peers are trusted more.__ Addresses from the configuration
--   are what the operator asked for, so they are never banned and never
--   forgotten: they back off at most 'maxBackoff', even after misbehaving.
--   Discovered addresses, from DNS seeds and gossip, are banned for
--   misbehaving, and at most 'maxDiscovered' of them are kept, so gossip
--   cannot exhaust memory.
--
-- * __Pure, with time as an input.__ Every transition takes the current time,
--   so the timing rules can be tested without waiting. "Haskoin.Node.PeerMgr"
--   keeps the book in a 'Control.Concurrent.STM.TVar'.
module Haskoin.Node.PeerMgr.Addresses
  ( AddressBook,
    Source (..),
    AddressState (..),
    Outcome (..),
    emptyBook,
    addAddress,
    takeAddress,
    returnAddress,
    markConnected,
    finishAddress,
    addressState,
    needsReload,
    markLoaded,
    knownAddresses,
    retryDelay,
    maxBackoff,
    banTime,
    reloadInterval,
    maxDiscovered,
  )
where

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

-- | Where an address came from.
data Source
  = -- | the static peers of the configuration
    Configured
  | -- | DNS seeds, or addresses relayed by peers
    Discovered
  deriving stock (Eq, Show)

-- | Whether an address can be tried.
data AddressState
  = -- | can be tried now
    Ready
  | -- | a connection to it is open or being opened
    InUse
  | -- | can be tried again at the given time; holds the number of failures in
    -- a row
    Waiting !UTCTime !Int
  deriving stock (Eq, Show)

-- | How a connection ended.
data Outcome
  = -- | the peer was online and the connection ended normally, such as by
    -- rotation or because the peer closed it
    Disconnected
  | -- | the connection could not be made, or the peer stopped responding
    Failed
  | -- | the peer broke the protocol
    Misbehaved
  deriving stock (Eq, Show)

data Entry = Entry
  { source :: !Source,
    state :: !AddressState,
    -- | failures in a row
    failures :: !Int
  }
  deriving stock (Eq, Show)

-- | Known addresses, and when the configured peers and seeds were last
-- loaded.
data AddressBook a = AddressBook
  { entries :: !(Map a Entry),
    loaded :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show)

-- | Wait after a normal disconnection before trying the address again.
retryDelay :: NominalDiffTime
retryDelay = 1

-- | Longest wait after failures.
maxBackoff :: NominalDiffTime
maxBackoff = 600

-- | Wait after a discovered address misbehaved.
banTime :: NominalDiffTime
banTime = 24 * 3600

-- | Shortest time between two loads of the configured peers and seeds.
reloadInterval :: NominalDiffTime
reloadInterval = 300

-- | Most discovered addresses kept.
maxDiscovered :: Int
maxDiscovered = 4096

-- | No addresses, never loaded.
emptyBook :: AddressBook a
emptyBook = AddressBook {entries = Map.empty, loaded = Nothing}

-- | Learn an address. A known address keeps its state; one from the
-- configuration is marked 'Configured'. A discovered address is dropped if
-- 'maxDiscovered' are known already.
addAddress :: (Ord a) => Source -> a -> AddressBook a -> AddressBook a
addAddress src a book =
  case Map.lookup a book.entries of
    Just e
      | src == Configured -> put e {source = Configured}
      | otherwise -> book
    Nothing
      | src == Discovered && discovered >= maxDiscovered -> book
      | otherwise -> put Entry {source = src, state = Ready, failures = 0}
  where
    put e = book {entries = Map.insert a e book.entries}
    discovered = Map.size (Map.filter ((== Discovered) . (.source)) book.entries)

-- | Take an address to connect to: a 'Ready' one, or a 'Waiting' one whose
-- time has come. It becomes 'InUse'.
takeAddress :: (Ord a) => UTCTime -> AddressBook a -> Maybe (a, AddressBook a)
takeAddress now book = do
  (a, e) <- find (ready . snd) (Map.toList book.entries)
  return (a, setEntry a e {state = InUse} book)
  where
    ready e = case e.state of
      Ready -> True
      InUse -> False
      Waiting t _ -> t <= now

-- | Give back an address taken with 'takeAddress' without connecting to it.
returnAddress :: (Ord a) => a -> AddressBook a -> AddressBook a
returnAddress = adjust $ \e -> case e.state of
  InUse -> e {state = Ready}
  _ -> e

-- | A connection to the address completed the handshake. Its failures are
-- forgotten.
markConnected :: (Ord a) => a -> AddressBook a -> AddressBook a
markConnected = adjust $ \e -> e {failures = 0}

-- | A connection to the address ended. Decide when to try it again.
finishAddress ::
  (Ord a) => UTCTime -> Outcome -> a -> AddressBook a -> AddressBook a
finishAddress now outcome = adjust $ \e ->
  let wait d f = e {state = Waiting (d `addUTCTime` now) f, failures = f}
      backoff f = min maxBackoff (retryDelay * 2 ^ min f 30)
   in case outcome of
        Disconnected -> wait retryDelay 0
        Failed -> let f = e.failures + 1 in wait (backoff f) f
        Misbehaved
          | e.source == Configured -> wait maxBackoff (e.failures + 1)
          | otherwise -> wait banTime (e.failures + 1)

-- | State of an address, or 'Nothing' if it is not known.
addressState :: (Ord a) => a -> AddressBook a -> Maybe AddressState
addressState a book = (.state) <$> Map.lookup a book.entries

-- | Whether to load the configured peers and seeds: when no address can be
-- tried now, and they were never loaded or not loaded for 'reloadInterval'.
needsReload :: (Ord a) => UTCTime -> AddressBook a -> Bool
needsReload now book =
  isNothing (takeAddress now book)
    && maybe True (\t -> now `diffUTCTime` t >= reloadInterval) book.loaded

-- | The configured peers and seeds were loaded at the given time.
markLoaded :: UTCTime -> AddressBook a -> AddressBook a
markLoaded now book = book {loaded = Just now}

-- | Every known address.
knownAddresses :: AddressBook a -> [a]
knownAddresses book = Map.keys book.entries

setEntry :: (Ord a) => a -> Entry -> AddressBook a -> AddressBook a
setEntry a e book = book {entries = Map.insert a e book.entries}

adjust :: (Ord a) => (Entry -> Entry) -> a -> AddressBook a -> AddressBook a
adjust f a book = book {entries = Map.adjust f a book.entries}
