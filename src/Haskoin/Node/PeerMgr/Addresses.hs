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
--   They are also tried first. Addresses from DNS seeds and gossip are
--   banned for misbehaving, and at most 'maxDiscovered' of each are kept, so
--   gossip cannot exhaust memory.
--
-- * __Seeds and gossip are kept apart.__ Addresses from DNS seeds count
--   against their own limit. A peer that gossips thousands of addresses can
--   fill the room for gossip, but not crowd out the seeds. Before, both
--   shared one limit, and once gossip had filled it, no new seed address was
--   learned.
--
-- * __A full book still learns.__ When the limit of a source is reached, a
--   new address takes the place of one from the same source that has failed
--   to connect, if there is one. Addresses that work, or have not been tried,
--   stay.
--
-- * __Random choice.__ 'takeAddress' picks among the addresses that can be
--   tried at random, as Bitcoin Core's address manager does. It used to take
--   the first one in the order of the map, which for socket addresses is the
--   order of the port, so an attacker that gossiped addresses with low ports
--   was always tried first. The randomness is an input, so that the book
--   stays pure.
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
    addressSource,
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

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word64)

-- | Where an address came from.
data Source
  = -- | the static peers of the configuration
    Configured
  | -- | the DNS seeds of the network
    Seeded
  | -- | relayed by peers
    Discovered
  deriving stock (Eq, Ord, Show)

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

-- | Known addresses, how many of them came from seeds and from gossip, the
-- ones that may make room for a new address, and when the configured peers
-- and seeds were last loaded.
--
-- The counts and the index are kept in step with the entries by 'putEntry',
-- through which every change goes, so that learning an address never scans
-- the book.
data AddressBook a = AddressBook
  { entries :: !(Map a Entry),
    loaded :: !(Maybe UTCTime),
    -- | how many entries are 'Seeded'
    seeded :: !Int,
    -- | how many entries are 'Discovered'
    discovered :: !Int,
    -- | seed and gossiped addresses that failed and are not in use, by
    -- source, with their failures in a row (see 'replaceable')
    failing :: !(Map Source (Set (Int, a)))
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

-- | Most addresses kept from each of the DNS seeds and gossip.
maxDiscovered :: Int
maxDiscovered = 4096

-- | No addresses, never loaded.
emptyBook :: AddressBook a
emptyBook =
  AddressBook
    { entries = Map.empty,
      loaded = Nothing,
      seeded = 0,
      discovered = 0,
      failing = Map.empty
    }

-- | Learn an address. A known address keeps its state; one from the
-- configuration is marked 'Configured'. When 'maxDiscovered' addresses from
-- the same source are known already, a new seed or gossiped address takes
-- the place of one from that source that has failed to connect and is not in
-- use, the one with the most failures in a row. If there is none, the new
-- address is dropped.
addAddress :: (Ord a) => Source -> a -> AddressBook a -> AddressBook a
addAddress src a book =
  case Map.lookup a book.entries of
    Just e
      | src == Configured -> setEntry a e {source = Configured} book
      | otherwise -> book
    Nothing
      | src == Configured -> fresh book
      | sourceCount src book < maxDiscovered -> fresh book
      | otherwise -> case evictable of
          Just old -> fresh (forget old book)
          Nothing -> book
  where
    fresh = setEntry a Entry {source = src, state = Ready, failures = 0}
    -- the one with the most failures, and the greatest address among those
    evictable = snd <$> (Set.lookupMax =<< Map.lookup src book.failing)

-- | Take an address to connect to: a 'Ready' one, or a 'Waiting' one whose
-- time has come. It becomes 'InUse'.
--
-- Configured addresses come first, in order. Otherwise the address is picked
-- at random, using the given number, among the seed and gossiped addresses
-- that the predicate allows. "Haskoin.Node.PeerMgr" uses the predicate to
-- skip addresses in a network group that it is connected to already.
takeAddress ::
  (Ord a) => UTCTime -> Word64 -> (a -> Bool) -> AddressBook a -> Maybe (a, AddressBook a)
takeAddress now r allowed book =
  case filter ((== Configured) . (.source) . snd) candidates of
    (a, e) : _ -> Just (take_ a e)
    [] -> case filter (allowed . fst) candidates of
      [] -> Nothing
      others ->
        let (a, e) = others !! fromIntegral (r `mod` fromIntegral (length others))
         in Just (take_ a e)
  where
    candidates = filter (ready . snd) (Map.toList book.entries)
    take_ a e = (a, setEntry a e {state = InUse} book)
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

-- | Where an address came from, or 'Nothing' if it is not known.
addressSource :: (Ord a) => a -> AddressBook a -> Maybe Source
addressSource a book = (.source) <$> Map.lookup a book.entries

-- | Whether to load the configured peers and seeds: when no address can be
-- tried now, and they were never loaded or not loaded for 'reloadInterval'.
needsReload :: (Ord a) => UTCTime -> AddressBook a -> Bool
needsReload now book =
  isNothing (takeAddress now 0 (const True) book)
    && maybe True (\t -> now `diffUTCTime` t >= reloadInterval) book.loaded

-- | The configured peers and seeds were loaded at the given time.
markLoaded :: UTCTime -> AddressBook a -> AddressBook a
markLoaded now book = book {loaded = Just now}

-- | Every known address.
knownAddresses :: AddressBook a -> [a]
knownAddresses book = Map.keys book.entries

setEntry :: (Ord a) => a -> Entry -> AddressBook a -> AddressBook a
setEntry a e = putEntry a (Just e)

-- | Remove an address.
forget :: (Ord a) => a -> AddressBook a -> AddressBook a
forget a = putEntry a Nothing

adjust :: (Ord a) => (Entry -> Entry) -> a -> AddressBook a -> AddressBook a
adjust f a book = case Map.lookup a book.entries of
  Nothing -> book
  Just e -> putEntry a (Just (f e)) book

-- | Whether an entry may make room for a new address from its source: it is
-- a seed or gossiped address that failed and is not in use.
replaceable :: Entry -> Bool
replaceable e = e.source /= Configured && e.failures > 0 && e.state /= InUse

-- | Set the entry of an address, or remove it with 'Nothing'. The counts of
-- seed and gossiped addresses and the index of 'replaceable' ones follow.
putEntry :: (Ord a) => a -> Maybe Entry -> AddressBook a -> AddressBook a
putEntry a new book =
  maybe id include new . maybe id exclude old $
    book {entries = Map.alter (const new) a book.entries}
  where
    old = Map.lookup a book.entries
    exclude e b =
      count e.source (-1) $
        if replaceable e
          then b {failing = Map.adjust (Set.delete (e.failures, a)) e.source b.failing}
          else b
    include e b =
      count e.source 1 $
        if replaceable e
          then b {failing = Map.insertWith Set.union e.source (Set.singleton (e.failures, a)) b.failing}
          else b

-- | Change the count of addresses from a source. Configured addresses are
-- not counted.
count :: Source -> Int -> AddressBook a -> AddressBook a
count src n book = case src of
  Configured -> book
  Seeded -> book {seeded = book.seeded + n}
  Discovered -> book {discovered = book.discovered + n}

-- | How many addresses from a source are known.
sourceCount :: Source -> AddressBook a -> Int
sourceCount src book = case src of
  Configured -> 0
  Seeded -> book.seeded
  Discovered -> book.discovered
