-- |
-- Module      : Haskoin.Node.Chain.Sync
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Which peer to ask for headers, as an explicit state machine.
--
-- Every connected peer has a 'PeerStatus'. "Haskoin.Node.Chain" asks one
-- peer at a time for headers. A peer is queued when it connects, and again
-- whenever it tells the node about blocks that the node does not have. If
-- nothing happens for a while, every idle peer is queued again.
--
-- = Why it works this way
--
-- * __Announcements can be missed.__ Peers announce a new block with a
--   headers message only while they believe the node knows its parent, and
--   fall back to an inventory message when many blocks arrive at once. If
--   the node only ever asked for headers when a peer connected, a missed or
--   unexpected announcement would leave it stuck at an old height for good.
--   So every hint of unknown blocks queues the peer, and a quiet period queues
--   every idle peer, which bounds how long any stall can last.
--
-- * __A peer that announced while syncing is asked again.__ Its last reply may
--   have been computed before the new block existed. The 'Syncing' status
--   remembers the announcement so that the peer is queued again when it is
--   done, rather than being forgotten or asked twice at once.
--
-- * __Misbehaving peers stay out.__ A peer that sent invalid headers or timed
--   out is marked 'Dropped' until its disconnection arrives, so nothing can
--   queue it in between.
--
-- * __A turn has a deadline.__ A peer that keeps sending full batches never
--   finishes its turn, and while it holds the turn nobody else is asked. So
--   each turn gets the deadline that Bitcoin Core gives its initial headers
--   sync, 15 minutes plus 1 millisecond for each header expected from the age
--   of the best block ('syncDeadline'). When it passes and another peer could
--   take over, the turn ends with the peer dropped ('syncTimedOut'). Core
--   applies the deadline only while the best block is more than a day old.
--   Here it applies to every turn, because the chain only counts as synced
--   once no peer holds a turn, so a peer that held one forever would keep it
--   from ever being synced. An honest turn after the initial sync takes
--   seconds.
--
-- * __Announcements get an answer at once.__ A peer that announces an
--   unknown block while another peer holds the turn is asked for headers
--   directly, as Bitcoin Core does, so that a slow or hostile syncing peer
--   cannot keep the node from hearing about new blocks. At most one such
--   request is in flight per peer ('mayAsk'): a request counts as answered
--   when the peer sends headers that connect, or none, and expires after
--   'headersResponseTime'.
--
-- * __Pure and polymorphic.__ The rules are plain functions on a value, over
--   any peer type with equality, so they can be tested exhaustively with
--   integers instead of network connections. Transitions that do not apply to
--   the current status leave the state unchanged, which makes them safe to
--   call whenever the matching event happens.
module Haskoin.Node.Chain.Sync
  ( SyncState,
    PeerStatus (..),
    emptySync,
    peerConnected,
    peerDisconnected,
    requestSync,
    startSync,
    syncDeadline,
    syncTimedOut,
    refreshSync,
    finishSync,
    dropPeer,
    pollPeers,
    touchSync,
    askHeaders,
    gotHeaders,
    mayAsk,
    headersResponseTime,
    headersSyncTimeoutBase,
    headersSyncTimeoutPerHeader,
    peerStatus,
    syncingPeer,
    queuedPeers,
    connectedPeers,
    lastActivity,
    isSynced,
    maxTipAge,
  )
where

import Data.Maybe (isNothing, listToMaybe)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Haskoin (BlockHeader (..), BlockNode (..), Network (..))

-- | Sync status of a connected peer.
data PeerStatus
  = -- | Nothing to ask it for.
    Idle
  | -- | Waiting for its turn to be asked for headers.
    Queued
  | -- | Being asked for headers. Holds the time of the last request, and
    -- whether the peer announced more blocks meanwhile, in which case it is
    -- queued again when done.
    Syncing !UTCTime !Bool
  | -- | Misbehaved and is being disconnected. Never asked for headers again.
    Dropped
  deriving stock (Eq, Show)

-- | Connected peers in queue order, with the last time that headers were
-- requested or received, the deadline of the current turn, and the requests
-- for headers in flight.
data SyncState p = SyncState
  { peers :: ![(p, PeerStatus)],
    activity :: !UTCTime,
    -- | when the turn of the syncing peer must end; set by 'startSync'
    deadline :: !(Maybe UTCTime),
    -- | peers asked for headers that have not answered, with the time asked
    asked :: ![(p, UTCTime)]
  }
  deriving stock (Eq, Show)

-- | No peers, with the given time as the last activity.
emptySync :: UTCTime -> SyncState p
emptySync t = SyncState {peers = [], activity = t, deadline = Nothing, asked = []}

-- | A peer completed the handshake. It is queued to be asked for headers.
peerConnected :: (Eq p) => p -> SyncState p -> SyncState p
peerConnected p s
  | Just _ <- peerStatus p s = s
  | otherwise = s {peers = s.peers <> [(p, Queued)]}

-- | A peer disconnected.
peerDisconnected :: (Eq p) => p -> SyncState p -> SyncState p
peerDisconnected p s =
  s {peers = filter ((/= p) . fst) s.peers, asked = filter ((/= p) . fst) s.asked}

-- | The peer told the node about blocks that it does not have.
requestSync :: (Eq p) => p -> SyncState p -> SyncState p
requestSync p s =
  case peerStatus p s of
    Just Idle -> enqueue p s
    Just (Syncing t _) -> setStatus p (Syncing t True) s
    _ -> s

-- | Start asking a queued peer for headers at the given time, unless another
-- peer is being asked already. The second time is the deadline of the turn
-- (see 'syncDeadline').
startSync :: (Eq p) => UTCTime -> UTCTime -> p -> SyncState p -> SyncState p
startSync now end p s =
  case (syncingPeer s, peerStatus p s) of
    (Nothing, Just Queued) ->
      setStatus p (Syncing now False) s {activity = now, deadline = Just end}
    _ -> s

-- | Base of the deadline of a turn: @HEADERS_DOWNLOAD_TIMEOUT_BASE@ in
-- Bitcoin Core 32.1, 15 minutes.
headersSyncTimeoutBase :: NominalDiffTime
headersSyncTimeoutBase = 15 * 60

-- | Time added to the deadline of a turn for each header expected:
-- @HEADERS_DOWNLOAD_TIMEOUT_PER_HEADER@ in Bitcoin Core 32.1, 1 millisecond.
headersSyncTimeoutPerHeader :: NominalDiffTime
headersSyncTimeoutPerHeader = 0.001

-- | Deadline of a turn that starts at the given time with the given best
-- block, as Bitcoin Core sets it for its initial headers sync:
-- 'headersSyncTimeoutBase', plus 'headersSyncTimeoutPerHeader' for each
-- block that should have been found since the best block, one every
-- 'targetSpacing' of the network.
syncDeadline :: Network -> UTCTime -> BlockNode -> UTCTime
syncDeadline net now best = timeout `addUTCTime` now
  where
    tipTime = posixSecondsToUTCTime (fromIntegral best.header.timestamp)
    age = max 0 (floor (now `diffUTCTime` tipTime)) :: Integer
    expected = age `div` max 1 (fromIntegral net.targetSpacing)
    timeout = headersSyncTimeoutBase + fromIntegral expected * headersSyncTimeoutPerHeader

-- | Whether the turn of the syncing peer has passed its deadline while
-- another connected peer could take over. Bitcoin Core only drops its
-- initial sync peer when it has another one to use.
syncTimedOut :: (Eq p) => UTCTime -> SyncState p -> Bool
syncTimedOut now s =
  case (syncingPeer s, s.deadline) of
    (Just (p, _), Just end) ->
      now > end && any (/= p) (connectedPeers s)
    _ -> False

-- | Another request was sent to the peer being synced against.
refreshSync :: (Eq p) => UTCTime -> p -> SyncState p -> SyncState p
refreshSync now p s =
  case peerStatus p s of
    Just (Syncing _ again) -> setStatus p (Syncing now again) s {activity = now}
    _ -> s

-- | The peer being synced against has no more headers to send.
finishSync :: (Eq p) => p -> SyncState p -> SyncState p
finishSync p s =
  case peerStatus p s of
    Just (Syncing _ True) -> enqueue p s
    Just (Syncing _ False) -> setStatus p Idle s
    _ -> s

-- | The peer misbehaved and is being disconnected.
dropPeer :: (Eq p) => p -> SyncState p -> SyncState p
dropPeer p s =
  case peerStatus p s of
    Just _ -> setStatus p Dropped s
    Nothing -> s

-- | Queue every idle peer, and record the time as activity so that polling
-- happens at most once per quiet period.
pollPeers :: UTCTime -> SyncState p -> SyncState p
pollPeers now s = s {peers = map poll s.peers, activity = now}
  where
    poll (p, Idle) = (p, Queued)
    poll x = x

-- | Headers were received at the given time.
touchSync :: UTCTime -> SyncState p -> SyncState p
touchSync now s = s {activity = now}

-- | Longest that a request for headers counts as in flight:
-- @HEADERS_RESPONSE_TIME@ in Bitcoin Core 32.1, 2 minutes.
headersResponseTime :: NominalDiffTime
headersResponseTime = 120

-- | A request for headers was sent to the peer at the given time.
askHeaders :: (Eq p) => UTCTime -> p -> SyncState p -> SyncState p
askHeaders now p s = s {asked = (p, now) : filter ((/= p) . fst) s.asked}

-- | The peer sent headers that answer a request: headers that connect to
-- the chain, or none. Headers that do not connect cannot be an answer, as
-- Bitcoin Core reasons, so they leave the request in flight.
gotHeaders :: (Eq p) => p -> SyncState p -> SyncState p
gotHeaders p s = s {asked = filter ((/= p) . fst) s.asked}

-- | Whether the peer may be asked for headers at the given time: no request
-- to it is in flight, or the last one is older than 'headersResponseTime'.
-- This is the rule of @MaybeSendGetHeaders@ in Bitcoin Core.
mayAsk :: (Eq p) => UTCTime -> p -> SyncState p -> Bool
mayAsk now p s =
  case lookup p s.asked of
    Nothing -> True
    Just t -> now `diffUTCTime` t > headersResponseTime

-- | Status of a peer, or 'Nothing' if it is not connected.
peerStatus :: (Eq p) => p -> SyncState p -> Maybe PeerStatus
peerStatus p s = lookup p s.peers

-- | The peer being synced against, with the time of the last request to it.
syncingPeer :: SyncState p -> Maybe (p, UTCTime)
syncingPeer s = listToMaybe [(p, t) | (p, Syncing t _) <- s.peers]

-- | Queued peers, first in line first.
queuedPeers :: SyncState p -> [p]
queuedPeers s = [p | (p, Queued) <- s.peers]

-- | Connected peers that have not been dropped.
connectedPeers :: SyncState p -> [p]
connectedPeers s = [p | (p, st) <- s.peers, st /= Dropped]

-- | Last time headers were requested or received, or peers were polled.
lastActivity :: SyncState p -> UTCTime
lastActivity s = s.activity

-- | Oldest that the best block may be for the chain to count as synced: two
-- hours, the most that a block timestamp may be ahead of the network's time.
maxTipAge :: NominalDiffTime
maxTipAge = 7200

-- | Whether the header chain counts as synced at the given time: the best
-- block is at most 'maxTipAge' old, it has the 'minimumChainWork' of the
-- network, and no peer is being synced against or waiting to be.
--
-- Recent headers alone do not prove much: a peer can feed a cheap chain with
-- fresh timestamps. Bitcoin Core requires the same work before it leaves
-- initial block download. And while peers are still queued, a better chain may
-- be about to arrive.
isSynced :: Network -> UTCTime -> BlockNode -> SyncState p -> Bool
isSynced net now best s = recent && enough && idle
  where
    tipTime = posixSecondsToUTCTime (fromIntegral best.header.timestamp)
    recent = now `diffUTCTime` tipTime <= maxTipAge
    enough = best.work >= net.minimumChainWork
    idle = isNothing (syncingPeer s) && null (queuedPeers s)

setStatus :: (Eq p) => p -> PeerStatus -> SyncState p -> SyncState p
setStatus p st s = s {peers = map set s.peers}
  where
    set (q, old)
      | q == p = (q, st)
      | otherwise = (q, old)

-- | Put a peer at the end of the queue.
enqueue :: (Eq p) => p -> SyncState p -> SyncState p
enqueue p s = s {peers = filter ((/= p) . fst) s.peers <> [(p, Queued)]}
