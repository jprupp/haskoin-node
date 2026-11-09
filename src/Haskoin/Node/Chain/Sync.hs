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
    refreshSync,
    finishSync,
    dropPeer,
    pollPeers,
    touchSync,
    peerStatus,
    syncingPeer,
    queuedPeers,
    connectedPeers,
    lastActivity,
  )
where

import Data.Maybe (listToMaybe)
import Data.Time.Clock (UTCTime)

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
-- requested or received.
data SyncState p = SyncState
  { peers :: ![(p, PeerStatus)],
    activity :: !UTCTime
  }
  deriving stock (Eq, Show)

-- | No peers, with the given time as the last activity.
emptySync :: UTCTime -> SyncState p
emptySync = SyncState []

-- | A peer completed the handshake. It is queued to be asked for headers.
peerConnected :: (Eq p) => p -> SyncState p -> SyncState p
peerConnected p s
  | Just _ <- peerStatus p s = s
  | otherwise = s {peers = s.peers <> [(p, Queued)]}

-- | A peer disconnected.
peerDisconnected :: (Eq p) => p -> SyncState p -> SyncState p
peerDisconnected p s = s {peers = filter ((/= p) . fst) s.peers}

-- | The peer told the node about blocks that it does not have.
requestSync :: (Eq p) => p -> SyncState p -> SyncState p
requestSync p s =
  case peerStatus p s of
    Just Idle -> enqueue p s
    Just (Syncing t _) -> setStatus p (Syncing t True) s
    _ -> s

-- | Start asking a queued peer for headers, unless another peer is being
-- asked already.
startSync :: (Eq p) => UTCTime -> p -> SyncState p -> SyncState p
startSync now p s =
  case (syncingPeer s, peerStatus p s) of
    (Nothing, Just Queued) -> setStatus p (Syncing now False) s {activity = now}
    _ -> s

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
pollPeers now s = SyncState {peers = map poll s.peers, activity = now}
  where
    poll (p, Idle) = (p, Queued)
    poll x = x

-- | Headers were received at the given time.
touchSync :: UTCTime -> SyncState p -> SyncState p
touchSync now s = s {activity = now}

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

setStatus :: (Eq p) => p -> PeerStatus -> SyncState p -> SyncState p
setStatus p st s = s {peers = map set s.peers}
  where
    set (q, old)
      | q == p = (q, st)
      | otherwise = (q, old)

-- | Put a peer at the end of the queue.
enqueue :: (Eq p) => p -> SyncState p -> SyncState p
enqueue p s = s {peers = filter ((/= p) . fst) s.peers <> [(p, Queued)]}
