-- |
-- Module      : Haskoin.Node.PeerMgr.Lifecycle
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- The life of a connection to a peer, as an explicit state machine: the
-- handshake, then keeping the peer alive with pings, then disconnecting.
--
-- "Haskoin.Node.PeerMgr" keeps a 'PeerState' for every connection. It feeds
-- 'stepPeer' what happens (a message arrived, a pong came back, the clock
-- ticked) and carries out the 'PeerAction's that come back.
--
-- = Why it works this way
--
-- * __Every stage is named.__ A connection is 'Handshaking', 'Online' or
--   'Disconnecting'. Before, a peer that had been told to disconnect looked
--   exactly like an online one until its process died: it was still offered
--   to applications, still had its messages routed, and could be told to
--   disconnect again. 'Disconnecting' makes that window explicit, and every
--   input in it is ignored.
--
-- * __Time is an input.__ The rules about silence, pings and age depend on
--   the clock. Instead of reading it, 'stepPeer' takes the time in 'Tick',
--   'Heard' and 'GotPong'. Every timing rule can then be tested by passing
--   times, without waiting.
--
-- * __Randomness is an input.__ A ping needs a fresh nonce. 'Tick' carries
--   one, so that the transition stays a pure function.
--
-- * __One reason per disconnection.__ 'DisconnectReason' records why the node
--   gave up on a peer, so that the peer manager can decide whether to try the
--   address again soon, later, or not for a long time (see
--   "Haskoin.Node.PeerMgr.Addresses").
module Haskoin.Node.PeerMgr.Lifecycle
  ( PeerState (..),
    Stage (..),
    Limits (..),
    PeerInput (..),
    PeerAction (..),
    DisconnectReason (..),
    newPeerState,
    stepPeer,
    isOnline,
    stageVersion,
    pendingPing,
  )
where

import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Word (Word64)
import Haskoin (Version)
import Haskoin.Node.PeerMgr.Handshake

-- | Stage of a connection.
data Stage
  = -- | the version handshake is in progress
    Handshaking !Handshake
  | -- | the handshake is complete; holds the version of the peer and the ping
    -- awaiting an answer, with the time it was sent and its nonce
    Online !Version !(Maybe (UTCTime, Word64))
  | -- | the peer was told to disconnect, for the given reason
    Disconnecting !DisconnectReason
  deriving stock (Eq, Show)

-- | Why the node disconnects from a peer.
data DisconnectReason
  = -- | the peer broke the handshake rules, or its version was refused
    Rejected !HandshakeError
  | -- | the handshake did not complete in time
    HandshakeTimeout
  | -- | the peer sent nothing for too long
    Silent
  | -- | the peer did not answer a ping in time
    NoPong
  | -- | the peer has been connected for the maximum time; connections rotate
    TooOld
  deriving stock (Eq, Show)

-- | State of a connection.
data PeerState = PeerState
  { -- | stage of the connection
    stage :: !Stage,
    -- | when the connection was opened
    connected :: !UTCTime,
    -- | when the peer last sent a message
    lastHeard :: !UTCTime,
    -- | round-trip times of the last eleven pings, newest first
    pings :: ![NominalDiffTime],
    -- | whether the peer was announced as connected, so that its
    -- disconnection must be announced too
    announced :: !Bool
  }
  deriving stock (Eq, Show)

-- | Time limits for a connection.
data Limits = Limits
  { -- | longest silence, unanswered ping or handshake
    timeout :: !NominalDiffTime,
    -- | longest connection, after which it is rotated
    maxLife :: !NominalDiffTime
  }
  deriving stock (Eq, Show)

-- | What happened to a connection.
data PeerInput
  = -- | the peer sent a message at the given time
    Heard !UTCTime
  | -- | the peer sent a handshake message
    GotHandshake !HandshakeEvent
  | -- | the version of the peer was refused (see 'checkVersion')
    Refused !HandshakeError
  | -- | the peer answered a ping with the given nonce at the given time
    GotPong !UTCTime !Word64
  | -- | the clock ticked; holds the time, and a nonce for a ping if one is due
    Tick !UTCTime !Word64
  deriving stock (Eq, Show)

-- | What the peer manager must do after a transition.
data PeerAction
  = -- | acknowledge the version of the peer
    ReplyVerAck
  | -- | publish that the peer is online
    Announce
  | -- | send a ping with the given nonce
    SendPing !Word64
  | -- | tell the peer process to disconnect
    Disconnect !DisconnectReason
  deriving stock (Eq, Show)

-- | A connection opened at the given time, with our version sent.
newPeerState :: UTCTime -> PeerState
newPeerState now =
  PeerState
    { stage = Handshaking AwaitingBoth,
      connected = now,
      lastHeard = now,
      pings = [],
      announced = False
    }

-- | Apply an input to a connection.
--
-- On a 'Tick', the checks come in this order: the connection is too old, the
-- peer has been silent too long, the handshake took too long, a ping went
-- unanswered too long. If none applies and no ping is pending, a ping is sent.
stepPeer :: Limits -> PeerInput -> PeerState -> (PeerState, [PeerAction])
stepPeer limits input st =
  case (st.stage, input) of
    (Disconnecting _, _) -> (st, [])
    (_, Heard t) -> (st {lastHeard = t}, [])
    (_, Refused e) -> disconnect (Rejected e)
    (Handshaking h, GotHandshake e) -> handshake h e
    (Online _ _, GotHandshake (GotVersion _)) -> disconnect (Rejected DuplicateVersion)
    (Online _ _, GotHandshake GotVerAck) -> disconnect (Rejected DuplicateVerAck)
    (Online v (Just (sent, n)), GotPong t n')
      | n == n' ->
          ( st
              { stage = Online v Nothing,
                pings = take 11 (t `diffUTCTime` sent : st.pings)
              },
            []
          )
    (_, GotPong _ _) -> (st, [])
    (stage, Tick now nonce)
      | now `since` st.connected > limits.maxLife -> disconnect TooOld
      | now `since` st.lastHeard > limits.timeout -> disconnect Silent
      | Handshaking _ <- stage,
        now `since` st.connected > limits.timeout ->
          disconnect HandshakeTimeout
      | Online _ (Just (sent, _)) <- stage,
        now `since` sent > limits.timeout ->
          disconnect NoPong
      | Online v Nothing <- stage ->
          (st {stage = Online v (Just (now, nonce))}, [SendPing nonce])
      | otherwise -> (st, [])
  where
    since = diffUTCTime
    disconnect r = (st {stage = Disconnecting r}, [Disconnect r])
    handshake h e =
      case stepHandshake h e of
        Left err -> disconnect (Rejected err)
        Right (Established v, acts) ->
          (st {stage = Online v Nothing, announced = True}, map action acts)
        Right (h', acts) -> (st {stage = Handshaking h'}, map action acts)
    action = \case
      SendVerAck -> ReplyVerAck
      AnnounceConnected -> Announce

-- | Whether the handshake is complete and the peer is not being disconnected.
isOnline :: PeerState -> Bool
isOnline st = case st.stage of
  Online _ _ -> True
  _ -> False

-- | Version of the peer, once received.
stageVersion :: PeerState -> Maybe Version
stageVersion st = case st.stage of
  Handshaking h -> handshakeVersion h
  Online v _ -> Just v
  Disconnecting _ -> Nothing

-- | The ping awaiting an answer: when it was sent, and its nonce.
pendingPing :: PeerState -> Maybe (UTCTime, Word64)
pendingPing st = case st.stage of
  Online _ p -> p
  _ -> Nothing
