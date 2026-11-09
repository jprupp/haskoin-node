-- |
-- Module      : Haskoin.Node.PeerMgr.Handshake
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- The version handshake with a peer, as an explicit state machine.
--
-- The node sends its version message as soon as a connection opens. The peer
-- must answer with its own version and acknowledge ours with a verack, in
-- either order. Only once both have arrived is the peer usable, and only then
-- does "Haskoin.Node.PeerMgr" announce it with a
-- 'Haskoin.Node.Peer.PeerConnected' event.
--
-- = Why a state machine
--
-- The handshake used to be tracked with independent flags, one for the
-- version and one for the verack, with each message handler checking the
-- flags it cared about. Every handler had to remember every rule, and a
-- combination nobody thought of, such as a second verack, would silently do
-- something. Here the states are listed once, 'stepHandshake' says what each
-- message does in each state, and a message that makes no sense in the
-- current state is an error. Unforeseen sequences are rejected instead of
-- handled by accident, and the rules can be tested exhaustively without a
-- network.
--
-- The module is pure. "Haskoin.Node.PeerMgr" stores the state of each peer
-- and carries out the actions that a transition returns.
module Haskoin.Node.PeerMgr.Handshake
  ( Handshake (..),
    HandshakeEvent (..),
    HandshakeAction (..),
    HandshakeError (..),
    stepHandshake,
    handshakeVersion,
    isEstablished,
    hasVerAck,
    checkVersion,
  )
where

import Data.Bits ((.&.))
import Data.Word (Word64)
import Haskoin (Network (..), Version (..), nodeNetwork, nodeWitness)

-- | How far the handshake with a peer has progressed.
data Handshake
  = -- | Our version was sent; nothing has been received yet.
    AwaitingBoth
  | -- | The version of the peer arrived; its verack has not.
    AwaitingVerAck !Version
  | -- | The verack of the peer arrived; its version has not.
    AwaitingVersion
  | -- | Both arrived. The peer is online.
    Established !Version
  deriving stock (Eq, Show)

-- | Handshake messages received from a peer.
data HandshakeEvent
  = -- | the peer sent its version
    GotVersion !Version
  | -- | the peer acknowledged our version
    GotVerAck
  deriving stock (Eq, Show)

-- | What the peer manager must do after a transition.
data HandshakeAction
  = -- | acknowledge the version of the peer with a verack
    SendVerAck
  | -- | publish that the peer is online
    AnnounceConnected
  deriving stock (Eq, Show)

-- | Reasons to disconnect a peer during the handshake.
data HandshakeError
  = -- | the peer sent a second version message
    DuplicateVersion
  | -- | the peer sent a second verack
    DuplicateVerAck
  | -- | the peer does not serve the full block chain
    NotFullNode
  | -- | the peer does not serve witness data, on a network with SegWit
    NotWitnessNode
  | -- | the version carries one of our own nonces: we connected to ourselves
    ConnectedToSelf
  deriving stock (Eq, Show)

-- | Apply a message to the handshake. Returns the new state and the actions to
-- carry out, or why the peer must be disconnected.
--
-- Each message may arrive once. A second version or verack is a protocol
-- violation: well-behaved peers never send one, and accepting them would let
-- a peer announce itself as connected twice.
stepHandshake ::
  Handshake ->
  HandshakeEvent ->
  Either HandshakeError (Handshake, [HandshakeAction])
stepHandshake state event =
  case (state, event) of
    (AwaitingBoth, GotVersion v) -> Right (AwaitingVerAck v, [SendVerAck])
    (AwaitingBoth, GotVerAck) -> Right (AwaitingVersion, [])
    (AwaitingVerAck v, GotVerAck) -> Right (Established v, [AnnounceConnected])
    (AwaitingVersion, GotVersion v) ->
      Right (Established v, [SendVerAck, AnnounceConnected])
    (AwaitingVerAck _, GotVersion _) -> Left DuplicateVersion
    (AwaitingVersion, GotVerAck) -> Left DuplicateVerAck
    (Established _, GotVersion _) -> Left DuplicateVersion
    (Established _, GotVerAck) -> Left DuplicateVerAck

-- | The version of the peer, once it has arrived.
handshakeVersion :: Handshake -> Maybe Version
handshakeVersion = \case
  AwaitingVerAck v -> Just v
  Established v -> Just v
  AwaitingBoth -> Nothing
  AwaitingVersion -> Nothing

-- | Whether the handshake is complete.
isEstablished :: Handshake -> Bool
isEstablished = \case
  Established _ -> True
  _ -> False

-- | Whether the peer acknowledged our version.
hasVerAck :: Handshake -> Bool
hasVerAck = \case
  AwaitingVersion -> True
  Established _ -> True
  _ -> False

-- | Check the version of a peer before accepting it. Takes the network and
-- the nonces of the version messages that we sent to all our peers.
--
-- A peer must serve the full chain ('nodeNetwork'), because the node syncs
-- headers and applications download blocks from it. On a network with SegWit
-- it must also serve witness data ('nodeWitness'), because
-- 'Haskoin.Node.Peer.getBlocks' and 'Haskoin.Node.Peer.getTxs' ask for
-- blocks and transactions with their witness. Bitcoin Core
-- 32.1 requires both of its outbound peers (@GetDesirableServiceFlags@). A
-- version carrying one of our nonces means the node connected to itself,
-- which happens when it learns its own address from gossip.
checkVersion :: Network -> [Word64] -> Version -> Either HandshakeError Version
checkVersion net nonces v
  | v.services .&. nodeNetwork == 0 = Left NotFullNode
  | net.segWit && v.services .&. nodeWitness == 0 = Left NotWitnessNode
  | v.nonce `elem` nonces = Left ConnectedToSelf
  | otherwise = Right v
