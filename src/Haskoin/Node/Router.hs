-- |
-- Module      : Haskoin.Node.Router
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- The order in which the node passes on what each peer does, as an explicit
-- state machine. "Haskoin.Node" runs it in the loop that routes the events of
-- peers to the peer manager, the chain and the application.
--
-- = The problem
--
-- The events of a peer come from two threads. Its messages come from the
-- thread that reads its socket; that it is connected, and later that it is
-- gone, come from the peer manager. Both publish to the one queue that the
-- router reads, and nothing orders the two. The peer manager announces a
-- peer once it has handled the message that completes the handshake, and by
-- then the reader may have queued more messages of the peer, which reach the
-- router first. The router used to ask the peer manager whether the peer was
-- online, which it was, and pass those messages on before the announcement:
-- the chain dropped the headers of a peer it did not know yet, and the
-- application saw messages from a peer that had not connected.
--
-- = The rule
--
-- For every peer, those that the router passes events to see
-- 'Haskoin.Node.Peer.PeerConnected' before any message that needs it, and
-- 'Haskoin.Node.Peer.PeerDisconnected' after all of them. The router keeps a
-- t'Route' for each peer:
--
-- * __No route__: the handshake is not complete. A message is handled as
--   from a peer that is not online ('Early'): @version@ and @verack@ go to
--   the peer manager, pings are answered, and the rest is dropped, as Bitcoin
--   Core does.
--
-- * 'Awaiting': the handshake completed ('HandshakeDone', which the router
--   learns from the peer manager right after it handles the message that
--   completes it), but the announcement has not come through the queue yet.
--   Messages are held, in order.
--
-- * 'Announced': the announcement came ('Connected'). The held messages are
--   handled first, in order, then every new one, as from an online peer
--   ('Deliver').
--
-- * 'Disconnected' ends the route: the announcement of the disconnection is
--   passed on ('Retire') and the route forgotten.
--
-- Order comes from the single queue and the single thread that reads it, not
-- from timing: whatever a peer's events were interleaved with, the actions
-- for one peer come out in this order.
--
-- = Bounded
--
-- Held messages count against the allowance of the peer
-- ('Haskoin.Node.Peer.maxPendingMessages'): the peer stops reading its socket
-- until they are handled, so at most that many are held for a peer. Every
-- message that arrives is disposed of exactly once, by 'Early', 'Deliver' or
-- 'Discard', which is what the allowance of the peer needs to be given back
-- exactly once per message.
module Haskoin.Node.Router
  ( Router,
    Route (..),
    RouterInput (..),
    RouterAction (..),
    newRouter,
    stepRouter,
    routeOf,
  )
where

import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq

-- | Where a peer is in the order of its events. A peer without a route has
-- not completed the handshake.
data Route m
  = -- | the handshake completed and the announcement has not come yet; holds
    -- the messages that arrived since, oldest first
    Awaiting !(Seq m)
  | -- | the announcement came
    Announced
  deriving stock (Eq, Show)

-- | The routes of the peers, by a key that tells the peers apart. There are
-- as many as the peers with a complete handshake, so a list is enough, and
-- the key needs only equality.
newtype Router k m = Router [(k, Route m)]
  deriving stock (Eq, Show)

-- | What the router learns about a peer.
data RouterInput k m
  = -- | the peer sent a message
    Arrived !k !m
  | -- | the peer manager has completed the handshake of the peer and
    -- announced it; the announcement is on its way through the queue
    HandshakeDone !k
  | -- | the announcement that the peer is connected came through the queue
    Connected !k
  | -- | the announcement that the peer is gone came through the queue
    Disconnected !k
  deriving stock (Eq, Show)

-- | What the router does.
data RouterAction k m
  = -- | handle a message from a peer whose handshake is not complete
    Early !k !m
  | -- | handle a message from a peer that has been announced
    Deliver !k !m
  | -- | give back the allowance of a message and do nothing else with it
    Discard !k !m
  | -- | pass on that the peer is connected
    Announce !k
  | -- | pass on that the peer is gone
    Retire !k
  deriving stock (Eq, Show)

-- | A router that knows no peer.
newRouter :: Router k m
newRouter = Router []

-- | The route of a peer, if it has one.
routeOf :: (Eq k) => k -> Router k m -> Maybe (Route m)
routeOf k (Router rs) = lookup k rs

-- | Apply an input.
--
-- Inputs that the order of the queue rules out, such as a disconnection of
-- a peer that was never announced, still give a sensible answer: held
-- messages are discarded, and nothing is passed on about a peer that was
-- not announced.
stepRouter :: (Eq k) => RouterInput k m -> Router k m -> (Router k m, [RouterAction k m])
stepRouter input router@(Router rs) =
  case input of
    Arrived k m -> case lookup k rs of
      Nothing -> (router, [Early k m])
      Just (Awaiting q) -> (set k (Awaiting (q |> m)), [])
      Just Announced -> (router, [Deliver k m])
    HandshakeDone k -> case lookup k rs of
      Nothing -> (set k (Awaiting Seq.empty), [])
      Just _ -> (router, [])
    Connected k -> case lookup k rs of
      Nothing -> (set k Announced, [Announce k])
      Just (Awaiting q) -> (set k Announced, Announce k : map (Deliver k) (toList q))
      Just Announced -> (router, [])
    Disconnected k -> case lookup k rs of
      Nothing -> (router, [])
      Just (Awaiting q) -> (unset k, map (Discard k) (toList q))
      Just Announced -> (unset k, [Retire k])
  where
    set k r = Router ((k, r) : filter ((/= k) . fst) rs)
    unset k = Router (filter ((/= k) . fst) rs)
