-- |
-- Module      : Haskoin.Node.Peer.Request
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Receiving the answer to a request for blocks or transactions, as an
-- explicit state machine. 'Haskoin.Node.Peer.getData' feeds it the messages
-- that a peer sends after the request, and the disconnection of the peer.
--
-- = Why it works this way
--
-- Right after a getdata message, the node sends the peer a ping. Peers answer
-- messages in order, so every requested item must arrive, in the order
-- requested, before the matching pong. That gives clear rules:
--
-- * The next requested item moves the request forward.
--
-- * A requested item that is not the next one means the peer skipped
--   something, so the request fails.
--
-- * A notfound message naming a requested item, or the pong arriving early,
--   means the peer will not send everything, so the request fails.
--
-- * The disconnection of the peer ends the request at once ('Disconnected'):
--   nothing more can come, and waiting for the timeout would only hold up
--   the caller.
--
-- * Anything else is unrelated traffic and is ignored: a block, transaction
--   or notfound for an item that this request did not ask for, pings,
--   inventory and other chatter. Peers send such messages whenever they like,
--   and failing a long download because of one of them would only make the
--   application retry it.
--
-- Keeping these rules in a pure function means each one can be tested on its
-- own, without a connection.
--
-- = Requests do not see each other's answers
--
-- "Haskoin.Node.Peer" runs one request at a time on a peer, so two requests
-- never wait on the same stream of answers. That leaves the answers to a
-- request that ended before its pong came back: one that timed out, failed,
-- or got all its items. They are still on their way, and the next request
-- may ask for the same items. Its rules above would take an earlier answer
-- for a skipped item and fail.
--
-- So a request starts by draining: if the request before it on the peer is
-- still owed its pong, 'newRequest' takes the nonce of that ping, and the
-- request ignores everything until that pong arrives. Peers answer in order,
-- so whatever comes before it answers the earlier request, and whatever
-- comes after answers this one. The alternative, matching answers to a
-- request by their content alone, cannot tell a late copy of a block from
-- the one asked for now.
--
-- A block is matched to a request by the hash of its header only. That is
-- enough because the peer process checks every block before passing it on,
-- with 'Haskoin.Node.Block.blockMutation': a block that arrives here holds
-- the transactions its header commits to. A transaction is matched by its
-- id, which does not cover its witness.
module Haskoin.Node.Peer.Request
  ( Request,
    RequestInput (..),
    RequestStep (..),
    newRequest,
    stepRequest,
  )
where

import Data.Text (Text)
import Data.Word (Word64)
import Haskoin

-- | A request in progress: the nonce of the ping of the earlier request that
-- it waits for first, if any, the nonce of the ping that follows it, the
-- items still expected, and the items received so far, newest first.
data Request = Request
  { draining :: !(Maybe Word64),
    nonce :: !Word64,
    remaining :: ![InvVector],
    received :: ![Either Tx Block]
  }
  deriving stock (Eq, Show)

-- | What happens on the peer while a request waits.
data RequestInput
  = -- | the peer sent a message
    Received !Message
  | -- | the peer disconnected
    Disconnected
  deriving stock (Eq, Show)

-- | Outcome of feeding a message to a request.
data RequestStep
  = -- | more items are expected
    Continue !Request
  | -- | every item arrived, in the order requested; transactions are 'Left'
    -- and blocks 'Right'
    Complete ![Either Tx Block]
  | -- | the peer will not send what was requested
    Failed !Text
  deriving stock (Eq, Show)

-- | Start a request for the given items, followed by a ping with the given
-- nonce. The first argument is the nonce of the ping of the earlier request
-- on the peer, if that request is still owed its pong: everything before
-- that pong is ignored. A request for nothing is complete at once.
newRequest :: Maybe Word64 -> Word64 -> [InvVector] -> RequestStep
newRequest stale n ivs =
  check Request {draining = stale, nonce = n, remaining = ivs, received = []}

-- | Feed what happened to a request.
stepRequest :: Request -> RequestInput -> RequestStep
stepRequest _ Disconnected = Failed "peer disconnected"
stepRequest r (Received message) =
  case (r.draining, r.remaining, message) of
    (_, [], _) -> Complete (reverse r.received)
    (Just stale, _, MPong (Pong n))
      | n == stale -> Continue r {draining = Nothing}
      | n == r.nonce -> Failed "incomplete response"
    (Just _, _, _) -> Continue r
    (Nothing, next : _, MTx tx)
      | isTx next tx -> accept (Left tx)
      | any (`isTx` tx) r.remaining -> Failed "transaction out of order"
    (Nothing, next : _, MBlock b)
      | isBlock next b -> accept (Right b)
      | any (`isBlock` b) r.remaining -> Failed "block out of order"
    (Nothing, _, MNotFound (NotFound ivs))
      | any (`elem` r.remaining) ivs -> Failed "item not found"
    (Nothing, _, MPong (Pong n))
      | n == r.nonce -> Failed "incomplete response"
    _ -> Continue r
  where
    accept x =
      check r {remaining = drop 1 r.remaining, received = x : r.received}

check :: Request -> RequestStep
check r
  | null r.remaining = Complete (reverse r.received)
  | otherwise = Continue r

-- | Whether a transaction is the one an inventory vector asks for.
isTx :: InvVector -> Tx -> Bool
isTx iv tx =
  iv.invType `elem` [InvTx, InvWitnessTx] && iv.invHash == (txHash tx).get

-- | Whether a block is the one an inventory vector asks for.
isBlock :: InvVector -> Block -> Bool
isBlock iv b =
  iv.invType `elem` [InvBlock, InvWitnessBlock]
    && iv.invHash == (headerHash b.header).get
