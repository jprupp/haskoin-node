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
-- that a peer sends after the request.
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
-- * Anything else is unrelated traffic and is ignored. Peers send pings,
--   inventory and other chatter whenever they like, and failing a long
--   download because of one of them would only make the application retry it.
--
-- Keeping these rules in a pure function means each one can be tested on its
-- own, without a connection.
--
-- A block is matched to a request by the hash of its header only. That is
-- enough because the peer process checks every block before passing it on,
-- with 'Haskoin.Node.Block.blockMutation': a block that arrives here holds
-- the transactions its header commits to. A transaction is matched by its
-- id, which does not cover its witness.
module Haskoin.Node.Peer.Request
  ( Request,
    RequestStep (..),
    newRequest,
    stepRequest,
  )
where

import Data.Text (Text)
import Data.Word (Word64)
import Haskoin

-- | A request in progress: the nonce of the ping that follows it, the items
-- still expected, and the items received so far, newest first.
data Request = Request
  { nonce :: !Word64,
    remaining :: ![InvVector],
    received :: ![Either Tx Block]
  }
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
-- nonce. A request for nothing is complete at once.
newRequest :: Word64 -> [InvVector] -> RequestStep
newRequest n ivs = check Request {nonce = n, remaining = ivs, received = []}

-- | Feed a message from the peer to a request.
stepRequest :: Request -> Message -> RequestStep
stepRequest r message =
  case (r.remaining, message) of
    ([], _) -> Complete (reverse r.received)
    (next : _, MTx tx)
      | isTx next tx -> accept (Left tx)
      | any (`isTx` tx) r.remaining -> Failed "transaction out of order"
    (next : _, MBlock b)
      | isBlock next b -> accept (Right b)
      | any (`isBlock` b) r.remaining -> Failed "block out of order"
    (_, MNotFound (NotFound ivs))
      | any (`elem` r.remaining) ivs -> Failed "item not found"
    (_, MPong (Pong n))
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
