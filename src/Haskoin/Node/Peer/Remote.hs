-- |
-- Module      : Haskoin.Node.Peer.Remote
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- What the node at the other end of a connection will do with a message,
-- before it is sent.
--
-- The peer-to-peer protocol is what Bitcoin Core and Bitcoin Cash Node do,
-- not what a specification says, and they punish a peer for things that no
-- BIP mentions. A @mempool@ message, for example, closes the connection
-- unless the node offers Bloom filters or whitelisted the peer. haskoin-core
-- translates both nodes into pure state machines ("Haskoin.Network.Protocol"),
-- each modelling how its node treats one peer. A t'Remote' runs that machine
-- for one connection, with this node as the peer, and gives a t'Judgement' of
-- each message this node is about to send.
--
-- "Haskoin.Node.Peer" judges every message on its way out. One that the node
-- would punish this node for is not sent ('Refused'), and a request that the
-- node would drop without an answer fails at once ('Unanswered'), rather than
-- wait for a timeout.
--
-- = What the model knows, and what it guesses
--
-- A machine reads the state of the node from an
-- t'Haskoin.Network.Protocol.Env': its chain, its
-- mempool, its other peers. This node knows none of it for certain. It knows
-- its own chain of headers, which follows the node's, and the version message
-- of the node. So a t'Remote' is exact about the rules of the protocol, and
-- generous about the chain:
--
-- * The options of the node come from its version message ('learnVersion'):
--   whether it offers Bloom filters, whether it is pruned, whether it relays
--   transactions. Until that message arrives, the machine has the defaults
--   of the node. When it arrives, the machine starts again with the options
--   it gives, and the messages sent so far.
-- * The permissions of this node, which the operator of the other node grants
--   with @-whitelist@ or @-whitebind@, are configured: nothing in the protocol
--   tells a peer about them.
-- * The node is taken to have every block that this node asks for, at the
--   height of this node's header of it when there is one. A judgement never
--   says that a request goes unanswered because the node lacks a block. This
--   node's chain is a lower bound of the node's, not a copy of it, and a
--   guess that the node lacks a block would fail requests that it serves.
-- * A disconnection that depends on what is guessed, such as the node
--   running out of upload bandwidth or having pruned a block, is left to
--   happen. A message is refused only for what the rules decide whatever the
--   chain holds.
--
-- = Why not a list of rules here
--
-- haskoin-node used to know such rules one at a time, written where they were
-- needed, and learned most of them when a node disconnected it. The machines
-- of haskoin-core cite the upstream line behind each decision, and follow the
-- releases of both nodes. A rule they learn reaches this node with the next
-- version of haskoin-core.
module Haskoin.Node.Peer.Remote
  ( -- * The model of a connection
    Remote,
    newRemote,
    remoteSetup,
    remoteConnection,
    learnVersion,
    learnedVersion,

    -- * Judging messages
    Facts (..),
    Judgement (..),
    judge,
    predict,
    judgementText,

    -- * What the node answers
    expectsReject,
  )
where

import Data.Bits ((.&.))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Haskoin
import Haskoin.Network.Protocol qualified as P
import Haskoin.Network.Protocol.BitcoinCashNode qualified as BCHN
import Haskoin.Network.Protocol.BitcoinCore qualified as Core

-- | The model of the node at the other end of one connection.
data Remote = Remote
  { -- | the rules of the node, with its options
    machine :: !P.Machine,
    -- | the connection as the node sees it
    setup :: !P.Setup,
    -- | the version of the node, once it arrived
    version :: !(Maybe Version),
    -- | the messages sent before the version of the node arrived, newest
    -- first, with the facts they were judged with, to run again with the
    -- options that the version gives
    replay :: ![(P.Env, Message)],
    -- | the state of the machine
    connection :: !P.Connection
  }

-- | A model for a connection to a node of this network. This node opened
-- the connection, so it is an inbound peer of the node.
newRemote ::
  Network ->
  -- | the permissions that the node grants this node, as configured
  P.Permissions ->
  -- | the address network of the connection, as the node sees it
  AddressNetwork ->
  -- | whether the node sees this node at a local address, as over a Unix
  -- socket or the loopback interface
  Bool ->
  -- | the time the connection opened
  P.Micros ->
  -- | a seed for the randomness of the machine
  Word64 ->
  Remote
newRemote net perms addrNet local opened seed =
  Remote
    { machine = m,
      setup = s,
      version = Nothing,
      replay = [],
      connection = P.start m s
    }
  where
    m = P.machine net
    s =
      P.Setup
        { connectionType = P.Inbound,
          permissions = perms,
          peerNetwork = addrNet,
          peerIsLocal = local,
          transport = P.TransportV1,
          openedAt = opened,
          seed = seed
        }

-- | The setup of the connection that the model runs.
remoteSetup :: Remote -> P.Setup
remoteSetup r = r.setup

-- | The state of the machine, to inspect with the functions of
-- "Haskoin.Network.Protocol.BitcoinCore" and
-- "Haskoin.Network.Protocol.BitcoinCashNode".
remoteConnection :: Remote -> P.Connection
remoteConnection r = r.connection

-- | The version message of the node, once 'learnVersion' has seen it.
learnedVersion :: Remote -> Maybe Version
learnedVersion r = r.version

-- | Take the options of the node from its version message, and run the
-- messages sent so far again with them. Only the first version counts: a
-- second one is a fault of the node, which "Haskoin.Node.PeerMgr" handles.
--
-- A version message tells the services of the node, which say whether it
-- offers Bloom filters (@NODE_BLOOM@) and whether it is pruned
-- (@NODE_NETWORK_LIMITED@ without @NODE_NETWORK@), and its relay flag, which
-- a node in blocks-only mode clears.
learnVersion :: Version -> Remote -> Remote
learnVersion v r
  | Just _ <- r.version = r
  | otherwise =
      let m = adjust r.machine
          again c (e, out) = fst (P.step e c (P.Receive out))
          c' = foldl again (P.start m r.setup) (reverse r.replay)
       in r {machine = m, version = Just v, replay = [], connection = c'}
  where
    bloom = v.services .&. nodeBloom /= 0
    pruned = v.services .&. nodeNetwork == 0 && v.services .&. nodeNetworkLimited /= 0
    blocksOnly = not v.relay
    adjust = \case
      P.BitcoinCore o ->
        P.BitcoinCore
          o {Core.peerBloomFilters = bloom, Core.prune = pruned, Core.blocksOnly = blocksOnly}
      P.BitcoinCashNode o ->
        P.BitcoinCashNode
          o {BCHN.peerBloomFilters = bloom, BCHN.prune = pruned, BCHN.blocksOnly = blocksOnly}

-- | What this node knows of the chain when it judges a message: the time,
-- its best header, and its headers of the blocks that the message names.
data Facts = Facts
  { now :: !P.Micros,
    best :: !BlockNode,
    known :: !(Map BlockHash BlockNode)
  }

-- | What the node will do with a message.
data Judgement
  = -- | the node acts on it, and answers it if it is a request
    Allowed
  | -- | the node drops it without an answer, for this reason
    Unanswered !P.Cite
  | -- | the node would punish this node for it, or close the connection:
    -- what it would do, and why
    Refused !P.Action !P.Cite
  deriving (Eq, Show)

-- | Judge a message before it is sent, and give the model as it is after the
-- node got it. Keep the new model only if the message is sent.
--
-- A message is 'Refused' when the node would add to this node's ban score,
-- discourage it, ban it, or close the connection for breaking a rule of the
-- protocol, for misbehaviour, or because this node lacks what the connection
-- needs. It is not refused for a disconnection that depends on the guessed
-- state of the node, such as its upload limit.
--
-- A message is 'Unanswered' when the node only drops it: no message goes
-- back, and nothing else happens.
judge :: Facts -> Message -> Remote -> (Judgement, Remote)
judge facts out r = let (effects, r') = predict facts out r in (classify effects, r')

-- | What the node would do with a message, effect by effect, and the model
-- as it is after the node got it: what 'judge' decides from. The tests use
-- it to compare the model with a running node.
predict :: Facts -> Message -> Remote -> ([P.Effect], Remote)
predict facts out r =
  (effects, r {connection = c, replay = replay'})
  where
    env = factsEnv (P.networkOf r.machine) facts
    (c, effects) = P.step env r.connection (P.Receive out)
    replay' = case r.version of
      Nothing -> (env, out) : r.replay
      Just _ -> []

classify :: [P.Effect] -> Judgement
classify effects =
  case mapMaybe punished effects of
    e : _ -> Refused e.action e.because
    []
      | any (sends . (.action)) effects -> Allowed
      | e : _ <- filter ((== P.Ignore) . (.action)) effects,
        all ((== P.Ignore) . (.action)) effects ->
          Unanswered e.because
      | otherwise -> Allowed
  where
    punished e = case e.action of
      P.Misbehave n | n > 0 -> Just e
      P.Discourage -> Just e
      P.Ban _ -> Just e
      P.Disconnect why | why `elem` [P.ProtocolViolation, P.Misbehaviour, P.Unsuitable] -> Just e
      _ -> Nothing
    sends = \case
      P.Send _ -> True
      _ -> False

-- | A judgement in words, for the log, with the upstream line behind it.
judgementText :: Judgement -> Text
judgementText = \case
  Allowed -> "the node takes it"
  Unanswered c -> "the node drops it without an answer: " <> citeText c
  Refused a c -> "the node would " <> actionText a <> ": " <> citeText c
  where
    citeText c = c.reason <> " (" <> T.pack c.file <> ":" <> T.pack (show c.line) <> ")"
    actionText = \case
      P.Misbehave n -> "add " <> T.pack (show n) <> " to the ban score of this node"
      P.Discourage -> "discourage this node"
      P.Ban s -> "ban this node for " <> T.pack (show s) <> " seconds"
      P.Disconnect why -> "disconnect (" <> T.pack (show why) <> ")"
      other -> T.pack (show other)

-- | The environment of the machine, from what this node knows. The node is
-- out of initial block download, its tip is this node's best header, and it
-- has the data of every block that it is asked for. A block that this node
-- knows has its height and time; another one is taken to be at the tip. The
-- content of a block is never needed to judge a request for it, so the
-- genesis block stands in for every block.
factsEnv :: Network -> Facts -> P.Env
factsEnv net facts =
  (P.defaultEnv tip (genesisBlock net) facts.now)
    { P.initialBlockDownload = False,
      P.lookupBlock = Just . lookupInfo,
      P.headerOf = \h -> (.header) <$> Map.lookup h facts.known,
      P.blockData = const (Just (genesisBlock net)),
      P.blockAtHeight = \n ->
        if n == facts.best.height
          then Just tip
          else info <$> atHeight n,
      P.bestHeader = tip
    }
  where
    info n =
      P.BlockInfo
        { hash = headerHash n.header,
          height = n.height,
          status = P.HaveData,
          inActiveChain = True,
          work = n.work,
          time = n.header.timestamp
        }
    tip = info facts.best
    lookupInfo h = maybe (tip {P.hash = h} :: P.BlockInfo) info (Map.lookup h facts.known)
    atHeight n = case filter ((== n) . (.height)) (Map.elems facts.known) of
      x : _ -> Just x
      [] -> Nothing

-- | Whether the node answers a transaction that its mempool refuses with a
-- @reject@ message (BIP 61): Bitcoin Cash Node does by default, and Bitcoin
-- Core never does. The machine of the network is asked, not a table: a
-- connection is opened with this version message, its handshake completed,
-- and a transaction that the mempool refuses by policy is sent.
expectsReject :: Network -> Version -> Bool
expectsReject net ours =
  any isReject (snd (P.step refusing handshaken (P.Receive (MTx net.genesisTx))))
  where
    best = genesisNode net
    facts = Facts {now = 1, best = best, known = Map.empty}
    env = factsEnv net facts
    refusing = env {P.checkTx = const (P.TxPolicyReject "policy")}
    remote = newRemote net Set.empty AddrIPv4 False 1 0
    handshaken =
      foldl
        (\c out -> fst (P.step env c (P.Receive out)))
        (remoteConnection remote)
        [MVersion ours, MVerAck]
    isReject e = case e.action of
      P.Send (MReject _) -> True
      _ -> False
