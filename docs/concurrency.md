# Concurrency in haskoin-node

haskoin-node is a handful of threads that share a little state. This document
says, for each of them, what it owns, what decides what it does, and why no
two threads can get in each other's way. It is for anyone who changes the
code: read the part about a protocol before you change it.

## The rule

Every concurrent protocol is a pure state machine. Its transitions are a
function in a module of its own, tested with QuickCheck through arbitrary
sequences of events. The threads only store the state and carry out the
actions that a transition gives back. Races are ruled out by construction, in
one of three ways:

- **A single owner.** One thread changes the state; others at most read it.
- **One transaction per decision.** The state is read, the transition run and
  the new state written in one STM transaction, so two threads cannot both
  act on the same old state.
- **The order of a single queue.** Events that must keep an order go
  through one queue that one thread reads.

Time and randomness are inputs of the transitions, never read inside them.

## The threads

| Thread | Runs | Owns |
| --- | --- | --- |
| Peer writer | `Haskoin.Node.Peer.peer`, one per connection | the socket's write side |
| Peer reader | the same, in a linked thread | the socket's read side |
| Housekeeping | `withHousekeeping`, one per connection | nothing: sends `CheckPeer` ticks |
| Peer manager | `Haskoin.Node.PeerMgr.peerManager` | the list of connections and their `PeerState`, the address book |
| Connect loop | `withConnectLoop` | nothing: sends `Connect` |
| Chain | `Haskoin.Node.Chain` | the header database, `ChainState` |
| Router | `Haskoin.Node.peerEvents` | its `Router`: the order of each peer's events |
| Chain events | `Haskoin.Node.chainEvents` | nothing: forwards |
| Publishers | NQE, one for peer events, one for chain events, the application's | their subscriber lists |
| Callers | the application: `getData`, `pingPeer`, `sendMessage` | their own request |

## Peer process

Two threads per connection, and a `Peer` handle that other threads use.

- **Writer.** Takes `SendMessage`, `NotSent` and `KillPeer` from the mailbox,
  in order, and writes. It owns the write side.
- **Reader.** Reads and checks messages, waits for room in the allowance,
  and publishes each message. It owns the read side.
- **Closing.** The connection runs in a race against the `closing` flag of
  the handle. `killPeer` sets it, together with queuing `KillPeer`, in one
  transaction, and the process sets it as it ends, however it ends.

The state in the handle:

| State | Changed by | How |
| --- | --- | --- |
| `remote` (the model) | `sendMessage`; the reader on `version` | one transaction per message: judge and commit |
| `pending` (allowance) | up: the reader; down: the router | the reader waits for room and counts in one transaction |
| `busy` (reservation) | `reservePeer`, `releasePeer` | one transaction each, rules in `Haskoin.Node.Peer.Lock` |
| `closing` | `killPeer`; the process as it ends | a flag that only goes from `False` to `True` |
| `requestLock` | `getData` | held for the whole request |
| `owed` | set by `getData` as it queues its ping; cleared by the reader | see Requests |

**Audit: a kill could be lost.** `killPeer` only queued `KillPeer`. A peer
that stopped reading left the writer blocked in a write, `KillPeer` was never
taken, and the process never ended. **Now** the process races its
connection against `closing`, so it stops at once, and the peer manager
cancels the thread if it is still there after a timeout (see Peer manager).
`killPeer` no longer waits for messages queued before it.

## Remote model

`Haskoin.Node.Peer.Remote` runs haskoin-core's protocol machine of the node
at the other end. `sendMessage` judges a message and commits the new model
in one transaction, so the model sees messages in the order they are queued.

**Audit: time could go backwards.** The time in the facts of a message is
read before the transaction. Two threads sending at once could commit in the
other order than they read the clock. **Now** the model keeps the latest time
it has seen (`remoteClock`), and every judgement uses the later of that and
the time of its facts. The clamp is part of the transition, so of the one
transaction. A QuickCheck property feeds times in any order and checks that
the clock never goes down.

## Allowance of pending messages

The reader stops when a peer has `maxPendingMessages` (16) messages
published and not handled. It counts a message up in the transaction that
waits for room; the router counts it down once it is handled
(`peerMessageDone`).

**Invariant.** Each `PeerMessage` is given back exactly once, by its one
consumer. The router's machine disposes of every message exactly once, by
`Early`, `Deliver` or `Discard`, and a property checks it over arbitrary
interleavings. The count never goes below zero (`max 0`), so a stray extra
call cannot give a peer more than its allowance, but the invariant is what
keeps the count right; the clamp is a guard, not a rule to rely on.

**Audit.** The `max 0` hid the question of a double decrement. **Now** the
invariant is documented on `peerMessageDone` and checked on the router. A
token in each message, to make a second call impossible, was left out: it
would change `PeerEvent` for every consumer, for a rule that one place keeps.

## Peer manager

`Haskoin.Node.PeerMgr` is a single thread. It is the only writer of the list
of connections, and changes the state of a connection with
`Haskoin.Node.PeerMgr.Lifecycle.stepPeer` in one transaction (`peerInput`),
then carries out the actions: reply with `verack`, announce, ping,
disconnect, abort. Other threads read the list (`getPeers`,
`getOnlinePeer`).

A connection is `Handshaking`, `Online` or `Disconnecting`. Ticks come from
the housekeeping thread of each connection, every quarter to half of the
timeout.

**Audit: a disconnection could last forever.** `Disconnecting` ignored every
input, the clock too. If the process never stopped, the slot, the network
group and the `PeerDisconnected` were lost. **Now** `Disconnecting` records
when it began, and a tick more than the timeout later gives `Abort`: the
peer manager cancels the thread of the process (in a thread of its own,
since `cancel` waits), which then dies, and `PeerDied` follows as for any
end. `Abort` comes again after each further timeout. Properties check that
an abort always follows a disconnection that has not ended a timeout later,
and that `Abort` only comes after `Disconnect`, for the same reason.

## Router

`Haskoin.Node.peerEvents` reads the one queue of peer events and passes them
to the peer manager, the chain and the application. Its machine is
`Haskoin.Node.Router`.

**Audit: messages before the announcement.** A peer's messages come from its
reader, and `PeerConnected` from the peer manager, which publishes it after
handling the message that completes the handshake. By then the reader may
have queued more messages, ahead of the announcement. The router asked
whether the peer was online, which it was, and passed them on first: the
chain dropped the headers of a peer it did not know yet. A test reproduced it
on every run.

**Now** the router keeps a route for each peer:

- No route: the handshake is not complete. `version` and `verack` go to the
  peer manager, which the router waits for; pings are answered; the rest is
  dropped.
- `Awaiting`: right after the message that completes the handshake, the
  router asks the peer manager whether it announced the peer. If it did,
  messages are held, in order, until `PeerConnected` comes through the queue.
- `Announced`: the held messages, then every new one, are handled as from an
  online peer.
- `PeerDisconnected` ends the route.

The order is that of the single queue and the single thread that reads it.
Held messages count against the allowance, so at most 16 are held for a
peer. Properties over arbitrary interleavings of three peers check that
`PeerConnected` comes before any message past the handshake and
`PeerDisconnected` after all of them, that messages keep their order, and
that none past the handshake is dropped. `version` and `verack` are still
published before `PeerConnected`: they are the handshake itself.

## Chain

`Haskoin.Node.Chain` is a single thread, the only writer of `ChainState` and
of the database. Which peer syncs is `Haskoin.Node.Chain.Sync`, a pure
machine. Headers and inventory come from the router, in the order the peer
sent them; the router's handling of the next message of the same peer waits
in the chain's mailbox (`chainThen`), so the chain sees a peer's headers in
order. The chain reserves the peer it syncs from (`reservePeer`) and drops
peers with `killPeer`.

## Reservations

`Haskoin.Node.Peer.Lock` decides who holds a peer for a long conversation.
Each reservation or release is one transaction. Only the holder can
release.

## Requests

`getData`, `getBlocks` and `getTxs` feed what the peer sends to
`Haskoin.Node.Peer.Request`. After the request, the node sends a ping;
peers answer in order, so every item comes before the pong.

**Audit.**

- A request ignored the disconnection of its peer and waited for its
  timeout. So did `pingPeer`.
- Two requests at once on one peer saw each other's blocks and failed with
  "out of order". The late answers of a request that had timed out did the
  same to the next one.

**Now:**

- `Disconnected` is an input of the machine: a request ends at once when
  `PeerDisconnected` comes for its peer, and at once if the peer is closing
  when it starts. `pingPeer` does the same.
- Requests run one at a time per peer, under `requestLock`. The timeout
  counts from the call, the wait for the lock included.
- The nonce of the ping of the last request (`owed`) is set in the
  transaction that queues that ping, and cleared by the reader in the
  transaction that counts its pong, before the pong is published. A new
  request subscribes, then reads `owed`: if it is set, the pong is still to
  be published, and the request ignores everything until it comes, since all
  that comes before it answers the earlier request.
- A block, transaction or notfound for an item that the request did not ask
  for is ignored. One that it asked for, out of order, still fails it.

## Known limits

- **The application's subscription is unbounded.** NQE publishers do not
  wait for their subscribers, and a subscription is an unbounded queue. A
  subscriber that falls behind holds every event it has not read. The
  allowance bounds what each peer can make the node buffer inside, not what
  the application leaves unread. This is by design, and documented in the
  README.
- `getData`'s own subscription to the peer events is unbounded too, for the
  length of a request.
- A peer outside a peer manager, as in the tests of `Haskoin.Node.Peer`,
  publishes no `PeerDisconnected`, so a request on it ends at its timeout if
  the peer dies during the request.
