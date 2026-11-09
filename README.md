# Haskoin Node

[![CI](https://github.com/jprupp/haskoin-node/actions/workflows/ci.yml/badge.svg)](https://github.com/jprupp/haskoin-node/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/haskoin-node.svg)](https://hackage.haskell.org/package/haskoin-node)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jprupp/haskoin-node/blob/master/LICENSE)

Haskoin Node is a peer-to-peer networking library for Bitcoin (BTC) and Bitcoin
Cash (BCH), written in Haskell. It connects to the network, keeps a pool of
healthy peers, and syncs block headers into a [RocksDB](https://rocksdb.org)
database.

It does not download or validate blocks and transactions by itself. It
publishes everything that happens as events, and gives you the peers to ask for
the data you need. It is the networking layer of
[Haskoin Store](https://github.com/jprupp/haskoin-store).

- [API documentation](https://hackage.haskell.org/package/haskoin-node)
- [Changelog](https://github.com/jprupp/haskoin-node/blob/master/CHANGELOG.md)
- [Report a bug](https://github.com/jprupp/haskoin-node/issues)

## Contents

- [Why Haskoin Node](#why-haskoin-node)
- [Design principles](#design-principles)
- [Features](#features)
- [Installation](#installation)
- [Quick start](#quick-start)
- [Tutorial: a block watcher](#tutorial-a-block-watcher)
- [Concepts](#concepts)
- [Usage guide](#usage-guide)
- [Configuration](#configuration)
- [Module overview](#module-overview)
- [Security notes](#security-notes)
- [Development](#development)
- [Related projects](#related-projects)
- [License](#license)

## Why Haskoin Node

A program that wants Bitcoin data has two usual options. It can run a full node
such as Bitcoin Core next to it and talk to its RPC interface, which means
operating a second large piece of software. Or it can speak the peer-to-peer
protocol itself, which means solving the same problems every node solves:
finding peers, keeping good ones, dropping bad ones, parsing untrusted
messages safely, and following the header chain through reorganisations.

Haskoin Node solves those problems once, as a library. It gives a Haskell
program a direct, well-behaved presence on the network, and stops there: what
to download, what to validate and what to store is up to the program. It is
the networking layer of [Haskoin Store](https://github.com/jprupp/haskoin-store),
which indexes the whole Bitcoin and Bitcoin Cash block chains this way without
a full node.

## Design principles

- **Do one job.** Keep connected and keep the header chain current. Blocks,
  transactions, validation and storage of anything but headers belong to the
  application, because every application wants something different.
- **Headers are the source of truth.** Headers are small (80 bytes) and carry
  the proof of work. With them, the node knows which chain is real without
  downloading blocks, and can tell the application which blocks to fetch.
- **Tell, don't ask.** Everything the node learns becomes an event on a
  publisher. The application subscribes and reacts; the node never needs to know
  who is listening.
- **Peers are strangers.** Message sizes are checked before payloads are read,
  a payload costs time linear in its size however it is split, anything
  invalid ends the connection, a block must hold the transactions its header
  commits to, relayed addresses are filtered and spread over network groups,
  and the node reads a peer only as fast as it handles its messages. Your
  subscribers are not throttled: one that falls behind holds memory.
- **Prefer good peers, but keep moving.** Peers are ranked by measured latency
  so the fastest are used first, and every connection is eventually replaced so
  the node never depends on the same few peers forever.
- **Small, isolated components.** The peer manager, the header chain and each
  connection are separate [NQE](https://github.com/jprupp/nqe) processes with
  their own state. One misbehaving peer dies alone.
- **Explicit protocol states.** What the node expects from each peer, during the
  handshake, while syncing headers and while receiving requested data, is
  written down as a pure state machine. A message that does not fit the current
  state is refused instead of handled by accident, and every rule is tested
  without a network.
- **Plain `IO`.** Every function runs in `IO`, with no monad transformers and
  no `MonadIO` or `MonadUnliftIO` constraints. Types say exactly what runs, GHC
  has nothing to specialise, and long-running loops cannot leak through an
  unspecialised dictionary. From your own monad, call the node with `liftIO`.
- **Keep moving.** Every hint that a peer has blocks the node lacks makes it
  ask that peer for headers, at once even while another peer syncs, and a
  quiet period makes it ask every peer, so a missed announcement cannot leave
  the chain behind. A syncing peer loses its turn when it sends nothing new,
  and when it runs past the deadline that Bitcoin Core gives its initial
  headers sync.

## Features

| Area            | What you get                                                                                   |
| --------------- | ---------------------------------------------------------------------------------------------- |
| Networks        | Every network of haskoin-core: Bitcoin and Bitcoin Cash mainnet, testnets and regtest           |
| Peer discovery  | DNS seeds and addresses relayed by peers, or a static list of peers                             |
| Peer management | Version handshake, ping-based latency, peers sorted by median ping, rotation after a maximum age, retry backoff and bans for bad addresses, random choice among learned addresses with one connection per network group |
| Header sync     | Headers downloaded from one peer at a time, with a deadline per turn, connected to the chain with the most work, stored in RocksDB; new blocks followed whether peers announce them with headers or inventory, announcing peers asked at once, and peers polled after a quiet period |
| Events          | Best header changes, sync completion, peer connections and every message received              |
| Data requests   | Fetch blocks and transactions from a peer, with a timeout; every block checked against its header's Merkle root and witness commitment |
| Protection      | Strict parsing, per-command size limits, linear-time reads, back-pressure, bounded address pool, explicit handshake states, connect timeout |

### What is not included

- No block or transaction validation, and no mempool. Haskoin Store does this
  on top of the events.
- No inbound connections: the node only connects out.
- No compact blocks, Bloom filters (BIP-37) or compact block filters (BIP-157).

## Installation

### System libraries

Haskoin Node needs [RocksDB](https://rocksdb.org) and
[libsecp256k1](https://github.com/bitcoin-core/secp256k1) 0.2.0 or later, with
`pkg-config` to find them.

```sh
# Debian and Ubuntu
sudo apt-get install librocksdb-dev libsecp256k1-dev pkg-config

# Fedora
sudo dnf install rocksdb-devel libsecp256k1-devel pkgconf

# macOS with Homebrew
brew install rocksdb secp256k1 pkg-config

# Nix
nix-shell -p rocksdb secp256k1 pkg-config
```

Some older distributions ship a libsecp256k1 below 0.2.0. See the
[haskoin-core installation notes](https://github.com/jprupp/haskoin-core#system-library)
for how to build it.

### Check the libraries are found

```sh
pkg-config --modversion libsecp256k1   # 0.2.0 or later
ls /usr/include/rocksdb/c.h            # or $(brew --prefix)/include/rocksdb/c.h
```

### Compiler

It needs GHC 9.2 or later, because it uses `OverloadedRecordDot`. It is built
and tested with the GHC of the Stackage nightly in `stack.yaml`.

### With Cabal

```cabal
build-depends:
    base
  , data-default
  , haskoin-core
  , haskoin-node
  , nqe
  , rocksdb-haskell-jprupp
```

You open the database with `rocksdb-haskell-jprupp` and create the event
publisher with `nqe`.

### With Stack

Recent Stackage nightly snapshots include Haskoin Node. With an older resolver,
add it and its dependencies to `stack.yaml`:

```yaml
extra-deps:
  - haskoin-node-2.4.0
  - haskoin-core-1.6.1
  - secp256k1-haskell-2.0.1
  - base16-2.0
  - rocksdb-haskell-jprupp-2.6.1
  - rocksdb-query-0.6.1
  - nqe-0.6.9
```

### Step by step with Stack

1. Install [Stack](https://docs.haskellstack.org/) and the libraries above.
2. Create a project:

   ```sh
   stack new node-demo simple
   cd node-demo
   ```

3. Add the packages listed under [With Cabal](#with-cabal) to `build-depends`
   in `node-demo.cabal`, plus `network`.
4. If your resolver is older than the packages, add the `extra-deps` shown
   under [With Stack](#with-stack) to `stack.yaml`.
5. Put the [quick start](#quick-start) program in `src/Main.hs` and run it:

   ```sh
   stack run
   ```

   Within a few seconds it logs connections and starts printing header
   heights. A full header sync of Bitcoin usually takes minutes and on the
   order of a hundred megabytes of disk; it resumes where it stopped if you
   interrupt it.

## Quick start

This program connects to Bitcoin, syncs headers, and prints what happens:

```haskell
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad (forever)
import Data.Default (def)
import Database.RocksDB (Config (..), withDB)
import Haskoin (BlockNode (..), NetworkAddress (..), btc, sockToHostAddress)
import Haskoin.Node
import NQE (receive, withPublisher, withSubscription)
import Network.Socket (SockAddr (..))

main :: IO ()
main =
  withLogger LevelInfo $ \logger ->
    withDB "/tmp/haskoin-node" def {createIfMissing = True} $ \db ->
      withPublisher $ \pub ->
        withSubscription pub $ \events -> do
          let cfg =
                NodeConfig
                  { maxPeers = 8,
                    db = db,
                    cf = Nothing,
                    peers = [],
                    discover = True,
                    address = NetworkAddress 0 (sockToHostAddress (SockAddrInet 0 0)),
                    net = btc,
                    pub = pub,
                    timeout = 120,
                    maxPeerLife = 48 * 3600,
                    connect = withConnection,
                    logger = logger
                  }
          withNode cfg $ \_node -> forever $
            receive events >>= \case
              ChainEvent (ChainBestBlock bn) ->
                putStrLn ("Best header: " <> show bn.height)
              ChainEvent (ChainSynced _) ->
                putStrLn "Headers in sync"
              PeerEvent (PeerConnected p) ->
                putStrLn ("Connected: " <> show p)
              _ -> return ()
```

Subscribe to the publisher before calling `withNode`, as here, or you may miss
the first events.

## Tutorial: a block watcher

This builds a program that prints every new Bitcoin block with its number of
transactions. It shows the intended division of labour: the node follows
headers and tells you about them, and you fetch what you need.

### 1. Wait until the headers are synced

At start-up the node downloads hundreds of thousands of headers, each one a
`ChainBestBlock` event. Reacting to each of those would mean fetching every
historical block. Wait for `ChainSynced` first:

```haskell
waitSynced :: Inbox NodeEvent -> IO ()
waitSynced events =
  receive events >>= \case
    ChainEvent (ChainSynced _) -> return ()
    _ -> waitSynced events
```

### 2. React to new best headers

After that, each `ChainBestBlock` is a new block at the tip, or the new tip
after a reorganisation:

```haskell
watch :: Network -> Node -> Inbox NodeEvent -> IO ()
watch net node events = forever $
  receive events >>= \case
    ChainEvent (ChainBestBlock bn) -> fetch net node bn
    _ -> return ()
```

### 3. Fetch the block from the fastest peer

`getPeers` lists online peers, fastest first. Ask one, and try the next if it
fails or times out:

```haskell
fetch :: Network -> Node -> BlockNode -> IO ()
fetch net node bn = getPeers node.peerMgr >>= go
  where
    hash = headerHash bn.header
    go [] = putStrLn ("No peer could send block " <> show bn.height)
    go (op : ops) =
      getBlocks net 30 op.mailbox [hash] >>= \case
        Just [b] ->
          putStrLn (show bn.height <> ": " <> show (length b.txs) <> " transactions")
        _ -> go ops
```

### 4. Put it together

Use the configuration from the [quick start](#quick-start), and run the watcher
inside `withNode`. Besides the imports of the quick start, the watcher needs
`Block (..)`, `Network` and `headerHash` from `Haskoin`, and `Inbox` from `NQE`:

```haskell
withNode cfg $ \node -> do
  waitSynced events
  watch btc node events
```

### What the node did for you

While this runs, the node keeps eight connections, measures and ranks them,
drops silent or misbehaving peers and finds replacements, stores headers so a
restart is quick, and switches to a heavier chain if one appears. The watcher is
only twenty lines because none of that is its concern.

## Concepts

### Components

`withNode` starts two processes, built with [NQE](https://github.com/jprupp/nqe):

- The **peer manager** (`Haskoin.Node.PeerMgr`) finds peer addresses, connects
  to them, does the version handshake, pings them, and drops those that are slow,
  silent, misbehaving or too old. Configured peers are tried first; learned
  addresses are picked at random, at most one per network group. An address
  whose connection failed is tried again later each time, and one whose peer
  misbehaved waits a day.
- The **chain** (`Haskoin.Node.Chain`) asks one peer at a time for headers,
  connects them to the headers it has, follows the chain with the most work, and
  stores the headers in RocksDB. It asks a peer for headers whenever the peer
  announces blocks it does not know, with headers or with inventory, at once if
  another peer holds the turn, and asks every peer when it has heard nothing
  for `timeout` seconds. A turn ends when the peer has nothing more, sends a
  full batch with nothing new, or runs past its deadline.

The rules for talking to peers are pure state machines, each in its own module:

| Module                           | Rules                                                          |
| -------------------------------- | -------------------------------------------------------------- |
| `Haskoin.Node.PeerMgr.Lifecycle` | Life of a connection: handshake, pings, timers, disconnection  |
| `Haskoin.Node.PeerMgr.Handshake` | The version handshake, the first stage of a connection         |
| `Haskoin.Node.PeerMgr.Addresses` | Which address to try next, and when to retry one               |
| `Haskoin.Node.Chain.Sync`        | Which peer to ask for headers, and when headers count as synced |
| `Haskoin.Node.Peer.Request`      | Receiving requested blocks and transactions                    |
| `Haskoin.Node.Peer.Lock`         | Who may hold a peer for a long conversation                    |

The processes store the state and carry out what a transition says. Time is an
input to the transitions, so every timing rule is tested without waiting.

`Haskoin.Node.Block` is a pure check of another kind: whether the
transactions of a block are the ones its header commits to. Every block a peer
sends passes through it.

Each peer connection is its own process (`Haskoin.Node.Peer`), run by a
supervisor inside the peer manager.

### Events

Everything is published to the `Publisher NodeEvent` in the configuration:

| Event                                   | When                                                    |
| --------------------------------------- | ------------------------------------------------------- |
| `ChainEvent (ChainBestBlock node)`      | The best header changed, including on start             |
| `ChainEvent (ChainSynced node)`         | Headers caught up with the network (once)               |
| `PeerEvent (PeerConnected peer)`        | A peer finished the handshake                           |
| `PeerEvent (PeerDisconnected peer)`     | An online peer went away                                |
| `PeerEvent (PeerMessage peer msg)`      | An online peer sent a message: `MInv`, `MBlock`, `MTx` and so on |

Messages are published only from peers that are online, except `version` and
`verack`, which are the handshake itself. A block in an `MBlock` message has
passed `blockMutation`: its transactions hash to the Merkle root of its header,
and its witness data matches its coinbase when the node knows its parent.
Nothing else about it is checked, and a transaction in an `MTx` message is not
checked at all.

"In sync" means the best header is less than two hours old, has at least the
minimum chain work of the network, and no peer is left to sync from.

### Back-pressure

A peer may have at most 16 messages published and not yet handled by the node.
Past that, the node stops reading its socket, so a fast peer fills its own TCP
window instead of the node's memory. The node does not wait for your
subscriber, though: a subscription is an unbounded queue, and one that falls
behind holds every event it has not read. Keep reading events, and hand slow
work to another thread.

### Database layout

Headers are stored in the column family you configure, or the default one:

| Key prefix | Value                         |
| ---------- | ----------------------------- |
| `0x90`     | header (`BlockNode`) by hash  |
| `0x91`     | best header                   |
| `0x92`     | data version                  |

You can keep your own data in the same database, as Haskoin Store does, as long
as your keys do not start with these bytes. `Haskoin.Node.Chain.Database` has
the code.

## Usage guide

### Picking a peer and requesting data

```haskell
ps <- getPeers node.peerMgr -- online peers, fastest first
case ps of
  op : _ -> do
    let p = op.mailbox
    mblocks <- getBlocks btc 30 p [blockHash] -- Maybe [Block]
    mtxs <- getTxs btc 30 p [txHash] -- Maybe [Tx]
    sendMessage MMempool p
  [] -> return ()
```

`getBlocks`, `getTxs` and `getData` return `Nothing` if the peer does not send
everything, in order, before the timeout in seconds. Unrelated messages that the
peer sends in between are ignored. `pingPeer` measures a round trip.

A block that `getBlocks` returns holds the transactions its header commits to.
A peer that sends another block under the requested header is disconnected,
and the request gives `Nothing` when it times out. The transactions are not
validated otherwise. A transaction from `getTxs` is matched by its id, which
does not cover its witness, so treat its witness data as unauthenticated until
you find the transaction in a block.

`getPeers` leaves out peers that are being disconnected, even before their
connection closes.

For a conversation that spans many messages, such as downloading a long run of
blocks, reserve the peer first so that no other task uses it at the same time.
The header chain does the same while it syncs. Only the owner that reserved a
peer can release it:

```haskell
owner <- newOwner "downloader"
reserved <- reservePeer owner p
when reserved $ do
  -- ... long conversation with p ...
  void (releasePeer owner p)
```

`setBusy` and `setFree` still work, but are deprecated: they act for a single
owner shared by all their callers.

### Querying headers

```haskell
best <- chainGetBest node.chain
mnode <- chainGetBlock node.chain someHash
isMain <- chainBlockMain node.chain someHash
fork <- chainGetSplitBlock node.chain nodeA nodeB
parents <- chainGetParents node.chain height top
synced <- chainIsSynced node.chain
```

`chainRunHeaders` runs the header functions of haskoin-core, such as `getAncestor`,
against the database of the chain.

### Static peers

```haskell
cfg {peers = ["node1.example.com", "10.0.0.5:8333", "[2001:db8::1]:8333"], discover = False}
```

A peer without a port uses the default port of the network.

Addresses from DNS seeds must be public, like relayed addresses. On regtest the
seed of haskoin-core is `localhost`, which is not, so list local peers in
`peers` instead of relying on discovery.

### Custom connections

`connect` decides how to open a connection. `withConnection` opens a plain TCP
socket, and gives up if it does not open within five seconds (`connectTimeout`,
as in Bitcoin Core). Replace it to go through a proxy, or to plug in mock peers
in tests, as the test suite does.

### Logging

`withLogger level` logs to standard error, one line per message, such as
`[Info#Chain] Best block header at height 870000`. Build a `Logger` with your
own `push` function to log elsewhere.

## Configuration

| Field         | Meaning                                                                   |
| ------------- | ------------------------------------------------------------------------- |
| `maxPeers`    | Maximum number of peers to connect to                                     |
| `db`          | RocksDB handle for the headers                                            |
| `cf`          | Column family for the headers, or `Nothing` for the default one           |
| `peers`       | Static peers, as `host`, `host:port` or `[ipv6]:port`                     |
| `discover`    | Use DNS seeds and addresses relayed by peers                              |
| `address`     | This node's address, sent in version messages                             |
| `net`         | Network constants from haskoin-core (`btc`, `bch`, `btcTest`, ...)        |
| `pub`         | Publisher that receives every `NodeEvent`                                 |
| `timeout`     | Seconds of silence, without a pong, or without completing the handshake, before a peer is dropped; also how long the syncing peer may take to send headers, and the quiet period after which every peer is asked for headers |
| `maxPeerLife` | Seconds after which a peer is dropped, so that connections rotate         |
| `connect`     | How to open a connection; usually `withConnection`                        |
| `logger`      | Where to log; see `withLogger`                                            |

## Module overview

`Haskoin.Node` re-exports everything a program needs:

| Part           | Contents                                                                    |
| -------------- | --------------------------------------------------------------------------- |
| Node           | `NodeConfig`, `NodeEvent`, `Node`, `withNode`, `withConnection`              |
| Peers          | `Peer`, `PeerEvent`, `PeerException`, `sendMessage`, `getBlocks`, `getTxs`, `getData`, `pingPeer`, `killPeer`, `newOwner`, `reservePeer`, `releasePeer` |
| Blocks         | `blockMutation`, `BlockMutation`, `witnessCheck`, `WitnessCheck`, `segWitHeight` |
| Peer manager   | `PeerMgr`, `OnlinePeer`, `PeerState`, `Stage`, `DisconnectReason`, `getPeers`, `getOnlinePeer`, `toSockAddr`, `isRoutable` |
| Chain          | `Chain`, `ChainEvent`, `chainGetBest`, `chainGetBlock`, `chainGetAncestor`, `chainGetParents`, `chainGetSplitBlock`, `chainBlockMain`, `chainIsSynced`, `chainRunHeaders` |
| Logging        | `Logger`, `LogLevel`, `withLogger`, `withSource`, `logDebug` ... `logError`  |

The lower-level functions that feed messages to the peer manager and the chain
are exported too, for programs that wire the components themselves.

Every module is exposed and can be imported on its own:

| Module                           | Purpose                                                     |
| -------------------------------- | ----------------------------------------------------------- |
| `Haskoin.Node.Peer`              | The process that talks to one peer, and data requests       |
| `Haskoin.Node.Peer.Request`      | State machine for receiving requested blocks and transactions |
| `Haskoin.Node.Peer.Lock`         | State machine for reserving a peer                          |
| `Haskoin.Node.Block`             | Whether a block holds the transactions its header commits to |
| `Haskoin.Node.PeerMgr`           | Finds, connects, ranks and drops peers                      |
| `Haskoin.Node.PeerMgr.Lifecycle` | State machine for the life of a connection                  |
| `Haskoin.Node.PeerMgr.Handshake` | State machine for the version handshake                     |
| `Haskoin.Node.PeerMgr.Addresses` | State machine for the address book                          |
| `Haskoin.Node.Chain`             | Keeps the header chain in sync                              |
| `Haskoin.Node.Chain.Sync`        | State machine that decides which peer to ask for headers, and when the chain is synced |
| `Haskoin.Node.Chain.Database`    | Storage of headers in RocksDB                               |
| `Haskoin.Node.Address`           | Parsing peer addresses, network groups and opening connections |
| `Haskoin.Node.Logger`            | Logging                                                     |

## Security notes

- Every message header is checked before its payload is read. The payload limit
  is the `maxPayloadSize` of the command on the network: 4,000,000 bytes on
  Bitcoin, and on Bitcoin Cash 2 MiB for everything but block messages.
- Reading a payload costs time and memory linear in its size, however small
  the pieces it arrives in.
- List messages are refused as soon as their count is over the limit, such as
  2000 headers, 1000 addresses, 50000 inventory entries or 101 locator hashes.
- A peer that sends anything invalid is disconnected, with a `PeerException`
  saying why.
- A peer that sends a block whose transactions are not the ones its header
  commits to is disconnected (`MutatedBlock`). The check is Bitcoin Core's
  `IsBlockMutated`: the Merkle root, a tree that repeats transactions
  (CVE-2012-2459), and, once SegWit is active at the block's height, the
  witness commitment. The header itself is not marked invalid. When the node
  does not know the parent of a block, its witness data is not checked.
- Messages from a peer that has not completed the handshake are not acted on
  or published, except the handshake messages.
- On Bitcoin, a peer must offer witness data (`NODE_WITNESS`) as well as the
  full chain, as Bitcoin Core requires of its outbound peers.
- A peer that sends a second version or verack, or does not complete the
  handshake within `timeout` seconds even while sending other messages, is
  disconnected.
- A peer that sends invalid headers, or does not answer a request for headers in
  time, is never asked for headers again before it disconnects.
- A syncing peer whose full batch of headers brings nothing new loses its
  turn. One that is still syncing after 15 minutes plus 1 millisecond for each
  block expected since the best header is disconnected when another peer can
  take over, as Bitcoin Core does.
- Addresses relayed by peers and returned by DNS seeds are only used if they
  are public unicast addresses, so a peer or a resolver cannot make the node
  connect to services on its own host or network. Static peers are not checked.
- Learned addresses are picked at random, and at most one connection is open to
  each network group (a /16 of IPv4, a /32 of IPv6), so one operator cannot take
  every slot. Static peers are exempt.
- At most 4096 relayed addresses and 4096 seed addresses are kept, apart, so
  gossip cannot crowd out the seeds. When the room for one is full, a new
  address replaces one that has failed to connect.
- A connection that does not open within five seconds is given up.
- An address whose connection fails waits twice as long after each failure
  before it is tried again, up to ten minutes. A relayed address whose peer
  broke the protocol is not tried again for a day. Static peers are never
  banned, and wait at most ten minutes.
- Headers are not reported as synced until they have the minimum chain work of
  the network. Above that work, forks from far below the tip on a chain with
  less work are refused.
- Below that work, during the initial sync, every valid header is stored. A
  peer can make the node store a fork of low difficulty, at the cost of its
  proof of work. Bitcoin Core first checks that a peer's headers reach the
  minimum work before it stores any; this node does not yet.
- The node trusts the chain with the most work. It checks that a block holds
  the transactions its header commits to, but does not validate the block:
  check the transactions you get from peers before relying on them.

## Development

```sh
git clone https://github.com/jprupp/haskoin-node.git
cd haskoin-node

stack build                        # build the library
stack test                         # run the tests
stack haddock --no-haddock-deps    # build the API documentation
```

On NixOS, or with `stack --nix`, the `nix` section of `stack.yaml` provides
RocksDB and libsecp256k1.

`package.yaml` is the source of the package description. Change it rather than
`haskoin-node.cabal`, which [hpack](https://github.com/sol/hpack) generates.

The package also builds with [Cabal](https://www.haskell.org/cabal)
(`cabal build`, `cabal test`).

There are two test suites, using [hspec](https://hspec.github.io/) and
[QuickCheck](https://hackage.haskell.org/package/QuickCheck):

- `spec` (`test/unit`) tests the state machines with properties, and runs the
  peer, peer manager, chain and node against in-memory mock peers. It needs no
  Internet access.

  ```sh
  stack test haskoin-node:spec
  ```

- `regtest` (`test/regtest`) runs the node against
  [Bitcoin Core](https://bitcoincore.org) and
  [Bitcoin Cash Node](https://bitcoincashnode.org) in regression test mode. The
  nodes run in a [Podman](https://podman.io) container built from
  `test/regtest/Containerfile`, whose script `test/regtest/install-nodes.sh`
  installs the official release binaries on Ubuntu after checking their SHA-256
  hashes. The image is built the first time
  the suite runs. Nodes use random ports, so runs do not collide. The suite is
  skipped if Podman is missing or `HASKOIN_NODE_SKIP_REGTEST` is set.

  ```sh
  stack test haskoin-node:regtest
  ```

Set `HASKOIN_TEST_LOG` to see the node's logs while tests run.

CI builds and tests every change with the OS libraries, with `stack --nix`,
and against the regtest nodes. Its steps call the scripts in `scripts/`, which
you can also run locally: `scripts/check-local-deps.sh` fails if `stack.yaml`
points at a package outside the repository, which CI cannot build, and
`scripts/install-system-libraries.sh` installs the C libraries on Ubuntu.

### Contributing

Pull requests are welcome.

- Format the code with [Ormolu](https://github.com/tweag/ormolu).
- Add tests for the behaviour you change.
- Keep lines of code, including scripts, workflows and the examples in the
  documentation, shorter than 120 columns.
- Put multi-line scripts in their own files rather than embedding them in a
  workflow or a Containerfile.
- Describe the change under a new version in `CHANGELOG.md`. The file follows
  [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and versions follow
  [Semantic Versioning](https://semver.org/).

## Related projects

- [haskoin-core](https://github.com/jprupp/haskoin-core) has the Bitcoin data
  types and protocol that this library speaks.
- [haskoin-store](https://github.com/jprupp/haskoin-store) indexes blocks and
  transactions on top of this node and serves them over a REST API.
- [nqe](https://github.com/jprupp/nqe) provides the processes, supervisors and
  publishers.
- [rocksdb-query](https://github.com/jprupp/rocksdb-query) stores the headers.

## License

Haskoin Node is released under the
[MIT License](https://github.com/jprupp/haskoin-node/blob/master/LICENSE).
