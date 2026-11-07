# Haskoin Node

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
  anything invalid ends the connection, relayed addresses are filtered, and a
  peer cannot push data faster than the node can handle it.
- **Prefer good peers, but keep moving.** Peers are ranked by measured latency
  so the fastest are used first, and every connection is eventually replaced so
  the node never depends on the same few peers forever.
- **Small, isolated components.** The peer manager, the header chain and each
  connection are separate [NQE](https://github.com/jprupp/nqe) processes with
  their own state. One misbehaving peer dies alone.

## Features

| Area            | What you get                                                                                   |
| --------------- | ---------------------------------------------------------------------------------------------- |
| Networks        | Every network of haskoin-core: Bitcoin and Bitcoin Cash mainnet, testnets and regtest           |
| Peer discovery  | DNS seeds and addresses relayed by peers, or a static list of peers                             |
| Peer management | Version handshake, ping-based latency, peers sorted by median ping, rotation after a maximum age |
| Header sync     | Headers downloaded from one peer at a time, connected to the chain with the most work, stored in RocksDB |
| Events          | Best header changes, sync completion, peer connections and every message received              |
| Data requests   | Fetch blocks and transactions from a peer, with a timeout                                       |
| Protection      | Strict parsing, per-command size limits, back-pressure, bounded address pool                    |

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
  - haskoin-node-2.3.1
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad (forever)
import Data.Default (def)
import Database.RocksDB (Config (..), withDB)
import Haskoin (NetworkAddress (..), btc, sockToHostAddress)
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
inside `withNode`:

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
  silent, misbehaving or too old.
- The **chain** (`Haskoin.Node.Chain`) asks one peer at a time for headers,
  connects them to the headers it has, follows the chain with the most work, and
  stores the headers in RocksDB.

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
| `PeerEvent (PeerMessage peer msg)`      | A peer sent a message: `MInv`, `MBlock`, `MTx` and so on |

"In sync" means the best header is less than two hours old, has at least the
minimum chain work of the network, and no peer is left to sync from.

### Back-pressure

A peer may have at most 16 messages published and not yet handled by the node.
Past that, the node stops reading its socket, so a fast peer fills its own TCP
window instead of the node's memory. Your subscriber should keep reading events;
a subscription that falls far behind holds memory.

### Database layout

Headers are stored in the column family you configure, or the default one:

| Key prefix | Value                         |
| ---------- | ----------------------------- |
| `0x90`     | header (`BlockNode`) by hash  |
| `0x91`     | best header                   |
| `0x92`     | data version                  |

You can keep your own data in the same database, as Haskoin Store does, as long
as your keys do not start with these bytes.

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
everything, in order, before the timeout in seconds. `pingPeer` measures a round
trip.

### Querying headers

```haskell
best <- chainGetBest node.chain
mnode <- chainGetBlock node.chain someHash
isMain <- chainBlockMain node.chain someHash
fork <- chainGetSplitBlock node.chain nodeA nodeB
parents <- chainGetParents node.chain height top
synced <- chainIsSynced node.chain
```

`runChainT` runs the header functions of haskoin-core, such as `getAncestor`,
against the database of the chain.

### Static peers

```haskell
cfg {peers = ["node1.example.com", "10.0.0.5:8333", "[2001:db8::1]:8333"], discover = False}
```

A peer without a port uses the default port of the network.

### Custom connections

`connect` decides how to open a connection. `withConnection` opens a plain TCP
socket. Replace it to go through a proxy, or to plug in mock peers in tests, as
the test suite does.

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
| `timeout`     | Seconds of silence, or without a pong, before a peer is dropped           |
| `maxPeerLife` | Seconds after which a peer is dropped, so that connections rotate         |
| `connect`     | How to open a connection; usually `withConnection`                        |
| `logger`      | Where to log; see `withLogger`                                            |

## Module overview

`Haskoin.Node` is the only exposed module. It re-exports:

| Part           | Contents                                                                    |
| -------------- | --------------------------------------------------------------------------- |
| Node           | `NodeConfig`, `NodeEvent`, `Node`, `withNode`, `withConnection`              |
| Peers          | `Peer`, `PeerEvent`, `sendMessage`, `getBlocks`, `getTxs`, `getData`, `pingPeer`, `killPeer` |
| Peer manager   | `PeerMgr`, `OnlinePeer`, `getPeers`, `getOnlinePeer`, `toSockAddr`, `isRoutable` |
| Chain          | `Chain`, `ChainEvent`, `chainGetBest`, `chainGetBlock`, `chainGetAncestor`, `chainGetParents`, `chainGetSplitBlock`, `chainBlockMain`, `chainIsSynced`, `runChainT` |
| Logging        | `Logger`, `LogLevel`, `withLogger`, `withSource`, `logDebug` ... `logError`  |

The lower-level functions that feed messages to the peer manager and the chain
are exported too, for programs that wire the components themselves.

## Security notes

- Every message header is checked before its payload is read. The payload limit
  is the `maxPayloadSize` of the command on the network: on Bitcoin Cash, 2 MiB
  for everything but block messages.
- List messages are refused as soon as their count is over the limit, such as
  2000 headers, 1000 addresses or 50000 inventory entries.
- A peer that sends anything invalid is disconnected.
- Addresses relayed by peers are only used if they are public unicast
  addresses, so a peer cannot make the node connect to services on its own host
  or network. Static peers are not checked.
- At most 4096 relayed addresses are kept.
- Headers are not reported as synced until they have the minimum chain work of
  the network. Above that work, forks from far below the tip on a chain with
  less work are refused.
- The node trusts the chain with the most work. It does not validate blocks:
  check the data you get from peers before relying on it.

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

The tests are in `test/` and use [hspec](https://hspec.github.io/). They run
against mock peers on the Bitcoin Cash regression test network, so they need no
Internet access.

### Contributing

Pull requests are welcome.

- Format the code with [Ormolu](https://github.com/tweag/ormolu).
- Add tests for the behaviour you change.
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
