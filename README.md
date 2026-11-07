# Haskoin Node

Haskoin Node is a peer-to-peer networking library for Bitcoin (BTC) and
Bitcoin Cash (BCH), written in Haskell. It connects to the network,
keeps a pool of healthy peers, and synchronises block headers into a
[RocksDB](https://rocksdb.org) database. Your application listens to the
events it publishes and asks its peers for blocks and transactions as needed.

It is the networking layer of
[Haskoin Store](https://github.com/jprupp/haskoin-store), and is built on
[haskoin-core](https://github.com/jprupp/haskoin-core) and
[NQE](https://github.com/jprupp/nqe).

## Features

- Peer discovery through DNS seeds and relayed addresses, or a static list of
  peers.
- Version handshake, ping-based latency tracking, and automatic
  disconnection of slow, idle, misbehaving or long-lived peers.
- Peers sorted by median ping, so the fastest come first.
- Header synchronisation following the chain with the most work, with
  reorganisation support and a "synced" notification.
- Back-pressure: a peer that sends faster than the node can work is
  throttled at the TCP level instead of filling memory.
- Strict message parsing with per-command size limits.
- Helpers to fetch blocks and transactions from a peer with a timeout.

## Requirements

- [Stack](https://docs.haskellstack.org/)
- The C libraries [RocksDB](https://rocksdb.org/) and
  [libsecp256k1](https://github.com/bitcoin-core/secp256k1).

On Debian or Ubuntu:

```sh
sudo apt install pkg-config librocksdb-dev libsecp256k1-dev
```

## Usage

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
import Control.Monad (forever)
import Data.Default (def)
import Database.RocksDB (Config (..), withDB)
import Haskoin (btc, NetworkAddress (..), sockToHostAddress)
import Haskoin.Node
import Network.Socket (SockAddr (..))
import NQE (receive, withPublisher, withSubscription)

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
    withNode cfg $ \node -> forever $
      receive events >>= \case
        ChainEvent (ChainBestBlock bn) ->
          putStrLn ("Best header: " <> show bn.height)
        ChainEvent (ChainSynced _) ->
          putStrLn "Headers in sync"
        PeerEvent (PeerConnected p) ->
          putStrLn ("Connected: " <> show p)
        _ -> return ()
```

### Configuration

| Field         | Meaning                                                              |
|---------------|----------------------------------------------------------------------|
| `maxPeers`    | Maximum number of peers to connect to.                               |
| `db`, `cf`    | RocksDB handle and optional column family for headers.               |
| `peers`       | Static peers, as `host`, `host:port` or `[ipv6]:port`.               |
| `discover`    | Use DNS seeds and addresses relayed by peers.                        |
| `address`     | This node's address, sent in version messages.                       |
| `net`         | Network constants from haskoin-core (`btc`, `bch`, `btcTest`, ...).  |
| `pub`         | Publisher that receives every `NodeEvent`.                           |
| `timeout`     | Seconds before an unresponsive peer is dropped.                      |
| `maxPeerLife` | Seconds after which a peer is dropped, so connections rotate.        |
| `connect`     | How to open a TCP connection; usually `withConnection`.              |
| `logger`      | A `Logger`; see `withLogger`.                                        |

### Events

Every event is published as a `NodeEvent`:

- `ChainEvent (ChainBestBlock node)`: the best header changed.
- `ChainEvent (ChainSynced node)`: headers caught up with the network
  (published once).
- `PeerEvent (PeerConnected peer)` / `PeerEvent (PeerDisconnected peer)`.
- `PeerEvent (PeerMessage peer msg)`: every message received from a peer,
  such as `MInv`, `MBlock` or `MTx`.

### Talking to peers

```haskell
ps <- getPeers node.peerMgr                -- fastest first
case ps of
  op : _ -> do
    mblocks <- getBlocks btc 30 op.mailbox [blockHash]
    mtxs    <- getTxs btc 30 op.mailbox [txHash]
    sendMessage MMempool op.mailbox
  [] -> return ()
```

`getBlocks`, `getTxs` and `getData` return `Nothing` if the peer does not
deliver everything in order before the timeout (in seconds).

### Querying headers

```haskell
best  <- chainGetBest node.chain
mnode <- chainGetBlock node.chain someHash
main  <- chainBlockMain node.chain someHash
split <- chainGetSplitBlock node.chain nodeA nodeB
```

### Database layout

Headers live in the configured column family (or the default one) under keys
starting with byte `0x90`. The best header is at `0x91` and a data version at
`0x92`. Don't use these prefixes for your own data in the same column family.

## Building and testing

```sh
stack build
stack test
stack haddock
```

The tests run against mock peers on the Bitcoin Cash regression test network,
so they do not need Internet access.

## License

MIT. See [LICENSE](LICENSE).
