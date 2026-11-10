# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.5.0] - 2026-11-10

This release fixes the findings of a security review. Several fixes change
the API; each is marked "Breaking" below.

### Added

- `Haskoin.Node.Block`, re-exported by `Haskoin.Node`: `blockMutation` tells
  whether the transactions of a block are the ones its header commits to, as
  `IsBlockMutated` in Bitcoin Core 32.1 does. `witnessCheck` and
  `segWitHeight` choose the witness rule for a block from the height of its
  parent.
- `Haskoin.Node.Address.netGroup` and `NetGroup`: the network group of an
  address, as Bitcoin Core forms it, and `connectTimeout`.
- `Haskoin.Node.Chain.Sync`: `syncDeadline`, `syncTimedOut`, `askHeaders`,
  `gotHeaders`, `mayAsk`, `headersResponseTime`, `headersSyncTimeoutBase` and
  `headersSyncTimeoutPerHeader`, for the deadline of a turn and the requests
  for headers in flight.
- `Haskoin.Node.PeerMgr.Addresses`: the `Seeded` source and `addressSource`.

### Security

- Blocks are checked against their headers. A peer could answer a request for
  a block with the real header, and its proof of work, together with
  transactions of its choice, and `getBlocks`, `getData` and the `MBlock`
  events passed them on. Applications such as Haskoin Store would then record
  payments that never happened. Every block a peer sends now has to pass
  `blockMutation`: the transaction ids hash to the Merkle root, the tree does
  not repeat transactions (CVE-2012-2459), a block without a coinbase holds no
  64-byte transaction, and, once SegWit is active at the block's height, the
  witness data matches the commitment in the coinbase. A peer that sends a
  block that fails is disconnected with `MutatedBlock`. The header is not
  marked invalid. When the node does not know the parent of a block, the
  witness data is not checked, as in Bitcoin Core.
- One peer could hold the header sync forever. It kept its turn as long as it
  sent full batches, even of headers the node already had, and no other peer
  was asked. Now a full batch that brings no new header ends the turn, a turn
  that runs past the deadline Bitcoin Core gives its initial headers sync (15
  minutes plus 1 millisecond per block expected since the best header) ends
  with the peer dropped when another peer can take over, and a peer that
  announces an unknown block is asked for headers at once, with at most one
  request in flight per peer, as in Bitcoin Core.
- Reading a message took time that grew with the square of its size when it
  arrived in small pieces. A peer could keep a core busy for seconds with one
  4 MB message, and for about a minute with a 64 MB Bitcoin Cash block
  message. Reading is now linear.
- One IP address could take every outbound slot. Addresses were tried in the
  order of their port, any number of connections could go to one network, and
  a book full of gossip learned no new seed address. Learned addresses are now
  picked at random, at most one connection goes to each network group (a /16
  of IPv4, a /32 of IPv6) unless the address is configured, seed addresses
  have their own room, and a new address can replace one that failed.
- Messages from a peer that had not completed the handshake were published to
  the application, outside back-pressure. They are now dropped, except the
  `version` and `verack` messages, as Bitcoin Core does.
- Addresses returned by DNS seeds must be routable, like relayed addresses,
  and the ORCHID ranges `2001:10::/28` and `2001:20::/28` are no longer
  routable, as in Bitcoin Core.
- On networks with SegWit, a peer must offer `NODE_WITNESS`, as Bitcoin Core
  requires of its outbound peers. The node asks for blocks and transactions
  with their witness, which such a peer cannot serve.
- `withConnection` gives up on a connection that does not open within five
  seconds. A relayed address that dropped connection requests held a slot for
  about two minutes.

### Changed

- Allow `rocksdb-haskell-jprupp` 2.7, which adds iterators that stop at a
  bound. Nothing in this package changes with it: version 2.7 is a new
  major version only because the `DB` record gained a field, and this
  package gets its `DB` from `withDBCF`. It is built and tested with 2.7.1,
  and versions from 2.6.1 on are still allowed.
- Breaking: `PeerConfig` and `PeerMgrConfig` have a `lookupBlock` field, the
  function that finds a stored header. A peer uses it to tell whether SegWit
  applies to a block it receives. `withNode` fills it in; code that builds
  these records itself must add it. A function that always gives `Nothing`
  keeps the Merkle root check but skips the witness check.
- Breaking: `PeerException` has a `MutatedBlock` constructor, and
  `HandshakeError` a `NotWitnessNode` one.
- Breaking: `checkVersion` takes the network.
- Breaking: `takeAddress` takes a random number and a predicate that says
  which seed and gossiped addresses may be taken. Configured addresses come
  first. `Source` has a `Seeded` constructor, and the peer manager adds seed
  addresses with it.
- Breaking: `startSync` takes the deadline of the turn.
- On regtest, the DNS seed of haskoin-core is `localhost`, which is not
  routable, so discovery no longer finds a local node. Configure local peers
  in `peers`.
- When a batch of headers fails after some valid headers, the node keeps the
  valid ones, as before, and now publishes the `ChainBestBlock` event for the
  best block they set.
- Counting the gossiped addresses in the book no longer scans it for every
  new address.
- The library depends on streaming-commons directly, for the TCP connection
  that `withConnection` opens. It was already a dependency of conduit-extra.
- The documentation says that a subscriber that falls behind holds memory,
  that witness data from `getTxs` and `MTx` events is not authenticated, and
  that headers below the minimum chain work are stored during the initial
  sync.
- The README quick start compiles again: it needed `DisambiguateRecordFields`
  and the fields of `BlockNode`. The tutorial names the imports it adds.
- Tests: the Bitcoin Core blocks used as fixtures, the limits of inventory,
  locator and user agent sizes, back-pressure, the edges of the tip age and of
  the maximum life, reserved address ranges, and ties between forks of equal
  work are tested. Mock peers send their test messages once they are online,
  which ends a race that made some tests time out or hang.
- The CI workflow's shell steps moved into `scripts/check-local-deps.sh` and
  `scripts/install-system-libraries.sh`, and the regtest Containerfile's
  install step into `test/regtest/install-nodes.sh`, so each can be read and
  run on its own. The package description in `package.yaml` keeps its line
  breaks, so the generated cabal file has shorter lines.
- The instructions for contributors and for Claude ask for lines of code
  shorter than 120 columns and for scripts in their own files.
- The README shows a badge with the status of CI on `master`.

## [2.4.0] - 2026-11-09

### Added

- `chainBlocksAnnounced`: the node passes blocks announced in inventory
  messages to the chain, which asks the peer for headers if it does not know
  them.
- Pure state machines, each with property tests: the life of a connection
  (`Haskoin.Node.PeerMgr.Lifecycle`) and its version handshake
  (`Haskoin.Node.PeerMgr.Handshake`), the address book
  (`Haskoin.Node.PeerMgr.Addresses`), reserving a peer
  (`Haskoin.Node.Peer.Lock`), choosing which peer to ask for headers
  (`Haskoin.Node.Chain.Sync`), and receiving requested blocks and transactions
  (`Haskoin.Node.Peer.Request`).
- `isSynced` says whether the header chain counts as synced, as a pure
  function.
- `newOwner`, `reservePeer`, `releasePeer` and `peerHolder` reserve a peer for
  an owner, so that only that owner can release it.
- `PeerException` says why a peer connection ended, instead of an `error`
  message.
- `Haskoin.Node.Address` and `Haskoin.Node.Chain.Database` hold code split
  out of the peer manager, the node and the chain, which still re-export it.
  Every module is exposed, and `PeerMessage` is exported.
- Unit tests of the chain, peer and peer manager processes against in-memory
  mock peers.
- A regtest suite that runs the node against Bitcoin Core 32.1 and Bitcoin Cash
  Node 29.2.0 in Podman, from official binaries with checked hashes, and a CI
  job that runs it.
- `cabal.project`, so the package builds with Cabal too.
- `chainRunHeaders` runs header functions of haskoin-core, such as
  `getAncestor` or `blockLocator`, against the headers of a chain, and
  `runHeaders` does the same for a `ChainDB`.

### Changed

- If no headers are requested or received for `timeout` seconds, every
  connected peer is asked for headers again. This bounds how long any stall of
  the header chain can last.
- The chain checks for timeouts at intervals of at most a quarter of
  `timeout`, between 0.1 and 20 seconds, instead of every 2 to 20 seconds.
- Breaking: `OnlinePeer` holds the state of the connection in a `state` field
  instead of the `verack`, `online`, `version`, `ping`, `pings`, `connected`
  and `tickled` fields. These are still available as computed fields, so
  `o.version` and `o.online` keep working, but code that builds or pattern
  matches `OnlinePeer` records must change.
- Breaking: the busy flag of a `Peer` is a lock held by an owner, so `wrapPeer`
  takes a `TVar (Lock Owner)`. `setBusy` and `setFree` are deprecated: they act
  for a single owner shared by all their callers, and `setFree` no longer
  releases a peer that another owner reserved. The chain reserves peers as its
  own owner.
- A peer told to disconnect is no longer returned by `getPeers`, and its
  messages are no longer routed, even before its connection closes.
- A peer that does not complete the handshake within `timeout` seconds is
  disconnected, even if it keeps sending other messages.
- An address whose connection fails waits before it is tried again, twice as
  long after each failure up to ten minutes, and a relayed address whose peer
  broke the protocol waits a day. Before, failing addresses were retried
  within seconds forever. The configured peers and seeds are resolved again at
  most every five minutes.
- Breaking: `ChainT` and `runChainT` are removed, along with the
  `BlockHeaders` instances for `ReaderT Chain m` and `ReaderT ChainConfig m`.
  Use `chainRunHeaders` instead. The package no longer uses monad
  transformers outside `Haskoin.Node.Chain.Database`, where a `ReaderT` carries
  the database for haskoin-core's header functions; `withNode` nests its
  resources directly instead of through `ContT`.
- Breaking: every function runs in `IO`. The `MonadIO` and `MonadUnliftIO`
  constraints are gone, so code in another monad must call the node with
  `liftIO`. The library depends on async and stm instead of unliftio.
- The package uses the GHC2024 language edition, strict warnings and upper
  bounds on its dependencies. `Setup.hs`, `hie.yaml` and `.hindent.yaml` were
  removed.
- `stack.yaml` builds against secp256k1-haskell 2.0.1. The README's Stack
  example names the same version.

### Fixed

- Peers were ranked by the newest ping and the ten fastest ever measured,
  instead of the last eleven: slow recent pings were dropped. A peer that
  became slow kept ranking as fast.
- The header chain stopped following the network when a peer announced new
  blocks with an inventory message instead of headers, which peers do when
  many blocks are found at once. The chain stayed at the old height until the
  peer reconnected.
- A peer that announced headers that did not connect to the chain was
  disconnected. It is now asked for headers.
- A syncing peer that timed out kept the chain waiting until its disconnection
  arrived. The chain now moves on at once.
- A full batch of headers from a peer that was not being synced against was
  not followed up.
- `getData`, `getBlocks` and `getTxs` failed when the peer sent an unrelated
  message, or a notfound message for items that were not requested, before all
  the requested items arrived.

## [2.3.1] - 2026-11-07

### Changed

- Documentation: the README explains the purpose and design of the node, with
  installation steps, configuration, security notes and a tutorial, and the
  Haddock of every module and function says why it works the way it does.
- Build against nqe 0.6.9, rocksdb-haskell-jprupp 2.6.1, rocksdb-query 0.6.1
  and Stackage nightly-2026-11-07.

## [2.3.0] - 2026-11-06

### Changed

- This package requires haskoin-core 1.6.0, the release with the fixes of
  the November 2026 security review.
- Breaking: the `version`, `timestamp` and `startHeight` fields of the
  `Version` of an `OnlinePeer` are signed numbers, as haskoin-core 1.6.0
  reads them. `buildVersion` takes the same arguments as before.
- The headers are not reported as synced before the best header has the
  `minimumChainWork` of the network. This holds for the `ChainSynced` event
  and for `chainIsSynced`. A recent header on a chain with less work used to
  be enough.
- Once the best header has the `minimumChainWork` of the network, a peer
  whose headers fork from far below the tip, on a chain with less work, is
  disconnected. haskoin-core 1.6.0 refuses such headers.
- A peer that sends a header the node already has at or below a checkpoint
  is no longer disconnected. haskoin-core 1.6.0 returns the stored header.
- Transactions from Bitcoin Cash peers are read in the format without
  witness only, as haskoin-core 1.6.0 reads them on a network without SegWit.
- The log line for an invalid payload gives the reason the parser refused it.
- The payload size limit is the `maxPayloadSize` of the command on the
  network instead of the network's `maxMessageSize`. On Bitcoin Cash that is
  2 MiB for every message but `block`, `cmpctblock` and `blocktxn`, which may
  be twice the block size.

### Fixed

- Peers that send more than 50000 entries in an `inv` or `getdata` message,
  or a block locator of more than 101 hashes, are disconnected. haskoin-core
  1.6.0 refuses such a message as soon as it reads the count.

## [2.2.0] - 2026-11-04

### Changed

- Breaking: `Peer` has a `pending` field, `wrapPeer` takes its `TVar Int`,
  and `peer` takes the `Peer` that `wrapPeer` built instead of the busy
  `TVar`.
- Messages from a peer apply backpressure. A peer may have at most
  `maxPendingMessages` messages waiting for the node; after that the node
  stops reading its socket until it catches up. Address, header, version,
  verack, ping and pong messages are handled to completion before the next
  one is taken. A fast peer could fill the node's unbounded queues before.
- Outgoing messages are logged by command at debug level, not in full.

### Fixed

- Bitcoin Cash headers sync again. This package now requires haskoin-core
  1.5.1, which evaluates the ASERT difficulty rule at the parent block. With
  haskoin-core 1.5.0 the node stopped at mainnet block 975899 with
  `Incorrect difficulty bits`, because that release checked the bits of each
  header with a formula that used the header's own timestamp.
- Network magic and payload size are checked right after the message header,
  before the payload is read. The size limit is the network's
  `maxMessageSize` instead of a fixed 32 MiB. An oversized message used to be
  logged and its payload then parsed as the next header.
- Peers that send more than 2000 headers or more than 1000 addresses in one
  message are disconnected before the message is decoded.
- Address and header messages from a peer are ignored until it completes the
  handshake.
- A second version or verack from a peer disconnects it. Each one used to
  announce the peer as connected again.
- Relayed addresses that are not public unicast addresses (loopback,
  private, link-local, multicast, documentation and similar ranges, or port
  0) are not added to the address pool. `isRoutable` is exported.
- A peer that does not answer a ping within the timeout is disconnected, even
  if it keeps sending other messages.
- Only headers from the syncing peer reset its sync timeout. Another peer's
  headers could keep a silent syncing peer from ever timing out.
- The peer manager rechecks `maxPeers` when it handles a queued connection,
  so the connect loop can no longer go over the limit while the manager is
  busy.
- Purging an old chain database with a column family deleted the same keys
  from the default column family too. The purge now deletes keys as it walks
  them instead of building a list of all of them in memory.

## [2.1.2] - 2026-11-04

### Fixed

- An empty headers message from the syncing peer ends the header sync. The
  node used to take it as a reason to ask the same peer again, and kept
  sending getheaders in a tight loop, thousands per second, whenever its
  best header was the peer's tip after a full batch of 2000.

## [2.1.1] - 2026-11-04

### Changed

- The loop that parses messages from a peer uses `loopForever` too, like
  every other long-running loop.
- Tests use IPv4 only: the mock peer is at `127.0.0.1` instead of `[::1]`,
  and the address property test generates IPv4 addresses. On a host
  without IPv6 the mock peer could not be resolved and the peer and chain
  tests hung.

## [2.1.0] - 2026-11-03

- Update to haskoin-core-1.5.0.

## [2.0.0] - 2026-10-31

### Changed

- Replace monad-logger with fast-logger. Configuration records take a `Logger`,
  and functions need only `MonadIO` or `MonadUnliftIO`.
- Require haskoin-core 1.5.0, whose `buildMerkleRoot` returns
  `Either String MerkleRoot`.

## [1.4.4] - 2026-08-31

### Added

- Log `connectBlocks` error message.

### Changed

- Peer connection logged incorrectly as warn is now logged as info.

### Fixed

- Correct documentation for `importHeaders` function.

## [1.4.3] - 2026-08-13

### Fixed

- Fix regression with peer disconnection.

## [1.4.2] - 2026-08-13

### Fixed

- Fix regression with peer selection.

## [1.4.1] - 2026-08-13

### Removed

- Remove PeerException type which is no longer used.

## [1.4.0] - 2026-08-13

### Changed

- Add monad transformers and monad-logger again because logging is too slow.

## [1.3.1] - 2026-08-12

### Added

- Add monad transformers to support haskoin-store.

## [1.3.0] - 2026-08-12

### Changed

- Simplify modules to make compatible with upstream changes.

## [1.2.0] - 2026-08-12

### Changed

- Upstream changes on multiple libraries.

## [1.1.4] - 2025-05-08

### Changed

- Move to personal repository.

## [1.1.3] - 2024-05-23

### Fixed

- Tickle a peer whenever a message is received.

## [1.1.2] - 2024-05-23

### Fixed

- Correct log messages from peer manager.

## [1.1.1] - 2024-05-23

### Fixed

- Peers timeout more aggressively.

## [1.1.0] - 2024-03-14

### Changed

- Update LTS Haskell & upstream dependencies.

## [1.0.1] - 2023-08-03

### Changed

- Add compatibility with latest LTS Haskell.

## [1.0.0] - 2023-07-28

### Changed

- Make compatible with latest haskoin-core.
- Use DuplicateRecordFields and OverloadedRecordDot.
- Simplify pub/sub queues.
- Multiple refactoring passes.

## [0.18.1] - 2022-07-27

### Fixed

- Set default port for peers where it is unset.

## [0.18.0] - 2022-07-27

### Added

- Support setting up and connecting to IPv6 peers.

## [0.17.14] - 2021-08-14

### Fixed

- Reduce verbosity on incoming header decode test.
- Show appropriate error message upon receiving empty headers.

## [0.17.13] - 2021-08-14

### Added

- Display more details about invalid incoming headers.

## [0.17.12] - 2021-05-17

### Fixed

- Do not connect to more than the maximum number of peers.

## [0.17.11] - 2021-05-17

### Added

- Display message command that disconnects a peer.

## [0.17.10] - 2021-05-17

### Fixed

- Correct disconnect timeout algorithm bug.

## [0.17.9] - 2021-05-17

### Fixed

- Add randomised timeouts to avoid disconnecting all peers.

## [0.17.2] - 2021-03-09

### Fixed

- Do not start chain actor until database initialized.

## [0.17.1] - 2021-01-08

### Changed

- Depend on haskoin-core-0.17.3.

## [0.17.0] - 2020-10-21

### Added

- Support for Bitcoin Cash November 2020 hard fork.
- BlockHeaders instance for ReaderT Chain.

## [0.16.0] - 2020-07-23

### Changed

- Add support for column families.

## [0.15.0] - 2020-07-20

### Changed

- Use new Haskell bindings for RocksDB.

## [0.14.1] - 2020-06-19

### Fixed

- Correct flawed peer locking logic in Chain actor.

## [0.14.0] - 2020-06-18

### Changed

- Massively refactor everything in a non-backwards-compatible way.
- Use MIT license.
- Bump haskoin-core.
- Bump secp256k1-haskell.

### Fixed

- Fix getting stuck on a single peer.

## [0.13.0] - 2020-05-08

### Changed

- Depend on Haskoin Store 0.13.3.
- Better code organisation.

## [0.12.0] - 2020-05-06

### Changed

- Add a test suite that simulates network instead of connecting to real one.

## [0.11.3] - 2020-05-03

### Changed

- Revert including multiline decoding error text in logs.

## [0.11.2] - 2020-05-03

### Changed

- Include header decoding error text in logs.

## [0.11.1] - 2020-05-03

### Changed

- Improve logging.

## [0.11.0] - 2020-05-03

### Changed

- Set peer too old time

## [0.10.1] - 2020-05-03

### Changed

- Disconnect old peers after 48 hours instead of 30 minutes.

## [0.10.0] - 2020-05-03

### Changed

- Move modules out of Network.Haskoin namespace.
- Add better and more logging.
- Change Manager module name and related values to PeerManager.

## [0.9.21] - 2020-04-07

### Removed

- Remove unnecessary logging.

## [0.9.20] - 2020-04-07

### Changed

- Better log messages.
- Less verbose debug logging.

## [0.9.19] - 2020-04-07

### Changed
- Better log messages.

## [0.9.18] - 2020-04-07

### Added

- More aggressive peer discovery.

## [0.9.17] - 2020-04-07

### Added

- Peers are disconnected automatically after awhile.

## [0.9.16] - 2020-02-08

### Added

- Lower bound versions for some dependencies.

## [0.9.15] - 2020-01-15

### Changed

- Update to support new `NetworkAddress` data structure from `haskoin-core`.

## [0.9.14] - 2019-12-10

### Removed

- No longer support storing peers in db as performance tradeoff doesn't justify it.

## [0.9.13] - 2019-10-08

### Changed

- Really store peers in db.

## [0.9.12] - 2019-10-08

### Changed

- Demote some logging to debug level.

## [0.9.11] - 2019-10-02

### Added

- Add `-O2` optimisations to GHC.

## [0.9.10] - 2019-04-19

### Added

- Increase debugging information where application freezes.

## [0.9.9] - 2019-04-12

### Added

- Increase debugging information.

## [0.9.8] - 2019-04-12

### Changed

- Increase version of haskoin-core to 0.9.0.
- Fix some tests.

## [0.9.7] - 2019-04-12

### Added

- More debugging.

### Changed

- Be defensive against duplicate peers.
- Increase interval between housekeeping pings.
- Replace peers in database atomically.

## [0.9.6] - 2019-04-01

### Changed

- Randomize known peers instead of keeping scores.
- Simplify peer management code to avoid freezes.
- Merge logic for chain and manager.

## [0.9.5] - 2018-11-14

### Changed

- Do not record new peers in database when peer discovery is disabled.

## [0.9.4] - 2018-11-01

### Changed

- Don't spam best block events.

## [0.9.3] - 2018-10-22

### Changed

- Correct display of milliseconds in log.
- Correct bug when receiving headers from unknown peer.
- Simplify chain syncing code.

## [0.9.2] - 2018-10-18

### Changed

- Peer dies immediately when receiving a bad message.

## [0.9.1] - 2018-10-18

### Changed

- Keep track of last synced header from a peer to avoid endless loops on large reorgs.

## [0.9.0] - 2018-10-17

### Changed

- Use an STM listener instead of a publisher for the node API.

## [0.8.2] - 2018-10-17

### Added

- Expose `ChainMessage` and `ManagerMessage` types from `Haskoin.Node` module.

## [0.8.1] - 2018-10-11

### Changed

- Corrected documentation for `killPeer` function.
- Leave time out of logic code.

## [0.8.0] - 2018-10-09

### Changed

- Peers are now killed directly instead of through peer manager.

### Removed

- Chain no longer needs peer manager.

## [0.7.2] - 2018-10-09

### Added

- Compatibility with base 4.12.

### Changed

- Update base to 4.9.

## [0.7.1] - 2018-10-09

### Added

- Allow to easily obtain a peer's publisher.

## [0.7.0] - 2018-10-09

### Added

- Versioning for chain and peer database.
- Automatic purging of chain and peer database when version changes.
- Add extra timers.
- Add publishers to every peer.

### Changed

- Full reimplementation of node API.
- Simplify peer selection and management.
- Merge manager and peer events.
- Rename configuration variables for node.
- Separate logic from actors for peer manager and chain.

### Removed

- Remove irrelevant fields from peer information.
- Remove unreliable peer block head tracking.
- Remove dependency on deprecated binary conduits.
- Remove Bloom filter support from manager.
- Remove unreliable peer request tracking code.
- Remove separate manager events.

## [0.6.1] - 2018-09-14

### Changed

- Fix bug where peer height did not update in certain cases.

## [0.6.0] - 2018-09-14

### Added

- Documentation everywhere.

### Changed

- Make compatible with NQE 0.5.
- Use supervisor only in peer manager.
- API quality of life changes.
- Exposed module is now only `Haskoin.Node`.

### Removed

- No more direct access to internals.

## [0.5.2] - 2018-09-10

### Changed

- Improve dependency definitions.

## [0.5.1] - 2018-09-10

### Changed

- Dependency `sec256k1` changes to `secp256k1-haskell`.

## [0.5.0] - 2018-09-09

### Added

- New `CHANGELOG.md` file.
- Use `nqe` for concurrency.
- Peer discovery.
- RocksDB peer and block header storage.
- Support for Merkle blocks.

### Changed

- Split out of former `haskoin` repository.
- Use hpack and `package.yaml`.
- Old `haskoin-node` package now renamed to `old-haskoin-node` and deprecated.

### Removed

- Removed Old Haskoin Node package completely.
- Removed Stylish Haskell configuration file.
- Remvoed `haskoin-core` and `haskoin-wallet` packages from this repository.
