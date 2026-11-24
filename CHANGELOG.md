# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

Configured peers are kept. This breaks code that builds an `OnlinePeer`
or a `Limits` by hand: `OnlinePeer` has a new field, and `maxLife` of
`Limits` is a `Maybe`.

### Changed
- **The peers of the configuration are sticky.** They are the peers that
  the operator trusts, and the only reason for one to have no connection
  is now that it takes none.
  - **Never rotated.** `maxPeerLife` no longer applies to a configured
    peer. Rotation defends against peers that nobody chose. `maxLife` of
    `Limits` is `Nothing` for such a connection.
  - **Tried again within ten seconds, for ever.** A configured address
    that failed waited twice as long each time, up to ten minutes, and
    ten minutes after its peer broke the protocol. It now waits ten
    seconds at most, whatever ended the last connection
    (`configuredBackoff`). A node of the operator that restarts is a peer
    again within seconds.
  - **Its slot is kept.** While a configured peer had no connection, seed
    and gossiped addresses filled every slot, and the peer found none
    when it came back, until one of them left. A slot is now kept for
    each configured peer that has no connection, and no other address
    takes it (`roomForOthers`). One name is one peer, whatever it
    resolves to.
  - Tested: the two rules of the address book, as examples and with a
    property; that a connection without a longest life is not dropped
    for its age; and, against Bitcoin Core and Bitcoin Cash Node, that a
    configured peer stays past `maxPeerLife`, where the same test said
    before that it was dropped. Not tested end to end: a store with
    discovery whose configured peer is away, which needs a network of
    strangers.

### Added
- `configured` in `OnlinePeer`: whether the peer is one of the
  configuration.

## [2.7.0] - 2026-11-22

Peers on Unix sockets, a model of the node at the other end of each
connection, and haskoin-core 3.0, for haskoin-store 2.0.0.

### Added
- **A model of each peer's node.** Every connection runs the state machine
  of haskoin-core's `Haskoin.Network.Protocol` for its network: Bitcoin
  Core 32.1 or Bitcoin Cash Node 29.2.0, translated from their source, with
  this node as the peer (`Haskoin.Node.Peer.Remote`). `sendMessage`
  judges every message as it queues it, so that the model sees the messages
  in the order in which they go out. One that the node would punish
  (ban score, discouragement, a ban, or a disconnection for a rule of the
  protocol) is not sent, and the log says why, with the upstream line. A
  request in `getData`, `getBlocks` or `getTxs` that the node would drop
  without an answer gives `Nothing` at once rather than wait for the
  timeout. `judgeMessage` asks without sending. The model takes the options
  of the node from its version message (Bloom filters, pruning, blocks-only)
  and plays the messages sent before it again with them. It takes the node
  to have every block it is asked for, and leaves to happen what depends on
  the guessed state of the node, such as its upload limit.
- `permissions` in `NodeConfig` and `PeerMgrConfig`: the permissions that
  the nodes of `peers` grant this node with `-whitelist` or `-whitebind`,
  such as `PermMempool`. Nothing in the protocol tells a peer, so the
  operator says it. Discovered peers grant none. `Permission` and
  `Permissions` are re-exported from `Haskoin.Node`.
- `expectsReject`: whether the node of a network answers a transaction that
  it refuses with a `reject` message (BIP 61), as Bitcoin Cash Node does and
  Bitcoin Core never does, asked of the model.
- `newPeer` builds a `Peer` handle with a free lock, no pending messages and
  the model of its configuration.
- `seenAs` in `Haskoin.Node.Address`: the address network of a peer and
  whether the node sees this node at a local address, for its model.
- Tests: the model's rules for the mempool request, the handshake order,
  filtered blocks and the replay after the version. In regtest, against
  both nodes: the messages that the node sends up to its verack are the
  ones that the model predicts, a mempool request goes only where the node
  takes it, and a node that whitelists the store with `mempool` answers it.
- **Peers on Unix sockets.** A configured peer can be a Unix socket, written
  `unix:PATH` as Bitcoin Core writes the socket of a proxy, and
  `withConnection` opens it. Neither Bitcoin Core nor Bitcoin Cash Node
  listens on one, so something joins the socket to a node, such as socat or
  the bridge of a test in a container: tests that run at once then reach
  their nodes without taking a port of the machine. Such a peer is in the
  local network group, and the version message gives it the zero address,
  with the new `peerHostAddress`: haskoin-core's `sockToHostAddress` stops
  the program on an address that is not IPv4 or IPv6. A peer or a DNS seed
  can never give a Unix address.

### Fixed
- **A peer is announced before its messages.** A peer's messages come from
  its reader and its announcement from the peer manager, through one queue
  with no order between them. Messages sent right behind the handshake could
  reach the chain and the application before `PeerConnected`, and the chain
  dropped the headers of a peer it did not know yet. The router now runs a
  pure machine (`Haskoin.Node.Router`) that holds such messages until the
  announcement comes through the queue: for each peer, `PeerConnected` comes
  before any message past the handshake, and `PeerDisconnected` after all
  of them.
- **A disconnection always ends.** `killPeer` queued `KillPeer` behind the
  messages to send, and a writer blocked on a peer that stopped reading
  never took it: the connection kept its slot and network group forever,
  and `PeerDisconnected` never came. The process now races its connection
  against a `closing` flag that `killPeer` sets, and the peer manager
  cancels the thread of a process that is still there a timeout after it
  was told to disconnect (the new `Abort` action of
  `Haskoin.Node.PeerMgr.Lifecycle`). `killPeer` no longer sends the
  messages queued before it.
- **Requests end when their peer goes.** `getData`, `getBlocks`, `getTxs`
  and `pingPeer` give `Nothing` or `False` at once when `PeerDisconnected`
  comes for their peer, or when the peer is closing as they start, rather
  than wait for the timeout.
- **Requests on one peer no longer disturb each other.** Two at once took
  each other's blocks and failed as out of order, and so did a request after
  one that timed out, from its late answers. Requests now run one at a time
  per peer, the timeout counting the wait, and a request first ignores what
  the peer sends until the pong of the request before it, if that is still
  owed.
- **The model of a peer never sees time go back.** Two threads sending at
  once read the clock before their transactions and could commit in the
  other order. The model keeps the latest time it has seen, and judges with
  the later of that and the time of the facts (`remoteClock`).

### Changed
- **Every concurrent protocol is a pure machine.** The new
  [docs/concurrency.md](docs/concurrency.md) says, for every thread, what it
  owns, the machine that decides what it does, and why no two threads can
  race. The README links it.
- `Stage`: `Disconnecting` holds when the disconnection began, as
  `Disconnecting reason time`. `PeerInput`: `GotHandshake` and `Refused`
  take the time first. `PeerAction` has `Abort`.
- `Haskoin.Node.Peer.Request`: `stepRequest` takes a `RequestInput`
  (`Received` a message, or `Disconnected`), and `newRequest` takes first
  the nonce of the ping that the request before it is still owed, if any.
  A notfound or block for an item not asked for is ignored, as before; the
  rule is now written down.
- `Peer` has three new fields, `closing`, `requestLock` and `owed`.
  `wrapPeer` is gone: `newPeer` is the one way to build a `Peer`.
- `peerMessageDone` documents its invariant: once for each message, by one
  consumer. The router disposes of every message exactly once.
- Tests: QuickCheck properties of the router over arbitrary interleavings of
  three peers, of the abort of a disconnection, of requests after an owed
  pong and of the model's clock; a peer process that stops while its writer
  is blocked; requests that end when their peer goes; overlapping requests
  and the late answers of a request that timed out; and a node that
  publishes a peer before the burst it sends right behind its handshake.
- `PeerConfig` has two new fields, `bestBlock` and `remote` (the model when
  the connection opens). `Peer` has two, `remote` and `facts`. Build a
  `Peer` with `newPeer`.
  `PeerMessage` has a new constructor, `NotSent`, for a message that the
  model refused, which the peer process logs.
- **Tests of an honest peer run against real nodes.** What the mock peers
  of the unit tests did as an honest node would, the regtest tests now
  check against Bitcoin Core and Bitcoin Cash Node in Podman: a block as the
  node keeps it, with its witness; a block of more than 2 MiB on Bitcoin
  Cash; a peer that hangs (a node stopped with `SIGSTOP`), whose blocks and
  pings give nothing and which is dropped; a peer kept past its longest
  life; the pings of the node; a pruned node, refused; the wait before
  trying a failed peer again; the minimum chain work; the parents of a
  block; and no more requests for headers once the node has them all. The
  mock versions are gone. Peers that break the rules stay scripted in
  memory, since no real node does, and every peer of a test is named by a
  Unix socket, not by an address and a port. Each node has two groups of
  regtest tests, its chain and its one peer, which run at the same time.
- Needs haskoin-core 3.0, for its protocol machines. 3.0 also reads the
  token prefix of Bitcoin Cash outputs (CashTokens) and adds a field to
  `Network`, which change nothing here: haskoin-node passes outputs on as
  they are, and builds no `Network` of its own. Until 3.0 is on Hackage,
  `stack.yaml` pins a commit of haskoin-core's master.
- CI runs through the workflow that the Haskell packages of jprupp share in
  [jprupp/tools](https://github.com/jprupp/tools), with the check of
  `stack.yaml` for local paths (`check-stack-paths`) and the cache of the
  Nix build kept per release of nixpkgs.
- `scripts/check-local-deps.sh` is gone: `check-stack-paths`, from
  jprupp/tools, replaces it.
- `haskell-dev`, from jprupp/tools, is the fast loop of work on the code:
  `build`, `test` and `docs`, without optimisation and with the unit tests
  in several processes. What it needs to know about this package, the slow
  spec modules, is in the new `haskell-dev.toml`.
- `.gitignore` ignores `TAGS` and `dist-newstyle/` again, which ran
  together on one line, and the documentation's work folder.
- The regtest tests run their containers through
  [podman-test](https://github.com/jprupp/podman-test), in place of the code
  of their own in `Regtest.Podman`: each container is in a pod with no
  network, the peer-to-peer port of each of its nodes is bridged to a Unix
  socket, and the node under test reaches the nodes as the peers
  `unix:PATH`. Inside the container the ports are fixed, where they were
  random. `PODMAN_TEST_SKIP` skips the tests, as `HASKOIN_NODE_SKIP_REGTEST`
  still does, and the Containerfile makes its script executable as it
  copies it, since podman-test writes the files of the context without
  their modes.
- The regtest tests are in the `spec` suite (`Haskoin.Node.RegtestSpec`),
  so that `stack test` and `haskell-dev test` run every test; the `regtest`
  suite is gone. They are still skipped without Podman or with
  `HASKOIN_NODE_SKIP_REGTEST` set, and CI still runs them in a job of their
  own. The container of the nodes may use 2 GiB of memory.

## [2.6.1] - 2026-11-18

### Fixed
- **Headers no longer hold up the messages of other peers.** The loop that
  routes peer messages waited for the chain to import each `headers` message.
  On Bitcoin Cash that took 20 seconds per message, and in that time no
  message of any peer reached the application: blocks and pongs piled up, and
  Haskoin Store took peers that were delivering for stalled ones. The loop now
  hands headers and inventory to the chain and goes on. The rest of the
  handling of that message, publishing it to the application and counting it
  as handled for the peer's back-pressure, runs when the chain is done with
  it. Ping, pong and address messages go to the peer manager the same way.
  The application now gets a peer's header message after the chain has its
  headers, and other messages of the same peer may reach it before.
- Requires haskoin-core 2.0.1, which connects Bitcoin Cash headers about 60
  times as fast.

### Added
- `chainThen` and `peerMgrThen` run an action once the chain or the peer
  manager has handled every message sent to it before, without waiting.
- `peerMessageDoneSTM`: `peerMessageDone` in `STM`.


## [2.6.0] - 2026-11-15

### Changed
- **Builds against haskoin-core 2.0.0**, which keeps the context of
  libsecp256k1 to itself: nothing takes a `Ctx` any more. This package
  never took one, and its own API is the same. A program that uses both
  packages must move to haskoin-core 2.0.0 with it.
- **Builds against nqe 0.7.0**, which runs in plain `IO` and no longer uses
  unliftio. This package already ran in `IO`, and its own API is the same. A
  program that uses both packages must move to nqe 0.7.0 with it, and has to
  lift the calls to NQE that it makes from another monad, as in
  `liftIO (receive events)`.
- **Peers can be stopped wherever they are.** A supervisor of nqe before
  0.7.0 started its children with asynchronous exceptions masked, so a peer
  could be cancelled, or timed out, only where it blocked. With nqe 0.7.0
  each peer is an ordinary thread.
- **Builds against rocksdb-haskell-jprupp 2.8** as well as 2.6 and 2.7, and
  against rocksdb-query 0.7 as well as 0.6. With 0.7 a list read from the
  database takes one foreign call for a few hundred entries where it took
  one for each, and a stack that does not grow with the list.
- The tests no longer use unliftio. They take threads, STM, exceptions and
  timeouts from base, async and stm, and temporary directories from
  temporary. With that the unliftio package is gone from everything this
  package builds with. Only unliftio-core remains, which conduit needs.

## [2.5.1] - 2026-11-11

### Changed

- Log levels say who has the problem. An error is now something wrong with
  this node, and nothing a peer does is one:
  - a peer that closes its connection is logged for debugging, and one
    that sends something invalid as a warning; both were errors;
  - headers that do not connect are logged once, as the warning that
    disconnects the peer, and no longer as an error before it;
  - connecting to a peer, its connection and its end are logged for
    debugging, where they were information;
  - the number of connected peers is logged for debugging at every change,
    as a warning when the node loses its last peer, and as information
    when it has peers again.

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
