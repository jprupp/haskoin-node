-- | Helpers shared by the unit and regtest test suites.
module Haskoin.Node.Test
  ( -- * Logging
    withTestLogger,

    -- * Timing
    within,
    eventually,
    receiveWithin,
    expectNothingWithin,

    -- * Block chains
    mkBlocks,
    mkHeaders,
    coinbaseTx,
    serveHeaders,
    headersMessage,
    nowTimestamp,

    -- * Blocks from Bitcoin Core
    corePreSegWitBlock,
    coreSegWitBlock,

    -- * Database
    withTestDB,
    withDBAt,

    -- * In-memory connections
    Remote (..),
    memoryConnection,
    remoteSend,
    remoteSendRaw,
    remoteReceive,
    remoteReceiveMatch,
    remoteClose,
    remoteVersion,
    fullNodeServices,
    respondHonestly,
    honestRemote,
    encodeMessage,

    -- * Nodes
    TestNode (..),
    withTestNode,
    setTimeout,
    setMaxPeerLife,
    waitForPeer,
    waitForDisconnect,
    waitForBest,
    zeroAddress,
  )
where

import Conduit (awaitForever, yield)
import Control.Monad (forM_, unless)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Default (def)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Serialize (decode, encode, runGet, runPut)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32, Word64)
import Database.RocksDB (DB)
import Database.RocksDB qualified as R
import Haskoin
import Haskoin.Node
import Haskoin.Node qualified as NC (NodeConfig (..))
import NQE
import Network.Socket (SockAddr (..))
import System.Environment (lookupEnv)
import System.Random (randomIO)
import Test.HUnit (assertFailure)
import UnliftIO
import UnliftIO.Concurrent (threadDelay)

-- | Run with logging to stderr if HASKOIN_TEST_LOG is set, silently otherwise.
withTestLogger :: (Logger -> IO a) -> IO a
withTestLogger f =
  lookupEnv "HASKOIN_TEST_LOG" >>= \case
    Just _ -> withLogger LevelDebug f
    Nothing -> f Logger {level = LevelError, source = "", push = \_ -> return ()}

-- | Fail the test if the action does not complete in the given seconds.
within :: Double -> String -> IO a -> IO a
within s what act =
  timeout (round (s * 1000000)) act >>= \case
    Nothing -> assertFailure $ "Timed out after " <> show s <> "s: " <> what
    Just x -> return x

-- | Fail the test unless the condition becomes true in the given seconds.
eventually :: Double -> String -> IO Bool -> IO ()
eventually s what cond = within s what go
  where
    go = cond >>= \b -> unless b (threadDelay 20000 >> go)

-- | Receive the first matching message in the given seconds.
receiveWithin ::
  (InChan mbox) =>
  Double ->
  String ->
  mbox msg ->
  (msg -> Maybe a) ->
  IO a
receiveWithin s what mbox f = within s what (receiveMatch mbox f)

-- | Fail the test if a matching message arrives in the given seconds.
expectNothingWithin ::
  (InChan mbox) =>
  Double ->
  String ->
  mbox msg ->
  (msg -> Maybe a) ->
  IO ()
expectNothingWithin s what mbox f =
  timeout (round (s * 1000000)) (receiveMatch mbox f) >>= \case
    Nothing -> return ()
    Just _ -> assertFailure $ "Unexpected: " <> what

-- | Current time as a block timestamp.
nowTimestamp :: IO Word32
nowTimestamp = floor <$> getPOSIXTime

-- | Coinbase-like transaction that makes each block unique.
coinbaseTx :: Word32 -> Int -> Tx
coinbaseTx tag i =
  Tx
    { version = 1,
      inputs =
        [ TxIn
            { outpoint = OutPoint {hash = z, index = maxBound},
              script = encode (tag, fromIntegral i :: Word64),
              sequence = maxBound
            }
        ],
      outputs = [TxOut {value = 5000000000, script = B.singleton 0x51}],
      witness = [],
      locktime = 0
    }
  where
    z = "0000000000000000000000000000000000000000000000000000000000000000"

-- | Mine blocks on top of a parent header. Timestamps start at the given
-- time and increase by one second. Use different tags to build forks.
mkBlocks :: Network -> Word32 -> BlockHeader -> Word32 -> Int -> [Block]
mkBlocks net tag parent t0 n = go parent 0
  where
    go p i
      | i >= n = []
      | otherwise =
          let tx = coinbaseTx tag i
              h =
                mineBlock net 0 $
                  BlockHeader
                    { version = 0x20000000,
                      prev = headerHash p,
                      merkle = either error id (buildMerkleRoot [txHash tx]),
                      timestamp = max (t0 + fromIntegral i) (p.timestamp + 1),
                      bits = net.genesisHeader.bits,
                      nonce = 0
                    }
           in Block {header = h, txs = [tx]} : go h (i + 1)

-- | Headers for 'mkBlocks'.
mkHeaders :: Network -> Word32 -> BlockHeader -> Word32 -> Int -> [BlockHeader]
mkHeaders net tag parent t0 n = map (.header) (mkBlocks net tag parent t0 n)

-- | Answer a header request like a full node would. The chain excludes the
-- genesis block.
serveHeaders :: Network -> [BlockHeader] -> GetHeaders -> [BlockHeader]
serveHeaders net chain gh =
  take 2000 (stop_at (drop start chain))
  where
    hashes = headerHash net.genesisHeader : map headerHash chain
    start =
      fromMaybe 0 $
        listToMaybe [i | l <- gh.locator, Just i <- [elemIndex l hashes]]
    z = "0000000000000000000000000000000000000000000000000000000000000000"
    stop_at hs
      | gh.stop == z = hs
      | otherwise = case break ((== gh.stop) . headerHash) hs of
          (a, b : _) -> a <> [b]
          (a, []) -> a

headersMessage :: [BlockHeader] -> Message
headersMessage hs = MHeaders (Headers [(h, VarInt 0) | h <- hs])

-- | Block 1 of a regtest chain that Bitcoin Core 32.1 mined with
-- @-testactivationheight=segwit\@200@, so before SegWit was active. Its
-- hash is @55e949fa...bd71d75@. Core put a witness commitment in the
-- coinbase but no witness in it, as its miner does before activation, and
-- Core accepts the block. It has one transaction.
corePreSegWitBlock :: Block
corePreSegWitBlock =
  coreBlock
    "0000002006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f010cb977ece0103280894534\
    \38b7a55c1a016c9fa40c5205571956069623333bd8faf16affff7f200100000001020000000100000000000000000000\
    \00000000000000000000000000000000000000000000ffffffff025100feffffff0200f2052a010000001976a914636d\
    \4698f1523dc6688ccccd04850ade37a6c94988ac0000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa3\
    \6953755c690689799962b48bebd836974e8cf900000000"

-- | Block 302 of the same chain, after SegWit activated. Its hash is
-- @30f22128...3a62efb@ and its parent is block 301, @7bd5bfeb...b63e52b9@.
-- It has three transactions: the coinbase, whose witness is the 32-byte
-- reserved value; a P2WPKH spend, txid @9d304fed...6401d23d@ and wtxid
-- @a6b593b3...256a158e@; and a legacy spend without witness. Core accepted
-- it, and gave @bad-witness-merkle-match@ for a block like it whose spend had
-- one byte of its witness changed.
coreSegWitBlock :: Block
coreSegWitBlock =
  coreBlock
    "00000030b9523eb63f97c829f511bb532cf784c8827dc52b299f2fd994316deaebbfd57b9fa7b223e45c1d3509c35803\
    \274fc47a62cdbdd25f8b081e80d3c026577dfe7f0cfbf16affff7f200000000003020000000001010000000000000000\
    \000000000000000000000000000000000000000000000000ffffffff03022e01feffffff02ea0b834a000000001976a9\
    \145b0082ee7098f21fb42b61bf40ba80ed4714108288ac0000000000000000266a24aa21a9ed3689d337d76ca8ae1070\
    \b00bcfde4a16d82ae87ea9307d8705ef1b308792ef750120000000000000000000000000000000000000000000000000\
    \00000000000000002d0100000200000000010156ed4c57e51a1912b3d4a0c92dec5aa90e5db79f60ee3294490803bea1\
    \32ef1c0100000000fdffffff01d29f6659000000001976a9142d26b779a8053861ca5d01bf5d3880292280290288ac02\
    \47304402206b38df4637ea3c6474da4d1310260d3cd9738e0bd496ed33841c02973d65d56f0220465694613885394876\
    \ff1722e5675fd1e6dec5621b285fca4212c3e4a3135ade0121039e06e681933cef4160b02d907c68e3e54144b9ae0bdf\
    \d2c696d0d51132c6058c0000000002000000014e4d8f4536ded4cdfb0e1769f70e60904ee6fb6babca4cdf0dbf522030\
    \3c8985000000006a473044022013d2f4e1bd3a3f304d9028a4d1e3b4157989ac0481934c6250d8a29595b873d302201e\
    \df49731a27af137e0e0b6aee291d940db33033bbcebec78311c67650194472012102034d48cf5a3fcbd59d813ab02bec\
    \67ca2b351830ce6931a76c2ffe2529433d87fdffffff02f67e8b3b000000001976a914ca4b520b0a1c5d5595e337cd1f\
    \945bc9e2860b8c88ac40717759000000001976a9142b83d6a87a77b0f96f812d4df53830a64f6a909e88ac2d010000"

-- | A block from its serialization in hex, as @getblock@ prints it.
coreBlock :: Text -> Block
coreBlock hex =
  case decode =<< maybe (Left "bad hex") Right (decodeHex hex) of
    Left e -> error ("coreBlock: " <> e)
    Right b -> b

-- | Temporary RocksDB database.
withTestDB :: (DB -> IO a) -> IO a
withTestDB f =
  withSystemTempDirectory "haskoin-node-test-" $ \dir -> withDBAt dir f

-- | RocksDB database at a path, created if missing.
withDBAt :: FilePath -> (DB -> IO a) -> IO a
withDBAt dir = R.withDB dir def {R.createIfMissing = True}

-- | Remote end of an in-memory connection to a local peer process.
data Remote = Remote
  { net :: !Network,
    -- | bytes for the local peer, 'Nothing' closes the connection
    outbox :: !(Mailbox (Maybe ByteString)),
    -- | bytes from the local peer, 'Nothing' when it closed the connection
    inbox :: !(Inbox (Maybe ByteString)),
    buffer :: !(IORef ByteString),
    -- | set when the local peer closes the connection
    closed :: !(TVar Bool)
  }

-- | Connection to a remote peer implemented by the provided script. The
-- connection closes when the script returns.
memoryConnection :: Network -> (Remote -> IO ()) -> WithConnection
memoryConnection net script f = do
  (to_local_in, to_local) <- newMailbox
  (from_local_in, from_local) <- newMailbox
  buf <- newIORef B.empty
  closed <- newTVarIO False
  let remote =
        Remote
          { net = net,
            outbox = to_local,
            inbox = from_local_in,
            buffer = buf,
            closed = closed
          }
      src =
        receive to_local_in >>= \case
          Nothing -> return ()
          Just b -> yield b >> src
      snk = awaitForever $ \b -> Just b `send` from_local
      close_local = do
        atomically (writeTVar closed True)
        Nothing `send` from_local
  withAsync (script remote `finally` (Nothing `send` to_local)) $ \_ ->
    f (Conduits src snk) `finally` close_local

encodeMessage :: Network -> Message -> ByteString
encodeMessage net = runPut . putMessage net

remoteSend :: Remote -> Message -> IO ()
remoteSend r m = Just (encodeMessage r.net m) `send` r.outbox

remoteSendRaw :: Remote -> ByteString -> IO ()
remoteSendRaw r bs = Just bs `send` r.outbox

remoteClose :: Remote -> IO ()
remoteClose r = Nothing `send` r.outbox

-- | Receive next message from the local peer. 'Nothing' if it closed the
-- connection.
remoteReceive :: Remote -> IO (Maybe Message)
remoteReceive r =
  take_bytes 24 >>= \case
    Nothing -> return Nothing
    Just hdr -> do
      len <- case decode hdr of
        Left e -> assertFailure $ "Bad header from local peer: " <> e
        Right (MessageHeader _ _ len _) -> return len
      take_bytes (fromIntegral len) >>= \case
        Nothing -> return Nothing
        Just payload ->
          case runGet (getMessage r.net) (hdr <> payload) of
            Left e -> assertFailure $ "Bad message from local peer: " <> e
            Right m -> return (Just m)
  where
    take_bytes n = do
      b <- readIORef r.buffer
      if B.length b >= n
        then do
          let (x, y) = B.splitAt n b
          writeIORef r.buffer y
          return (Just x)
        else
          receive r.inbox >>= \case
            Nothing -> return Nothing
            Just more -> writeIORef r.buffer (b <> more) >> take_bytes n

-- | Receive messages from the local peer until one matches.
remoteReceiveMatch ::
  Double -> String -> Remote -> (Message -> Maybe a) -> IO a
remoteReceiveMatch s what r f = within s what go
  where
    go =
      remoteReceive r >>= \case
        Nothing -> assertFailure $ "Connection closed awaiting " <> what
        Just m -> maybe go return (f m)

zeroAddress :: Word64 -> NetworkAddress
zeroAddress srv = NetworkAddress srv (sockToHostAddress (SockAddrInet 0 0))

-- | Services that a full node of the network offers, as Bitcoin Core does:
-- the full chain, and witness data on a network with SegWit.
fullNodeServices :: Network -> Word64
fullNodeServices net
  | net.segWit = nodeNetwork .|. nodeWitness
  | otherwise = nodeNetwork

-- | Version message for a remote peer.
remoteVersion :: Network -> Word64 -> Word64 -> IO Version
remoteVersion net services nonce = do
  now <- nowTimestamp
  return $
    buildVersion
      net
      nonce
      0
      (zeroAddress services)
      (zeroAddress 0)
      (fromIntegral now)

-- | React to a message like a well-behaved full node with the provided
-- blocks (excluding genesis).
respondHonestly :: Remote -> TVar [Block] -> Message -> IO ()
respondHonestly r blocks = \case
  MVersion _ -> remoteSend r MVerAck
  MPing (Ping n) -> remoteSend r (MPong (Pong n))
  MGetHeaders gh -> do
    bs <- readTVarIO blocks
    remoteSend r . headersMessage $ serveHeaders r.net (map (.header) bs) gh
  MGetData (GetData ivs) -> do
    bs <- readTVarIO blocks
    let find_block (InvVector t h)
          | t `elem` [InvBlock, InvWitnessBlock] =
              listToMaybe [b | b <- bs, (headerHash b.header).get == h]
          | otherwise = Nothing
        found = mapMaybe (\iv -> (,) iv <$> find_block iv) ivs
        missing = [iv | iv <- ivs, iv `notElem` map fst found]
    forM_ found $ \(_, b) -> remoteSend r (MBlock b)
    unless (null missing) $ remoteSend r (MNotFound (NotFound missing))
  _ -> return ()

-- | Remote that completes the handshake and serves the provided blocks.
honestRemote :: TVar [Block] -> Remote -> IO ()
honestRemote blocks r = do
  v <- remoteVersion r.net (fullNodeServices r.net) =<< randomIO
  remoteSend r (MVersion v)
  let go =
        remoteReceive r >>= \case
          Nothing -> return ()
          Just m -> respondHonestly r blocks m >> go
  go

data TestNode = TestNode
  { node :: !Node,
    events :: !(Inbox NodeEvent),
    net :: !Network
  }

-- | Run a node against the provided peers using a temporary database. The
-- configuration can be adjusted before the node starts.
withTestNode ::
  Network ->
  [String] ->
  (SockAddr -> WithConnection) ->
  (NodeConfig -> NodeConfig) ->
  (TestNode -> IO a) ->
  IO a
withTestNode net peers connect modify f =
  withTestDB $ \db ->
    withTestLogger $ \logger ->
      withPublisher $ \pub ->
        withSubscription pub $ \sub -> do
          let cfg =
                NodeConfig
                  { maxPeers = 20,
                    db = db,
                    cf = Nothing,
                    peers = peers,
                    discover = False,
                    address = zeroAddress 0,
                    net = net,
                    pub = pub,
                    timeout = 120,
                    maxPeerLife = 48 * 3600,
                    connect = connect,
                    logger = logger
                  }
          withNode (modify cfg) $ \n ->
            f TestNode {node = n, events = sub, net = net}

setTimeout :: NominalDiffTime -> NodeConfig -> NodeConfig
setTimeout t c = c {NC.timeout = t}

setMaxPeerLife :: NominalDiffTime -> NodeConfig -> NodeConfig
setMaxPeerLife t c = c {NC.maxPeerLife = t}

waitForPeer :: TestNode -> IO Peer
waitForPeer t =
  receiveWithin 30 "peer connection" t.events $ \case
    PeerEvent (PeerConnected p) -> Just p
    _ -> Nothing

waitForDisconnect :: TestNode -> Peer -> IO ()
waitForDisconnect t p =
  receiveWithin 30 ("disconnection of peer " <> show p) t.events $ \case
    PeerEvent (PeerDisconnected p') | p == p' -> Just ()
    _ -> Nothing

-- | Wait until the best block header satisfies the condition.
waitForBest :: Double -> TestNode -> (BlockNode -> Bool) -> IO BlockNode
waitForBest s t f = do
  eventually s "best block header" $ f <$> chainGetBest t.node.chain
  chainGetBest t.node.chain
