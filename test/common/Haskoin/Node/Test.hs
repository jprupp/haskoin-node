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
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Default (def)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Serialize (decode, encode, runGet, runPut)
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
within :: (MonadUnliftIO m) => Double -> String -> m a -> m a
within s what act =
  timeout (round (s * 1000000)) act >>= \case
    Nothing -> liftIO . assertFailure $ "Timed out after " <> show s <> "s: " <> what
    Just x -> return x

-- | Fail the test unless the condition becomes true in the given seconds.
eventually :: (MonadUnliftIO m) => Double -> String -> m Bool -> m ()
eventually s what cond = within s what go
  where
    go = cond >>= \b -> unless b (threadDelay 20000 >> go)

-- | Receive the first matching message in the given seconds.
receiveWithin ::
  (MonadUnliftIO m, InChan mbox) =>
  Double ->
  String ->
  mbox msg ->
  (msg -> Maybe a) ->
  m a
receiveWithin s what mbox f = within s what (receiveMatch mbox f)

-- | Fail the test if a matching message arrives in the given seconds.
expectNothingWithin ::
  (MonadUnliftIO m, InChan mbox) =>
  Double ->
  String ->
  mbox msg ->
  (msg -> Maybe a) ->
  m ()
expectNothingWithin s what mbox f =
  timeout (round (s * 1000000)) (receiveMatch mbox f) >>= \case
    Nothing -> return ()
    Just _ -> liftIO . assertFailure $ "Unexpected: " <> what

-- | Current time as a block timestamp.
nowTimestamp :: (MonadIO m) => m Word32
nowTimestamp = floor <$> liftIO getPOSIXTime

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

remoteSend :: (MonadIO m) => Remote -> Message -> m ()
remoteSend r m = Just (encodeMessage r.net m) `send` r.outbox

remoteSendRaw :: (MonadIO m) => Remote -> ByteString -> m ()
remoteSendRaw r bs = Just bs `send` r.outbox

remoteClose :: (MonadIO m) => Remote -> m ()
remoteClose r = Nothing `send` r.outbox

-- | Receive next message from the local peer. 'Nothing' if it closed the
-- connection.
remoteReceive :: (MonadIO m) => Remote -> m (Maybe Message)
remoteReceive r = liftIO . runMaybeT $ do
  hdr <- MaybeT (take_bytes 24)
  len <- case decode hdr of
    Left e -> liftIO . assertFailure $ "Bad header from local peer: " <> e
    Right (MessageHeader _ _ len _) -> return len
  payload <- MaybeT (take_bytes (fromIntegral len))
  case runGet (getMessage r.net) (hdr <> payload) of
    Left e -> liftIO . assertFailure $ "Bad message from local peer: " <> e
    Right m -> return m
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
  (MonadUnliftIO m) => Double -> String -> Remote -> (Message -> Maybe a) -> m a
remoteReceiveMatch s what r f = within s what go
  where
    go =
      remoteReceive r >>= \case
        Nothing -> liftIO . assertFailure $ "Connection closed awaiting " <> what
        Just m -> maybe go return (f m)

zeroAddress :: Word64 -> NetworkAddress
zeroAddress srv = NetworkAddress srv (sockToHostAddress (SockAddrInet 0 0))

-- | Version message for a remote peer.
remoteVersion :: (MonadIO m) => Network -> Word64 -> Word64 -> m Version
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
  v <- remoteVersion r.net nodeNetwork =<< randomIO
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
            liftIO $ f TestNode {node = n, events = sub, net = net}

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
