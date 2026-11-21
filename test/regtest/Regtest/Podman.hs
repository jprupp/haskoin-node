-- | Bitcoin Core and Bitcoin Cash Node regtest nodes in a Podman container,
-- run by podman-test ("Test.Podman").
--
-- A container holds up to three nodes, which the tests start and stop with
-- @podman exec@. The container is in a pod of its own with no network: each
-- node's peer-to-peer port is bridged to a Unix socket of the pod, and the
-- node under test connects to it as the peer @unix:PATH@. Tests that run at
-- once each have their own sockets, so they never meet and take no port of
-- the machine. Inside the container, where nothing else runs, the ports are
-- fixed, and the nodes reach each other on @127.0.0.1@.
--
-- The image is built from @test\/regtest\/Containerfile@ and the script it
-- runs, read from the package folder, where the tests run. podman-test
-- tags it by a hash of both, and builds it once for every test process of
-- the machine.
module Regtest.Podman
  ( Impl (..),
    implName,
    implNet,
    Container (..),
    podmanUsable,
    withContainer,
    startNode,
    stopNode,
    freezeNode,
    thawNode,
    cli,
    hostAddress,
    nodeAddress,
    mine,
    mineTo,
    mineAddress,
    otherAddress,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, void)
import Data.ByteString qualified as B
import Data.Char (isHexDigit)
import Haskoin (Network, bchRegTest, btcRegTest)
import System.Exit (ExitCode (..))
import Test.Podman
  ( Address (..),
    ContainerSpec (..),
    Endpoint (..),
    Image (..),
    ImageSpec (..),
    Pod,
    PodSpec (..),
    address,
    container,
    containerName,
    execWithin,
    pod,
    podmanUsable,
    withPod,
  )
import Test.Podman.Command (podmanStatus)

data Impl = BitcoinCore | BitcoinCashNode
  deriving stock (Eq, Show)

implName :: Impl -> String
implName BitcoinCore = "Bitcoin Core"
implName BitcoinCashNode = "Bitcoin Cash Node"

implNet :: Impl -> Network
implNet BitcoinCore = btcRegTest
implNet BitcoinCashNode = bchRegTest

implDir :: Impl -> FilePath
implDir BitcoinCore = "/opt/bitcoin-core/bin/"
implDir BitcoinCashNode = "/opt/bchn/bin/"

-- | A running container for up to three nodes, in its pod.
data Container = Container
  { pod :: !Pod,
    impl :: !Impl
  }

-- | The peer-to-peer port of node number @i@ in the container. They are two
-- apart, and the RPC ports are above them, because a node may listen on the
-- port above its own for Tor.
p2pPort :: Int -> Int
p2pPort i = 18444 + 2 * i

-- | The RPC port of node number @i@ in the container.
rpcPort :: Int -> Int
rpcPort i = 18454 + 2 * i

-- | Number of nodes that can run in a container.
maxNodes :: Int
maxNodes = 3

-- | The name of the container in its pod.
regtest :: String
regtest = "regtest"

-- | The image of the nodes, from the Containerfile and its script in the
-- package folder.
nodeImage :: IO Image
nodeImage = do
  recipe <- B.readFile "test/regtest/Containerfile"
  script <- B.readFile "test/regtest/install-nodes.sh"
  return $
    Built
      ImageSpec
        { name = "haskoin-node-regtest",
          containerfile = recipe,
          files = [("install-nodes.sh", script)]
        }

-- | Run a container, idle until the tests start its nodes, and remove it
-- afterwards. Its pod is ready at once: the bridges listen on the sockets
-- before any node does, and 'startNode' waits for its node. The container
-- may use 2 GiB of memory, for its three nodes at most, so that a node that
-- runs away fails its test instead of taking the memory of the machine.
withContainer :: Impl -> (Container -> IO a) -> IO a
withContainer impl f = do
  image <- nodeImage
  let spec =
        (container regtest image)
          { memory = Just "2g",
            endpoints = [Bridged ("p2p" <> show i) (p2pPort i) | i <- [0 .. maxNodes - 1]]
          }
  withPod (pod "regtest" [spec]) {ready = []} $ \p -> f Container {pod = p, impl}

dataDir :: Int -> FilePath
dataDir i = "/data/node" <> show i

exec :: Container -> [String] -> IO String
exec c = execWithin 120 c.pod regtest

-- | Start node number @i@ in the container with extra arguments. Data
-- persists across restarts.
startNode :: Container -> Int -> [String] -> IO ()
startNode c i extra = do
  void $ exec c ["mkdir", "-p", dataDir i]
  void . exec c $
    [ implDir c.impl <> "bitcoind",
      "-regtest",
      "-daemon",
      "-datadir=" <> dataDir i,
      "-port=" <> show (p2pPort i),
      "-bind=0.0.0.0:" <> show (p2pPort i),
      "-rpcport=" <> show (rpcPort i),
      "-listen=1",
      "-listenonion=0",
      "-fallbackfee=0.0002",
      "-debug=net"
    ]
      <> extra
  void $ cli' 60 c i ["-rpcwait", "getblockcount"]

-- | Stop node number @i@ and wait until it exits.
stopNode :: Container -> Int -> IO ()
stopNode c i = do
  void $ cli c i ["stop"]
  let pidfile = dataDir i <> "/regtest/bitcoind.pid"
      wait n = do
        (code, _, _) <- podmanStatus 60 ["exec", containerName c.pod regtest, "test", "-e", pidfile]
        unless (code /= ExitSuccess || n > (300 :: Int)) $
          threadDelay 100000 >> wait (n + 1)
  wait 0
  threadDelay 500000

-- | Stop node number @i@ in its tracks, with @SIGSTOP@: its connections stay
-- open and it answers nothing on them, as a peer that hangs does. Its RPC
-- does not answer either until 'thawNode'.
freezeNode :: Container -> Int -> IO ()
freezeNode c i = signalNode c i "STOP"

-- | Let node number @i@ go on after 'freezeNode', with @SIGCONT@.
thawNode :: Container -> Int -> IO ()
thawNode c i = signalNode c i "CONT"

signalNode :: Container -> Int -> String -> IO ()
signalNode c i sig =
  void $ exec c ["sh", "-c", "kill -" <> sig <> " $(cat " <> dataDir i <> "/regtest/bitcoind.pid)"]

cli' :: Int -> Container -> Int -> [String] -> IO String
cli' secs c i args =
  execWithin secs c.pod regtest $
    [ implDir c.impl <> "bitcoin-cli",
      "-regtest",
      "-datadir=" <> dataDir i,
      "-rpcport=" <> show (rpcPort i)
    ]
      <> args

-- | Call RPC method on node number @i@.
cli :: Container -> Int -> [String] -> IO String
cli = cli' 120

-- | Address of node number @i@ for other nodes in the container.
nodeAddress :: Container -> Int -> String
nodeAddress _ i = "127.0.0.1:" <> show (p2pPort i)

-- | The peer of node number @i@ for the node under test: the Unix socket
-- of the pod that is bridged to its peer-to-peer port, as @unix:PATH@.
hostAddress :: Container -> Int -> IO String
hostAddress c i = case address c.pod ("p2p" <> show i) of
  UnixSocket path -> return ("unix:" <> path)
  a -> fail ("The peer-to-peer endpoint of node " <> show i <> " is not a socket: " <> show a)

-- | Regtest address valid for both implementations.
mineAddress :: String
mineAddress = "mipcBbFg9gMiCh81Kj8tqqdgoZub1ZJRfn"

-- | Another regtest address. Mining a replacement chain to a different
-- address avoids recreating blocks identical to invalidated ones.
otherAddress :: String
otherAddress = "mfcGAzvis9JQAb6avB6WBGiGrgWzLxuGaC"

-- | Mine blocks on node number @i@ and return their hashes.
mine :: Container -> Int -> Int -> IO [String]
mine c i n = mineTo c i n mineAddress

-- | Mine blocks to an address on node number @i@ and return their hashes.
mineTo :: Container -> Int -> Int -> String -> IO [String]
mineTo c i n addr = do
  out <- cli' 600 c i ["generatetoaddress", show n, addr]
  return (filter is_hash (words (map clean out)))
  where
    clean x
      | isHexDigit x = x
      | otherwise = ' '
    is_hash w = length w == 64
