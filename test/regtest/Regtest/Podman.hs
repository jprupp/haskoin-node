-- | Bitcoin Core and Bitcoin Cash Node regtest nodes in Podman containers.
module Regtest.Podman
  ( Impl (..),
    implName,
    implNet,
    Container (..),
    podmanAvailable,
    buildImage,
    withContainer,
    startNode,
    stopNode,
    cli,
    hostAddress,
    nodeAddress,
    mine,
    mineTo,
    mineAddress,
    otherAddress,
  )
where

import Control.Exception (bracket)
import Control.Monad (unless, void)
import Data.Char (isHexDigit, isSpace)
import Data.List (dropWhileEnd)
import Haskoin (Network, bchRegTest, btcRegTest)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Random (randomRIO)
import UnliftIO (timeout)
import UnliftIO.Concurrent (threadDelay)

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

-- | Running container with ports for up to three nodes.
data Container = Container
  { cid :: !String,
    impl :: !Impl,
    -- | peer-to-peer port of each node inside the container
    p2pPorts :: ![Int],
    -- | RPC port of each node inside the container
    rpcPorts :: ![Int]
  }

-- | Number of nodes that can run in a container.
maxNodes :: Int
maxNodes = 3

image :: String
image = "localhost/haskoin-node-regtest:latest"

-- | Directory with the Containerfile, relative to the package root.
imageDir :: FilePath
imageDir = "test/regtest"

-- | Run podman and return its standard output. Throws on failure or if it
-- takes longer than two minutes.
podman :: [String] -> IO String
podman = podmanWithin 120

-- | Run podman with a timeout in seconds.
podmanWithin :: Int -> [String] -> IO String
podmanWithin secs args =
  timeout (secs * 1000000) (readProcessWithExitCode "podman" args "") >>= \case
    Nothing ->
      fail $
        "podman " <> unwords args <> " timed out after " <> show secs <> "s"
    Just (ExitSuccess, out, _) -> return (dropWhileEnd isSpace out)
    Just (ExitFailure c, out, err) ->
      fail $
        "podman "
          <> unwords args
          <> " exited with "
          <> show c
          <> ": "
          <> out
          <> err

podmanAvailable :: IO Bool
podmanAvailable =
  findExecutable "podman" >>= \case
    Nothing -> return False
    Just _ -> do
      (c, _, _) <- readProcessWithExitCode "podman" ["info"] ""
      return (c == ExitSuccess)

-- | Build the image. Cached layers make this quick after the first time.
buildImage :: IO ()
buildImage = void $ podmanWithin 1800 ["build", "--quiet", "--tag", image, imageDir]

-- | Run a container. Node ports inside it are random, and are published to
-- random ports on the loopback interface of the host.
withContainer :: Impl -> (Container -> IO a) -> IO a
withContainer impl = bracket start stop
  where
    start = do
      ports <- randomPorts (2 * maxNodes)
      let (p2p, rpc) = splitAt maxNodes ports
      cid <-
        podman $
          ["run", "--detach", "--rm"]
            <> concat [["--publish", "127.0.0.1::" <> show p] | p <- p2p]
            <> [image]
      return Container {cid = cid, impl = impl, p2pPorts = p2p, rpcPorts = rpc}
    stop :: Container -> IO ()
    stop c = void $ podman ["rm", "--force", "--time", "0", c.cid]

-- | Distinct random ports. They are at least two apart because nodes may
-- also listen on the next port up for Tor.
randomPorts :: Int -> IO [Int]
randomPorts n = go []
  where
    go ps
      | length ps == n = return (reverse ps)
      | otherwise = do
          p <- randomRIO (20000, 60000)
          if any (\q -> abs (p - q) < 2) ps then go ps else go (p : ps)

p2pPort :: Container -> Int -> Int
p2pPort c i = c.p2pPorts !! i

rpcPort :: Container -> Int -> Int
rpcPort c i = c.rpcPorts !! i

dataDir :: Int -> FilePath
dataDir i = "/data/node" <> show i

exec :: Container -> [String] -> IO String
exec c args = podman (["exec", c.cid] <> args)

-- | Like 'exec' with a timeout in seconds.
execWithin :: Int -> Container -> [String] -> IO String
execWithin secs c args = podmanWithin secs (["exec", c.cid] <> args)

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
      "-port=" <> show (p2pPort c i),
      "-bind=0.0.0.0:" <> show (p2pPort c i),
      "-rpcport=" <> show (rpcPort c i),
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
        (code, _, _) <-
          readProcessWithExitCode "podman" ["exec", c.cid, "test", "-e", pidfile] ""
        unless (code /= ExitSuccess || n > (300 :: Int)) $
          threadDelay 100000 >> wait (n + 1)
  wait 0
  threadDelay 500000

cli' :: Int -> Container -> Int -> [String] -> IO String
cli' secs c i args =
  execWithin secs c $
    [ implDir c.impl <> "bitcoin-cli",
      "-regtest",
      "-datadir=" <> dataDir i,
      "-rpcport=" <> show (rpcPort c i)
    ]
      <> args

-- | Call RPC method on node number @i@.
cli :: Container -> Int -> [String] -> IO String
cli = cli' 120

-- | Address of node number @i@ for other nodes in the container.
nodeAddress :: Container -> Int -> String
nodeAddress c i = "127.0.0.1:" <> show (p2pPort c i)

-- | Address and port on the host for node number @i@.
hostAddress :: Container -> Int -> IO String
hostAddress c i = do
  out <- podman ["port", c.cid, show (p2pPort c i) <> "/tcp"]
  case lines out of
    a : _ -> return a
    [] -> fail $ "No port mapping for node " <> show i

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
