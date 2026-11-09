-- |
-- Module      : Haskoin.Node.Address
-- Copyright   : (c) Haskoin Developers
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
--
-- Peer addresses: parsing them, deciding which relayed addresses are worth
-- connecting to, and opening TCP connections.
--
-- "Haskoin.Node.PeerMgr" re-exports 'toHostService', 'toSockAddr' and
-- 'isRoutable', and "Haskoin.Node" re-exports 'withConnection', where they
-- used to be defined. They live here because none of them depends on the
-- peer manager or the node: they are about addresses and sockets only.
module Haskoin.Node.Address
  ( toHostService,
    toSockAddr,
    isRoutable,
    NetGroup (..),
    netGroup,
    withConnection,
    connectTimeout,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Exception (IOException, catch)
import Data.Bits (complement, shiftR, (.&.), (.|.))
import Conduit (liftIO, yield)
import Control.Exception (finally)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Conduit.Network (sinkSocket)
import Data.Streaming.Network (clientSettingsTCP, getReadBufferSize, getSocketFamilyTCP)
import Data.Time.Clock (NominalDiffTime)
import Data.List (elemIndex)
import Data.String.Conversions (cs)
import Data.Word (Word16, Word8)
import Haskoin (Network (..))
import Haskoin.Node.Peer (Conduits (..), WithConnection)
import Network.Socket
  ( AddrInfo (..),
    Family (AF_UNSPEC),
    NameInfoFlag (..),
    SockAddr (..),
    close,
    getAddrInfo,
    getNameInfo,
    hostAddress6ToTuple,
    hostAddressToTuple,
    tupleToHostAddress6,
  )
import Network.Socket.ByteString (recv)
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- | Split a peer address into host and port. Accepts @host@, @host:port@,
-- @[ipv6]@ and @[ipv6]:port@. Either part is 'Nothing' if missing.
toHostService :: String -> (Maybe String, Maybe String)
toHostService str =
  let host = case m6 of
        Just (x, _) -> Just x
        Nothing -> case takeWhile (/= ':') str of
          [] -> Nothing
          xs -> Just xs
      srv = case m6 of
        Just (_, y) -> s y
        Nothing -> s str
      s xs =
        case dropWhile (/= ':') xs of
          [] -> Nothing
          _ : ys -> Just ys
      m6 = case str of
        (x : xs)
          | x == '[' -> do
              i <- elemIndex ']' xs
              return $ second (drop 1) $ splitAt i xs
          | x == ':' -> do
              return (str, "")
        _ -> Nothing
   in (host, srv)

-- | Resolve a peer address given as in 'toHostService' into socket addresses,
-- using the default port of the network if none is given. Returns an empty list
-- if it cannot be resolved.
--
-- Only resolver failures, which are 'IOException's, are caught. Catching
-- every exception would also swallow the asynchronous exception that cancels
-- the thread doing the lookup, and the node could then never shut down.
toSockAddr :: Network -> String -> IO [SockAddr]
toSockAddr net str =
  resolve `catch` \(_ :: IOException) -> return []
  where
    resolve = map (.addrAddress) <$> getAddrInfo Nothing host srv
    (host, srv) =
      second (<|> Just (show net.defaultPort)) (toHostService str)

-- | Whether an address that a peer relayed, or that a DNS seed returned, is
-- worth connecting to: a public unicast address with a port. Loopback,
-- private, link-local, shared, documentation, multicast and reserved ranges
-- are refused, so that a peer or a resolver cannot make the node connect to
-- services on its own host or network. So are the ORCHID ranges
-- @2001:10::\/28@ and @2001:20::\/28@, which Bitcoin Core 32.1 refuses too
-- (@IsRFC4843@ and @IsRFC7343@). Static peers in the configuration are not
-- checked.
isRoutable :: SockAddr -> Bool
isRoutable (SockAddrInet port h) =
  port /= 0 && routable4 (hostAddressToTuple h)
isRoutable (SockAddrInet6 port _ h _) =
  port /= 0 && case hostAddress6ToTuple h of
    (0, 0, 0, 0, 0, 0xffff, hi, lo) ->
      routable4
        ( fromIntegral (hi `div` 256),
          fromIntegral (hi `mod` 256),
          fromIntegral (lo `div` 256),
          fromIntegral (lo `mod` 256)
        )
    (a, b, _, _, _, _, _, _)
      | a == 0 -> False -- unspecified, loopback, IPv4-compatible
      | a .&. 0xfe00 == 0xfc00 -> False -- unique local
      | a .&. 0xffc0 == 0xfe80 -> False -- link-local
      | a .&. 0xffc0 == 0xfec0 -> False -- site-local
      | a .&. 0xff00 == 0xff00 -> False -- multicast
      | a == 0x2001 && b == 0x0db8 -> False -- documentation
      | a == 0x2001 && b .&. 0xfff0 == 0x0010 -> False -- ORCHID
      | a == 0x2001 && b .&. 0xfff0 == 0x0020 -> False -- ORCHIDv2
      | otherwise -> True
isRoutable _ = False

-- | A group of addresses that one operator can easily hold many of, from the
-- same block of a network. The node opens at most one connection to each
-- group from the addresses it learned, so that one operator cannot take every
-- slot by gossiping many addresses of its own.
data NetGroup
  = -- | loopback addresses
    LocalGroup
  | -- | other addresses that are not routable
    UnroutableGroup
  | -- | an IPv4 address, or an IPv6 address that carries one, by its first
    -- two bytes (a /16)
    IPv4Group !Word8 !Word8
  | -- | another IPv6 address, by its first four bytes (a /32), or its first
    -- four and a half for Hurricane Electric (a /36)
    IPv6Group ![Word8]
  deriving stock (Eq, Ord, Show)

-- | The network group of an address, as @NetGroupManager::GetGroup@ in
-- Bitcoin Core 32.1 forms it without an AS map. The port does not count.
--
-- IPv4 addresses are grouped by /16. So are IPv6 addresses that carry an
-- IPv4 address, by that address: IPv4-mapped (@::ffff:0:0\/96@), NAT64
-- (@64:ff9b::\/96@), SIIT (@::ffff:0:0:0\/96@), 6to4 (@2002::\/16@) and
-- Teredo (@2001::\/32@, with the bits of the address inverted). Hurricane
-- Electric (@2001:470::\/32@) is grouped by /36, and the rest of IPv6 by
-- /32.
netGroup :: SockAddr -> NetGroup
netGroup = \case
  SockAddrInet _ h -> v4 (hostAddressToTuple h)
  SockAddrInet6 _ _ h _ -> v6 (hostAddress6ToTuple h)
  _ -> UnroutableGroup
  where
    v4 t@(a, b, _, _)
      | a == 127 || a == 0 = LocalGroup
      | not (routable4 t) = UnroutableGroup
      | otherwise = IPv4Group a b
    v6 t@(a, b, c, d, e, f, g, h)
      | t == (0, 0, 0, 0, 0, 0, 0, 1) = LocalGroup
      | (a, b, c, d, e, f) == (0, 0, 0, 0, 0, 0xffff) = v4 (word32 g h)
      | not (isRoutable (SockAddrInet6 1 0 (tupleToHostAddress6 t) 0)) = UnroutableGroup
      | (a, b, c, d, e, f) == (0x64, 0xff9b, 0, 0, 0, 0) = linked g h
      | (a, b, c, d, e, f) == (0, 0, 0, 0, 0xffff, 0) = linked g h
      | a == 0x2002 = linked b c
      | (a, b) == (0x2001, 0) = linked (complement g) (complement h)
      | (a, b) == (0x2001, 0x0470) = IPv6Group (bytes [a, b] <> [hi c .|. 0x0f])
      | otherwise = IPv6Group (bytes [a, b])
    linked g h = let (w, x, _, _) = word32 g h in IPv4Group w x
    word32 :: Word16 -> Word16 -> (Word8, Word8, Word8, Word8)
    word32 g h = (hi g, lo g, hi h, lo h)
    bytes = concatMap (\w -> [hi w, lo w])
    hi, lo :: Word16 -> Word8
    hi w = fromIntegral (w `shiftR` 8)
    lo w = fromIntegral (w .&. 0xff)

-- | Whether an IPv4 address is public unicast.
routable4 :: (Word8, Word8, Word8, Word8) -> Bool
routable4 (a, b, c, _)
  | a == 0 = False -- this network
  | a == 10 = False -- private
  | a == 100 && b .&. 0xc0 == 64 = False -- shared address space
  | a == 127 = False -- loopback
  | a == 169 && b == 254 = False -- link-local
  | a == 172 && b .&. 0xf0 == 16 = False -- private
  | a == 192 && b == 0 && c == 0 = False -- protocol assignments
  | a == 192 && b == 0 && c == 2 = False -- documentation
  | a == 192 && b == 168 = False -- private
  | a == 198 && b .&. 0xfe == 18 = False -- benchmarking
  | a == 198 && b == 51 && c == 100 = False -- documentation
  | a == 203 && b == 0 && c == 113 = False -- documentation
  | a >= 224 = False -- multicast and reserved
  | otherwise = True

-- | Longest wait for a TCP connection to open: @DEFAULT_CONNECT_TIMEOUT@ in
-- Bitcoin Core 32.1, 5 seconds.
connectTimeout :: NominalDiffTime
connectTimeout = 5

-- | Open a TCP connection to a peer address and pass its conduits to a
-- function. The connection is closed when the function returns. Calls 'error'
-- if the address cannot be converted into a host and port.
--
-- Opening the connection fails with an 'IOException' after 'connectTimeout'.
-- Without the limit, an address that drops connection requests held a slot
-- of the peer manager until the operating system gave up, about two minutes
-- on Linux. The peer manager cannot stop a connection that is still opening,
-- because the peer process reads its mailbox only once the connection is
-- open.
withConnection :: SockAddr -> WithConnection
withConnection sa f =
  fromSockAddr sa >>= \case
    Nothing -> error $ "Invalid address " <> show sa
    Just (host, port) ->
      timeout micros (getSocketFamilyTCP host port AF_UNSPEC) >>= \case
        Nothing -> ioError (userError ("Connection to " <> show sa <> " timed out"))
        Just (s, _) ->
          f Conduits {inboundConduit = source s, outboundConduit = sinkSocket s}
            `finally` close s
  where
    micros = floor (connectTimeout * 1000000)
    -- Pieces of the size that the TCP client of streaming-commons reads.
    size = getReadBufferSize (clientSettingsTCP 0 "")
    source s = do
      bs <- liftIO (recv s size)
      unless (B.null bs) (yield bs >> source s)

-- | Turn a socket address into a numeric host and a port, or 'Nothing' if it
-- cannot be converted.
fromSockAddr :: SockAddr -> IO (Maybe (ByteString, Int))
fromSockAddr sa =
  resolve `catch` \(_ :: IOException) -> return Nothing
  where
    flags = [NI_NUMERICHOST, NI_NUMERICSERV]
    resolve = do
      (host, port) <- getNameInfo flags True True sa
      return $ (,) <$> (cs <$> host) <*> (readMaybe =<< port)
