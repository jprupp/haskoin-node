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
    withConnection,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Data.Bits ((.&.))
import Data.Conduit.Network
  ( ClientSettings,
    appSink,
    appSource,
    clientSettings,
    runTCPClient,
  )
import Data.List (elemIndex)
import Data.String.Conversions (cs)
import Data.Word (Word8)
import Haskoin (Network (..))
import Haskoin.Node.Peer (Conduits (..), WithConnection)
import Network.Socket
  ( AddrInfo (..),
    NameInfoFlag (..),
    SockAddr (..),
    getAddrInfo,
    getNameInfo,
    hostAddress6ToTuple,
    hostAddressToTuple,
  )
import Text.Read (readMaybe)
import UnliftIO (MonadUnliftIO, SomeException, catch, liftIO)

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
toSockAddr :: Network -> String -> IO [SockAddr]
toSockAddr net str =
  resolve `catch` \(_ :: SomeException) -> return []
  where
    resolve = map (.addrAddress) <$> getAddrInfo Nothing host srv
    (host, srv) =
      second (<|> Just (show net.defaultPort)) (toHostService str)

-- | Whether an address that a peer relayed is worth connecting to: a public
-- unicast address with a port. Loopback, private, link-local, shared,
-- documentation, multicast and reserved ranges are refused, so that a peer
-- cannot make the node connect to services on its own host or network.
-- Static peers in the configuration are not checked.
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
      | otherwise -> True
isRoutable _ = False

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

-- | Open a TCP connection to a peer address and pass its conduits to a
-- function. The connection is closed when the function returns. Calls 'error'
-- if the address cannot be converted into a host and port.
withConnection :: SockAddr -> WithConnection
withConnection sa f =
  fromSockAddr sa >>= \case
    Nothing -> error $ "Invalid address " <> show sa
    Just settings ->
      runTCPClient settings $ \ad ->
        f Conduits {inboundConduit = appSource ad, outboundConduit = appSink ad}

-- | Turn a socket address into client settings for a TCP connection, or
-- 'Nothing' if it cannot be converted.
fromSockAddr :: (MonadUnliftIO m) => SockAddr -> m (Maybe ClientSettings)
fromSockAddr sa =
  resolve `catch` \(_ :: SomeException) -> return Nothing
  where
    flags = [NI_NUMERICHOST, NI_NUMERICSERV]
    resolve = do
      (host, port) <- liftIO (getNameInfo flags True True sa)
      return $ clientSettings <$> (readMaybe =<< port) <*> (cs <$> host)
