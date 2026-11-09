module Haskoin.Node.AddressSpec (spec) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Word (Word16, Word8)
import Haskoin (Network (..), btcRegTest)
import Haskoin.Node (toHostService, toSockAddr)
import Haskoin.Node.Address (NetGroup (..), isRoutable, netGroup, withConnection)
import Network.Socket
  ( Family (AF_INET),
    SockAddr (..),
    SocketType (Stream),
    bind,
    close,
    connect,
    defaultProtocol,
    listen,
    socket,
    socketPort,
    tupleToHostAddress,
    tupleToHostAddress6,
  )
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

v4 :: (Word8, Word8, Word8, Word8) -> SockAddr
v4 = SockAddrInet 8333 . tupleToHostAddress

v6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> SockAddr
v6 t = SockAddrInet6 8333 0 (tupleToHostAddress6 t) 0

spec :: Spec
spec = do
  describe "routable addresses" $ do
    it "refuses the reserved IPv4 ranges" $
      forM_
        [ (198, 18, 0, 1), -- benchmarking, 198.18.0.0/15
          (198, 19, 255, 1),
          (192, 0, 2, 1), -- documentation
          (198, 51, 100, 1),
          (203, 0, 113, 1),
          (0, 1, 2, 3), -- this network
          (192, 0, 0, 1) -- protocol assignments
        ]
        $ \a -> isRoutable (v4 a) `shouldBe` False

    it "accepts the addresses just outside the benchmarking range" $ do
      isRoutable (v4 (198, 17, 255, 1)) `shouldBe` True
      isRoutable (v4 (198, 20, 0, 1)) `shouldBe` True

    it "refuses the reserved IPv6 ranges" $
      forM_
        [ (0xff02, 0, 0, 0, 0, 0, 0, 1), -- multicast
          (0xfec0, 0, 0, 0, 0, 0, 0, 1), -- site-local
          (0x2001, 0x0db8, 0, 0, 0, 0, 0, 1) -- documentation
        ]
        $ \a -> isRoutable (v6 a) `shouldBe` False

    it "refuses the ORCHID ranges, as Bitcoin Core does" $
      -- RFC 4843: 2001:10::/28; RFC 7343: 2001:20::/28
      forM_ [0x0010, 0x001f, 0x0020, 0x002f] $ \b ->
        isRoutable (v6 (0x2001, b, 0, 0, 0, 0, 0, 1)) `shouldBe` False

    it "accepts the addresses just outside the ORCHID ranges" $
      forM_ [0x000f, 0x0030] $ \b ->
        isRoutable (v6 (0x2001, b, 0, 0, 0, 0, 0, 1)) `shouldBe` True

  describe "connections" $ do
    it "gives up opening a connection that gets no answer after five seconds" $ do
      -- Bitcoin Core: DEFAULT_CONNECT_TIMEOUT = 5000 ms. A listening socket
      -- whose queue of one connection is full drops new requests on Linux,
      -- so opening a connection to it hangs.
      let loopback = tupleToHostAddress (127, 0, 0, 1)
          listener = do
            s <- socket AF_INET Stream defaultProtocol
            bind s (SockAddrInet 0 loopback)
            listen s 0
            return s
      bracket listener close $ \srv -> do
        port <- socketPort srv
        let sa = SockAddrInet port loopback
        bracket (socket AF_INET Stream defaultProtocol) close $ \filler -> do
          connect filler sa
          start <- getCurrentTime
          r <- try (withConnection sa (\_ -> return ()))
          end <- getCurrentTime
          either (\(_ :: IOException) -> True) (const False) r `shouldBe` True
          end `diffUTCTime` start `shouldSatisfy` (\d -> d >= 4.5 && d < 8)

  describe "network groups" $ do
    -- 45.1.2.3 is 0x2d010203.
    it "groups IPv4 addresses by their first two bytes, whatever the port" $ do
      netGroup (SockAddrInet 1000 (tupleToHostAddress (45, 1, 2, 3))) `shouldBe` IPv4Group 45 1
      netGroup (v4 (45, 1, 200, 9)) `shouldBe` IPv4Group 45 1
      netGroup (v4 (45, 2, 0, 1)) `shouldBe` IPv4Group 45 2

    it "groups IPv6 addresses that carry an IPv4 address by that address" $ do
      netGroup (v6 (0, 0, 0, 0, 0, 0xffff, 0x2d01, 0x0203)) `shouldBe` IPv4Group 45 1
      netGroup (v6 (0x0064, 0xff9b, 0, 0, 0, 0, 0x2d01, 0x0203)) `shouldBe` IPv4Group 45 1
      netGroup (v6 (0x2002, 0x2d01, 0x0203, 0, 0, 0, 0, 1)) `shouldBe` IPv4Group 45 1
      -- Teredo inverts the bits: 0xffffffff - 0x2d010203 = 0xd2fefdfc
      netGroup (v6 (0x2001, 0, 0, 0, 0, 0, 0xd2fe, 0xfdfc)) `shouldBe` IPv4Group 45 1

    it "groups other IPv6 addresses by their first four bytes" $ do
      netGroup (v6 (0x2a01, 0x04f8, 0x0001, 0, 0, 0, 0, 1))
        `shouldBe` IPv6Group [0x2a, 0x01, 0x04, 0xf8]
      netGroup (v6 (0x2a01, 0x04f8, 0xffff, 0, 0, 0, 0, 1))
        `shouldBe` IPv6Group [0x2a, 0x01, 0x04, 0xf8]

    it "groups Hurricane Electric addresses by 36 bits" $
      -- the fifth byte 0x12 with its low four bits set: 0x1f
      netGroup (v6 (0x2001, 0x0470, 0x1234, 0, 0, 0, 0, 1))
        `shouldBe` IPv6Group [0x20, 0x01, 0x04, 0x70, 0x1f]

    it "puts loopback and unroutable addresses in groups of their own" $ do
      netGroup (v4 (127, 0, 0, 1)) `shouldBe` LocalGroup
      netGroup (v6 (0, 0, 0, 0, 0, 0, 0, 1)) `shouldBe` LocalGroup
      netGroup (v4 (10, 0, 0, 1)) `shouldBe` UnroutableGroup

  let net = btcRegTest
  describe "addresses" $ do
    it "parses host and service"
      $ forM_
        [ ("localhost", (Just "localhost", Nothing)),
          ("::1", (Just "::1", Nothing)),
          ("localhost:8080", (Just "localhost", Just "8080")),
          ("example.com", (Just "example.com", Nothing)),
          ("api.example.com:443", (Just "api.example.com", Just "443")),
          ("api.example.com:http", (Just "api.example.com", Just "http")),
          ("[::1]", (Just "::1", Nothing)),
          ("[::1]:8080", (Just "::1", Just "8080")),
          ("[2002::dead:beef]:ssh", (Just "2002::dead:beef", Just "ssh"))
        ]
      $ \(s, r) -> toHostService s `shouldBe` r

    it "resolves numeric addresses with the default port" $ do
      as <- toSockAddr net "127.0.0.1"
      map show as `shouldContain` ["127.0.0.1:" <> show net.defaultPort]

    it "returns no addresses for unresolvable hosts" $
      toSockAddr net "host.invalid" `shouldReturn` []

    it "keeps an explicit port" $ do
      as <- toSockAddr net "127.0.0.1:1234"
      map show as `shouldContain` ["127.0.0.1:1234"]

    -- IPv4 only: resolving an IPv6 address needs IPv6 on the host, which
    -- containers and CI runners often lack. The IPv6 forms are still parsed
    -- by the toHostService tests above, which need no resolver.
    prop "resolves any numeric IPv4 address to itself" $ \(e, w) -> do
      let a = SockAddrInet (toEnum (e `mod` 65536)) w
      s <- toSockAddr net (show a)
      s `shouldStartWith` [a]
