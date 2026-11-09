module Haskoin.Node.AddressSpec (spec) where

import Control.Monad (forM_)
import Haskoin (Network (..), btcRegTest)
import Haskoin.Node (toHostService, toSockAddr)
import Network.Socket (SockAddr (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
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
