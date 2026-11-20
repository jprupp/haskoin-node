module Haskoin.Node.Peer.RemoteSpec (spec) where

import Data.Bits ((.|.))
import Data.Set qualified as Set
import Data.Word (Word64)
import Haskoin
import Haskoin.Network.Protocol qualified as P
import Haskoin.Node (buildVersion)
import Haskoin.Node.Peer.Remote
import Network.Socket (SockAddr (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "a mempool request" $ do
    it "is refused by Bitcoin Core without the mempool permission" $
      fst (judge (facts btc) MMempool (handshaken btc Set.empty plain)) `shouldSatisfy` refused
    it "is allowed by Bitcoin Core with the mempool permission" $
      fst (judge (facts btc) MMempool (handshaken btc (Set.singleton P.PermMempool) plain))
        `shouldBe` Allowed
    it "is dropped without a word by Bitcoin Core with noban" $
      fst (judge (facts btc) MMempool (handshaken btc (Set.singleton P.PermNoBan) plain))
        `shouldSatisfy` unanswered
    it "is allowed by a node that offers Bloom filters" $
      fst (judge (facts btc) MMempool (handshaken btc Set.empty bloom)) `shouldBe` Allowed
    it "is refused by Bitcoin Cash Node that offers no Bloom filters" $
      fst (judge (facts bch) MMempool (handshaken bch Set.empty plain)) `shouldSatisfy` refused
    it "is allowed by Bitcoin Cash Node that offers Bloom filters" $
      fst (judge (facts bch) MMempool (handshaken bch Set.empty bloom)) `shouldBe` Allowed

  describe "the version of the node" $ do
    it "replays the messages sent before it with the options it gives" $ do
      -- the mempool request is judged with the defaults of Bitcoin Cash
      -- Node, which offer Bloom filters; the node then says it offers none
      let r0 = sendAll bch (fresh bch Set.empty) [MVersion ours, MVerAck]
          r1 = learnVersion (theirs plain) r0
      fst (judge (facts bch) MMempool r0) `shouldBe` Allowed
      fst (judge (facts bch) MMempool r1) `shouldSatisfy` refused
    it "counts only once" $ do
      let r = learnVersion (theirs bloom) (handshaken bch Set.empty plain)
      fst (judge (facts bch) MMempool r) `shouldSatisfy` refused

  describe "the order of the handshake" $ do
    it "lets Bitcoin Core drop a request before the version without a word" $
      fst (judge (facts btc) MGetAddr (fresh btc Set.empty)) `shouldSatisfy` unanswered
    it "refuses a request before the version to Bitcoin Cash Node" $
      fst (judge (facts bch) MGetAddr (fresh bch Set.empty)) `shouldSatisfy` refused
    it "allows the version first" $
      fst (judge (facts btc) (MVersion ours) (fresh btc Set.empty)) `shouldBe` Allowed

  describe "a request for blocks" $ do
    it "is allowed for a block that this node does not know" $ do
      let unknown :: BlockHash
          unknown = "0000000000000000000000000000000000000000000000000000000000000001"
          iv = InvVector InvWitnessBlock unknown.get
      fst (judge (facts btc) (MGetData (GetData [iv])) (handshaken btc Set.empty plain))
        `shouldBe` Allowed
    it "of a filtered block is refused by a node without Bloom filters" $ do
      let iv = InvVector InvMerkleBlock (headerHash btc.genesisHeader).get
      fst (judge (facts btc) (MGetData (GetData [iv])) (handshaken btc Set.empty plain))
        `shouldSatisfy` refused

  describe "the answer to a refused transaction" $ do
    it "is a reject on Bitcoin Cash Node" $ expectsReject bch ours `shouldBe` True
    it "is nothing on Bitcoin Core" $ expectsReject btc ours `shouldBe` False
  where
    refused = \case
      Refused _ _ -> True
      _ -> False
    unanswered = \case
      Unanswered _ -> True
      _ -> False

-- | The services of a node that offers no Bloom filters, and of one that
-- does.
plain, bloom :: Word64
plain = nodeNetwork .|. nodeWitness
bloom = plain .|. nodeBloom

facts :: Network -> Facts
facts net = Facts {now = 1000000, best = genesisNode net, known = mempty}

fresh :: Network -> P.Permissions -> Remote
fresh net perms = newRemote net perms AddrIPv4 False 0 7

sendAll :: Network -> Remote -> [Message] -> Remote
sendAll net = foldl (\r m -> snd (judge (facts net) m r))

-- | A model whose handshake is complete, with a node that offers these
-- services.
handshaken :: Network -> P.Permissions -> Word64 -> Remote
handshaken net perms services =
  learnVersion (theirs services) (sendAll net (fresh net perms) [MVersion ours, MVerAck])

zero :: NetworkAddress
zero = NetworkAddress 0 (sockToHostAddress (SockAddrInet 0 0))

ours :: Version
ours = buildVersion btc 1 0 zero zero 0

theirs :: Word64 -> Version
theirs services = (buildVersion btc 2 0 zero zero 0) {services = services, relay = True}
