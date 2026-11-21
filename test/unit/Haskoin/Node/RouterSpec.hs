-- | The order in which the router passes on the events of each peer,
-- driven through arbitrary interleavings of the events of several peers.
module Haskoin.Node.RouterSpec (spec) where

import Data.List (nub, sort)
import Haskoin.Node.Router
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Discard)

-- | A peer, and a message, numbered in the order it arrived.
type Key = Int

type Msg = Int

-- | What happens to one peer, in its own order.
data Ev
  = -- | the peer sends a message
    Msg
  | -- | the peer manager completed the handshake and announced the peer
    Done
  | -- | the announcement comes through the queue
    Conn
  | -- | the announcement of its disconnection comes through the queue
    Disc
  deriving stock (Eq, Show)

-- | What may happen to a peer, in an order that the node allows: messages
-- before the handshake, the end of the handshake, more messages, the
-- announcement, more messages, the disconnection. Any part may be missing,
-- as when a peer dies in its handshake, or the router does not hear from the
-- peer manager that the handshake completed because the peer was already
-- gone.
script :: Gen [Ev]
script = do
  early <- msgs
  done <- arbitrary
  held <- msgs
  late <- msgs
  stage <- chooseInt (0, 2)
  let afterDone = held <> [Conn | stage >= 1] <> (if stage >= 1 then late else []) <> [Disc | stage == 2]
  return (early <> [Done | done] <> afterDone)
  where
    msgs = do
      n <- chooseInt (0, 20)
      return (replicate n Msg)

-- | Merge the scripts of several peers in a random order that keeps the
-- order of each.
interleave :: [[a]] -> Gen [a]
interleave xss = case filter (not . null) xss of
  [] -> return []
  live -> do
    i <- chooseInt (0, length live - 1)
    let (front, chosen, back) = (take i live, live !! i, drop (i + 1) live)
    case chosen of
      x : rest -> (x :) <$> interleave (front <> [rest] <> back)
      [] -> interleave (front <> back)

-- | Number the messages and make inputs.
inputs :: [(Key, Ev)] -> [RouterInput Key Msg]
inputs = go 0
  where
    go _ [] = []
    go n ((k, e) : rest) = case e of
      Msg -> Arrived k n : go (n + 1) rest
      Done -> HandshakeDone k : go n rest
      Conn -> Connected k : go n rest
      Disc -> Disconnected k : go n rest

-- | The events of three peers, in an order the node allows.
newtype Allowed = Allowed [RouterInput Key Msg]
  deriving stock (Show)

instance Arbitrary Allowed where
  arbitrary = do
    scripts <- vectorOf 3 script
    Allowed . inputs <$> interleave [map (k,) s | (k, s) <- zip [1 ..] scripts]

-- | Any events at all of two peers, including orders the node rules out.
newtype Any = Any [RouterInput Key Msg]
  deriving stock (Show)

instance Arbitrary Any where
  arbitrary = do
    evs <- listOf ((,) <$> chooseInt (1, 2) <*> elements [Msg, Msg, Msg, Done, Conn, Disc])
    return (Any (inputs evs))

-- | Run the router, giving its last state and every action.
run :: [RouterInput Key Msg] -> (Router Key Msg, [RouterAction Key Msg])
run = foldl' go (newRouter, [])
  where
    go (r, acc) i = let (r', as) = stepRouter i r in (r', acc <> as)

keyOf :: RouterAction Key Msg -> Key
keyOf = \case
  Early k _ -> k
  Deliver k _ -> k
  Discard k _ -> k
  Announce k -> k
  Retire k -> k

-- | The message that an action disposes of, if any.
disposed :: RouterAction Key Msg -> Maybe Msg
disposed = \case
  Early _ m -> Just m
  Deliver _ m -> Just m
  Discard _ m -> Just m
  _ -> Nothing

delivered :: RouterAction Key Msg -> Maybe Msg
delivered = \case
  Deliver _ m -> Just m
  _ -> Nothing

arrived :: [RouterInput Key Msg] -> [Msg]
arrived is = [m | Arrived _ m <- is]

-- | Messages held at the end.
heldAtEnd :: Router Key Msg -> [Msg]
heldAtEnd r = concat [toList' q | k <- [1 .. 3], Just (Awaiting q) <- [routeOf k r]]
  where
    toList' = foldr (:) []

-- | No message of a peer is delivered unless the peer was announced since it
-- was last retired.
deliversOnlyAnnounced :: [RouterAction Key Msg] -> Bool
deliversOnlyAnnounced = go []
  where
    go _ [] = True
    go live (a : rest) = case a of
      Announce k -> go (k : live) rest
      Retire k -> k `elem` live && go (filter (/= k) live) rest
      Deliver k _ -> k `elem` live && go live rest
      _ -> go live rest

spec :: Spec
spec = describe "router" $ do
  it "handles the messages of a peer before its handshake at once" $
    snd (run [Arrived 1 0, Arrived 1 1]) `shouldBe` [Early 1 0, Early (1 :: Key) (1 :: Msg)]

  it "holds the messages that come between the handshake and the announcement" $ do
    let (r, actions) = run [HandshakeDone 1, Arrived 1 0, Arrived 1 1]
    actions `shouldBe` []
    heldAtEnd r `shouldBe` [0, 1]

  it "passes on the announcement, then the held messages in order" $
    snd (run [HandshakeDone 1, Arrived 1 0, Arrived 1 1, Connected 1, Arrived 1 2])
      `shouldBe` [Announce 1, Deliver 1 0, Deliver 1 1, Deliver 1 2]

  it "passes on the disconnection after every message" $
    snd (run [Connected 1, Arrived 1 0, Disconnected 1])
      `shouldBe` [Announce 1, Deliver 1 0, Retire 1]

  it "forgets a peer once it is gone" $
    fst (run [HandshakeDone 1, Connected 1, Disconnected (1 :: Key)]) `shouldBe` (newRouter :: Router Key Msg)

  it "announces a peer whose handshake it did not see end" $
    snd (run [Arrived 1 0, Connected 1, Arrived 1 1])
      `shouldBe` [Early 1 0, Announce 1, Deliver (1 :: Key) (1 :: Msg)]

  it "keeps peers apart" $
    snd (run [HandshakeDone 1, Arrived 1 0, Arrived 2 1, Connected 1])
      `shouldBe` [Early 2 1, Announce 1, Deliver (1 :: Key) (0 :: Msg)]

  describe "in any order that the node allows" $ do
    prop "announces a peer before any of its messages past the handshake, and retires it after" $
      \(Allowed is) ->
        let actions = snd (run is)
         in deliversOnlyAnnounced actions
              .&&. conjoin
                [ notElem (Announce k) (dropWhile (/= Retire k) actions)
                    .&&. all ((/= k) . keyOf) (drop 1 (dropWhile (/= Retire k) actions))
                | k <- [1 .. 3]
                ]

    prop "disposes of every message exactly once, or holds it" $
      \(Allowed is) ->
        let (r, actions) = run is
            done = [m | Just m <- map disposed actions]
         in sort (done <> heldAtEnd r) === arrived is
              .&&. length (nub done) === length done

    prop "delivers the messages of a peer in the order they came" $
      \(Allowed is) ->
        let actions = snd (run is)
         in conjoin
              [ let ms = [m | a <- actions, keyOf a == k, Just m <- [delivered a]]
                 in ms === sort ms
              | k <- [1 .. 3]
              ]

    prop "drops no message that comes after the handshake" $
      \(Allowed is) ->
        let actions = snd (run is)
            afterDone k = dropWhile (/= HandshakeDone k) is
            late k = [m | Arrived k' m <- afterDone k, k' == k]
            early = [m | Early _ m <- actions] <> [m | Discard _ m <- actions]
         in conjoin [all (`notElem` early) (late k) | k <- [1 .. 3]]

  describe "in any order at all" $ do
    prop "delivers nothing of a peer that is not announced" $
      \(Any is) -> deliversOnlyAnnounced (snd (run is))

    prop "disposes of no message twice" $
      \(Any is) ->
        let done = [m | Just m <- map disposed (snd (run is))]
         in length (nub done) === length done
