-- | Logging to standard error. A 'Logger' is a plain value carried in
-- configuration records, so code that logs only needs 'IO'.
--
-- The node used to log through a logging monad transformer. That forced every
-- function into that monad, and made polymorphic loops leak memory when GHC did
-- not specialise them (see "Haskoin.Node.Loop"). A plain value avoids both, and
-- programs that log elsewhere can supply their own @push@ function.
module Haskoin.Node.Logger
  ( Logger (..),
    LogLevel (..),
    withLogger,
    withSource,
    logAt,
    logDebug,
    logInfo,
    logWarn,
    logError,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import System.Log.FastLogger
  ( LogStr,
    LogType' (LogStderr),
    defaultBufSize,
    newFastLogger1,
    toLogStr,
  )

-- | Severity of a log message, from least to most severe.
data LogLevel
  = -- | everything, for debugging: each peer that connects or leaves, each
    -- message
    LevelDebug
  | -- | what an operator wants in a quiet log: the best block, headers in
    -- sync, peers again after having none
    LevelInfo
  | -- | something unusual that the node dealt with: a peer that sent
    -- something invalid and was disconnected, no peers left
    LevelWarn
  | -- | something wrong with this node, that someone should look at.
    -- Nothing that a peer does is an error.
    LevelError
  deriving (Eq, Ord, Show)

-- | Where and how to log. Every component of the node takes one in its
-- configuration. Build one with 'withLogger', or directly with any @push@
-- function to send log lines elsewhere.
data Logger = Logger
  { -- | messages below this level are dropped
    level :: !LogLevel,
    -- | tag printed with every message
    source :: !Text,
    -- | write one formatted line
    push :: !(LogStr -> IO ())
  }

-- | Log to standard error in the order messages are logged. Buffered lines are
-- written out when the action returns or throws.
withLogger :: LogLevel -> (Logger -> IO a) -> IO a
withLogger level go =
  bracket (newFastLogger1 (LogStderr defaultBufSize)) snd $ \(push, _) ->
    go Logger {level, source = "", push}

-- | Same output with a different tag. The node uses this to tag the messages of
-- each component, such as @Chain@ and @PeerMgr@.
withSource :: Text -> Logger -> Logger
withSource source logger = logger {source}

-- | Log a message at a level, if the level is at least that of the 'Logger'.
-- Lines look like monad-logger's: @[Info#Chain] message@.
logAt :: LogLevel -> Logger -> Text -> IO ()
logAt l logger msg =
  when (l >= logger.level) . logger.push $
    "[" <> toLogStr (drop 5 (show l)) <> tag <> "] " <> toLogStr msg <> "\n"
  where
    tag
      | T.null logger.source = ""
      | otherwise = "#" <> toLogStr logger.source

-- | Log at 'LevelDebug', 'LevelInfo', 'LevelWarn' or 'LevelError'.
logDebug, logInfo, logWarn, logError :: Logger -> Text -> IO ()
logDebug = logAt LevelDebug
logInfo = logAt LevelInfo
logWarn = logAt LevelWarn
logError = logAt LevelError
