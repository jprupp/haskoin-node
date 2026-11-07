{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Logging to standard error. A 'Logger' is a plain value carried in
-- configuration records, so code that logs only needs 'MonadIO'.
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
import UnliftIO (MonadIO, bracket, liftIO)

-- | Severity of a log message, from least to most severe.
data LogLevel
  = -- | detailed tracing, useful when debugging
    LevelDebug
  | -- | normal operation, such as peers connecting and blocks arriving
    LevelInfo
  | -- | unexpected events that the node recovers from
    LevelWarn
  | -- | failures
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
logAt :: (MonadIO m) => LogLevel -> Logger -> Text -> m ()
logAt l logger msg =
  when (l >= logger.level) . liftIO . logger.push $
    "[" <> toLogStr (drop 5 (show l)) <> tag <> "] " <> toLogStr msg <> "\n"
  where
    tag
      | T.null logger.source = ""
      | otherwise = "#" <> toLogStr logger.source

-- | Log at 'LevelDebug', 'LevelInfo', 'LevelWarn' or 'LevelError'.
logDebug, logInfo, logWarn, logError :: (MonadIO m) => Logger -> Text -> m ()
logDebug = logAt LevelDebug
logInfo = logAt LevelInfo
logWarn = logAt LevelWarn
logError = logAt LevelError
