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

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving (Eq, Ord, Show)

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

-- | Same output with a different tag.
withSource :: Text -> Logger -> Logger
withSource source logger = logger {source}

-- | Lines look like monad-logger's: @[Info#Chain] message@.
logAt :: (MonadIO m) => LogLevel -> Logger -> Text -> m ()
logAt l logger msg =
  when (l >= logger.level) . liftIO . logger.push $
    "[" <> toLogStr (drop 5 (show l)) <> tag <> "] " <> toLogStr msg <> "\n"
  where
    tag
      | T.null logger.source = ""
      | otherwise = "#" <> toLogStr logger.source

logDebug, logInfo, logWarn, logError :: (MonadIO m) => Logger -> Text -> m ()
logDebug = logAt LevelDebug
logInfo = logAt LevelInfo
logWarn = logAt LevelWarn
logError = logAt LevelError
