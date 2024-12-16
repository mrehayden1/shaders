module Graphics.Class (
  Logger(..),
  Log(..),
  LogLevel(..),

  debug,
  info,
  warn,
  err
) where

import Text.Printf

class Logger m where
  loggerLog :: Log -> m ()

data LogLevel = LogDebug | LogInfo | LogWarn | LogError
 deriving (Show)

data Log = Log {
    logLevel :: LogLevel,
    logMessage :: String
  }

debug :: Logger m => String -> m ()
debug = loggerLog . Log LogDebug

info :: Logger m => String -> m ()
info = loggerLog . Log LogInfo

warn :: Logger m => String -> m ()
warn = loggerLog . Log LogWarn

err :: Logger m => String -> m ()
err = loggerLog . Log LogError

-- | A basic, non threadsafe, uncofigurable implementation of Logger, for
-- testing.
instance Logger IO where
  loggerLog Log{..} = printf "[%s] %s\n" (logLevelStr logLevel) logMessage

logLevelStr :: LogLevel -> String
logLevelStr LogDebug = "DEBUG"
logLevelStr LogInfo  = "INFO "
logLevelStr LogWarn  = "WARN "
logLevelStr LogError = "ERROR"
