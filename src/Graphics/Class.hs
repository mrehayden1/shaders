module Graphics.Class (
  MonadLogger(..),
  Log(..),
  LogLevel(..),

  debug,
  info,
  warn,
  err
) where

class MonadLogger m where
  loggerLevel :: m LogLevel
  loggerLog :: Log -> m ()

data LogLevel = LogDebug | LogInfo | LogWarn | LogError | LogNone
 deriving (Eq, Ord, Show)

data Log = Log {
    logLevel :: LogLevel,
    logMessage :: String
  }

debug :: MonadLogger m => String -> m ()
debug = loggerLog . Log LogDebug

info :: MonadLogger m => String -> m ()
info = loggerLog . Log LogInfo

warn :: MonadLogger m => String -> m ()
warn = loggerLog . Log LogWarn

err :: MonadLogger m => String -> m ()
err = loggerLog . Log LogError
