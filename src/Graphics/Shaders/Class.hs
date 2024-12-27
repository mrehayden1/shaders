module Graphics.Shaders.Class (
  MonadLogger(..),
  Log(..),
  LogLevel(..),

  trace,
  debug,
  info,
  warn,
  err,

  hoistMaybe
) where

import Control.Monad.Codensity
import Control.Monad.State
import Control.Monad.Trans.Maybe

class MonadLogger m where
  loggerLevel :: m LogLevel
  loggerLog :: Log -> m ()

data LogLevel = LogTrace | LogDebug | LogInfo | LogWarn | LogError | LogNone
 deriving (Eq, Ord, Show)

data Log = Log {
    logLevel :: LogLevel,
    logMessage :: String
  }

trace :: MonadLogger m => String -> m ()
trace = loggerLog . Log LogTrace

debug :: MonadLogger m => String -> m ()
debug = loggerLog . Log LogDebug

info :: MonadLogger m => String -> m ()
info = loggerLog . Log LogInfo

warn :: MonadLogger m => String -> m ()
warn = loggerLog . Log LogWarn

err :: MonadLogger m => String -> m ()
err = loggerLog . Log LogError


instance (Monad m, MonadLogger m) => MonadLogger (Codensity m) where
  loggerLevel = lift loggerLevel
  loggerLog = lift . loggerLog

instance (Monad m, MonadLogger m) => MonadLogger (MaybeT m) where
  loggerLevel = lift loggerLevel
  loggerLog = lift . loggerLog

instance (Monad m, MonadLogger m) => MonadLogger (StateT s m) where
  loggerLevel = lift loggerLevel
  loggerLog = lift . loggerLog

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
