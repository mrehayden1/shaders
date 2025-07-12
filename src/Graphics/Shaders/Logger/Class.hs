module Graphics.Shaders.Logger.Class (
  MonadLogger(..),
  LogLevel(..),

  logTrace,
  debug,
  info,
  warn,
  err
) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource

class MonadLogger m where
  loggerLevel :: m LogLevel
  loggerLog :: LogLevel -> String -> m ()

data LogLevel = LogTrace | LogDebug | LogInfo | LogWarn | LogError | LogNone
 deriving (Eq, Ord, Show)

logTrace :: MonadLogger m => String -> m ()
logTrace = loggerLog LogTrace

debug :: MonadLogger m => String -> m ()
debug = loggerLog LogDebug

info :: MonadLogger m => String -> m ()
info = loggerLog LogInfo

warn :: MonadLogger m => String -> m ()
warn = loggerLog LogWarn

err :: MonadLogger m => String -> m ()
err = loggerLog LogError


instance (Monad m, MonadLogger m) => MonadLogger (ResourceT m) where
  loggerLevel = lift loggerLevel
  loggerLog lvl = lift . loggerLog lvl

instance (Monad m, MonadLogger m) => MonadLogger (MaybeT m) where
  loggerLevel = lift loggerLevel
  loggerLog lvl = lift . loggerLog lvl

instance (Monad m, MonadLogger m) => MonadLogger (StateT s m) where
  loggerLevel = lift loggerLevel
  loggerLog lvl = lift . loggerLog lvl
