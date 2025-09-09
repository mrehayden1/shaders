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

class Monad m => MonadLogger m where
  loggerLevel :: m LogLevel
  default loggerLevel :: (t m' ~ m, MonadTrans t, MonadLogger m') => m LogLevel
  loggerLevel = lift loggerLevel
  loggerLog :: LogLevel -> String -> m ()
  loggerLog lvl = lift . loggerLog lvl
  default loggerLog :: (t m' ~ m, MonadTrans t, MonadLogger m')
    => LogLevel
    -> String
    -> m ()

data LogLevel = LogTrace | LogDebug | LogInfo | LogWarn | LogError | LogNone
 deriving (Eq, Ord, Show)

-- Extra information
logTrace :: MonadLogger m => String -> m ()
logTrace = loggerLog LogTrace

-- Debugging
debug :: MonadLogger m => String -> m ()
debug = loggerLog LogDebug

-- Information
info :: MonadLogger m => String -> m ()
info = loggerLog LogInfo

-- Recoverable errors.
warn :: MonadLogger m => String -> m ()
warn = loggerLog LogWarn

-- Unrecoverable errors.
err :: MonadLogger m => String -> m ()
err = loggerLog LogError


instance (MonadLogger m) => MonadLogger (ResourceT m) where
  loggerLevel = lift loggerLevel
  loggerLog lvl = lift . loggerLog lvl

instance (MonadLogger m) => MonadLogger (MaybeT m) where
  loggerLevel = lift loggerLevel
  loggerLog lvl = lift . loggerLog lvl

instance (MonadLogger m) => MonadLogger (StateT s m) where
  loggerLevel = lift loggerLevel
  loggerLog lvl = lift . loggerLog lvl
