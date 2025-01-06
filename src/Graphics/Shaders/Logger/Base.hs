module Graphics.Shaders.Logger.Base (
  module Graphics.Shaders.Logger.Class,
  LoggerT,
  runLoggerT,

  logger
) where

import Control.Monad.Exception
import Control.Monad.Reader
import Text.Printf

import Graphics.Shaders.Logger.Class

newtype LoggerT m a = LoggerT {
    unLoggerT :: ReaderT LogLevel m a
  } deriving (Functor, Applicative, Monad, MonadException, MonadAsyncException)

-- Local logging changes, useful for debugging.
logger :: Monad m => LogLevel -> LoggerT m a -> LoggerT m a
logger l = LoggerT . local (const l) . unLoggerT

runLoggerT :: LogLevel -> LoggerT m a -> m a
runLoggerT l = flip runReaderT l . unLoggerT

instance MonadTrans LoggerT where
  lift = LoggerT . lift

instance MonadIO m => MonadIO (LoggerT m) where
  liftIO = LoggerT . liftIO

instance MonadIO m => MonadLogger (LoggerT m) where
  loggerLevel = LoggerT ask
  loggerLog lvl msg = do
    minLvl <- loggerLevel
    when (minLvl <= lvl) $
      liftIO $ printf "[%s] %s\n" (logLevelStr lvl) msg
   where
    logLevelStr :: LogLevel -> String
    logLevelStr LogTrace = "TRACE"
    logLevelStr LogDebug = "\x1b[36mDEBUG\x1b[0m"
    logLevelStr LogInfo  = "\x1b[37mINFO \x1b[0m"
    logLevelStr LogWarn  = "\x1b[33mWARN \x1b[0m"
    logLevelStr LogError = "\x1b[31ERROR \x1b[0m"
    logLevelStr LogNone  = error "Don't use LogNone"
