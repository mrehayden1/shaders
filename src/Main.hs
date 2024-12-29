module Main (
  main
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.IORef
import Data.Time.Clock
import System.Exit
import Text.Printf

import Graphics.Shaders as G
import Window

appName :: String
appName = "Bagatelle"

appVersion :: String
appVersion = "0.1.0"

appLoggingLevel :: LogLevel
appLoggingLevel = LogTrace


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
  loggerLog Log{..} = do
    l <- loggerLevel
    when (l <= logLevel) $
      liftIO $ printf "[%s] %s\n" (logLevelStr logLevel) logMessage
   where
    logLevelStr :: LogLevel -> String
    logLevelStr LogTrace = "TRACE"
    logLevelStr LogDebug = "\x1b[36mDEBUG\x1b[0m"
    logLevelStr LogInfo  = "\x1b[37mINFO \x1b[0m"
    logLevelStr LogWarn  = "\x1b[33mWARN \x1b[0m"
    logLevelStr LogError = "\x1b[31ERROR \x1b[0m"
    logLevelStr LogNone  = error "Don't use LogNone"

main :: IO ()
main = do
  printf "\nStarting %s v%s...\n\n" appName appVersion
  putStrLn "Copyright 2024 Categorical Industries.\n"

  runLoggerT appLoggingLevel . lowerCodensity $ do
    liftIO $ putStrLn "Initialising graphics..."

    debug "Creating window."
    window <- Codensity $ bracket
      (liftIO $ createWindow appName)
      (\window -> do
         debug "Destroying window."
         liftIO $ destroyWindow window
      )

    mGraphics <- G.initialise window
    unless (isJust mGraphics) . liftIO $ do
      putStrLn "Failed to initialise graphics"
      exitFailure
    let graphics = fromJust mGraphics

    liftIO $ putStrLn "Finished startup."
    liftIO $ putStrLn "Running...\n"

    timeRef <- liftIO $ newIORef =<< getCurrentTime

    let loop = do
          G.drawFrame graphics
          startTime <- liftIO $ readIORef timeRef
          endTime <- liftIO getCurrentTime
          liftIO $ writeIORef timeRef endTime
          let frameTime = realToFrac . diffUTCTime endTime $ startTime
                :: Float
          trace . printf "Frame time: %.5f" . (* 1000) $ frameTime
          trace . printf "FPS: %.5f" . (1/) $ frameTime
          liftIO pollEvents
          close <- liftIO $ windowShouldClose window
          unless close loop

    lift $ evalStateT loop 0

    liftIO $ putStrLn "Exiting..."
    debug "Awaiting idle."
    awaitIdle graphics

  putStrLn "Exited."
