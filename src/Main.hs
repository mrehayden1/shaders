module Main (
  main
) where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import System.Exit
import Text.Printf

import Graphics
import Window

appName :: String
appName = "Bagatelle"

appVersion :: String
appVersion = "0.1.0"

appLoggingLevel :: LogLevel
appLoggingLevel = LogNone


newtype LoggerT m a = LoggerT {
    unLoggerT :: ReaderT LogLevel m a
  } deriving (Functor, Applicative, Monad)

runLogger :: LogLevel -> LoggerT m a -> m a
runLogger l = flip runReaderT l . unLoggerT

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
main = runLogger appLoggingLevel $ do
  liftIO $ printf "\nStarting %s v%s...\n\n" appName appVersion
  liftIO $ putStrLn "Copyright 2024 Categorical Industries.\n"

  window <- liftIO $ createWindow appName

  liftIO $ putStrLn "Initialising graphics..."

  mGraphics <- Graphics.initialise window
  unless (isJust mGraphics) . liftIO $ do
    putStrLn "Failed to initialise graphics"
    exitFailure
  let graphics = fromJust mGraphics

  liftIO $ putStrLn "Finished startup."
  liftIO $ putStrLn "Running...\n"

  fix $ \loop -> do
    liftIO pollEvents
    close <- liftIO $ windowShouldClose window
    unless close loop

  liftIO $ putStrLn "Exiting..."
  liftIO $ destroyWindow window
  Graphics.cleanup graphics
  liftIO $ putStrLn "Exited."
