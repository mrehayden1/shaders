module Main (
  main
) where

import Control.Monad.Codensity
import Control.Monad.Reader
import Data.IORef
import Data.Time.Clock
import Linear hiding (trace)
import Text.Printf

import Graphics.Shaders
import Graphics.Shaders.Buffer
import Graphics.Shaders.Draw
import Graphics.Shaders.Logger.Base
import Graphics.Shaders.Pipeline
import Window

appName :: String
appName = "Bagatelle"

appVersion :: String
appVersion = "0.1.0"

appLoggingLevel :: LogLevel
appLoggingLevel = LogTrace


main :: IO ()
main = do
  liftIO $ printf "\nStarting %s v%s...\n\n" appName appVersion
  liftIO $ putStrLn "Copyright 2024 Categorical Industries.\n"

  runLoggerT appLoggingLevel . flip runCodensity return $ do
    liftIO $ putStrLn "Initialising graphics..."

    window <- withWindow appName

    runShadersT window $ do

      liftIO $ putStrLn "Finished startup."
      liftIO $ putStrLn "Running...\n"

      timeRef <- liftIO $ newIORef =<< getCurrentTime

      vertexBuffer <- withVerticesAsBuffer vertexData
      pipeline <- withPipeline (\(V2 x y, clr) -> (V4 x y 0 1, clr))

      let loop = do
            drawFrame pipeline vertexBuffer
            swap
            startTime <- liftIO $ readIORef timeRef
            endTime <- liftIO getCurrentTime
            liftIO $ writeIORef timeRef endTime
            let frameTime = realToFrac . diffUTCTime endTime
                              $ startTime :: Float
            trace . printf "Frame time: %.5f" . (* 1000) $ frameTime
            trace . printf "FPS: %.5f" . (1/) $ frameTime
            liftIO pollEvents
            close <- liftIO $ windowShouldClose window
            unless close loop

      loop

      liftIO $ putStrLn "Exiting..."
      debug "Awaiting graphics idle."
      awaitIdle

  liftIO $ putStrLn "Exited."
