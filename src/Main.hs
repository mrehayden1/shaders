module Main (
  main
) where

import Control.Monad.Codensity
import Control.Monad.Reader
import Data.IORef
import Data.Time.Clock
import Text.Printf

import Data.Linear
import Graphics.Shaders
import Window

appName :: String
appName = "Bagatelle"

appVersion :: String
appVersion = "0.1.0"

appLoggingLevel :: LogLevel
appLoggingLevel = LogTrace

vertexData :: [(V2 Float, V3 Float)]
vertexData = [
    (V2   0.0  (-0.5), V3 1.0 0.0 0.0),
    (V2 (-0.5)   0.5 , V3 0.0 0.0 1.0),
    (V2   0.5    0.5 , V3 0.0 1.0 0.0)
  ]

main :: IO ()
main = do
  liftIO $ printf "\nStarting %s v%s...\n\n" appName appVersion
  liftIO $ putStrLn "Copyright 2024 Categorical Industries.\n"

  runLoggerT appLoggingLevel . flip runCodensity return $ do
    liftIO $ putStrLn "Initialising graphics..."

    window <- withWindow appName

    runShadersT window $ do

      timeRef <- liftIO $ newIORef =<< getCurrentTime

      vertexBuffer <- withBuffer vertexData
      pipeline <- withPipeline @(V2 Float, V3 Float)
        $ \(pos, clr) -> (vec4 (_x pos) (_y pos) 0 1, clr)

      liftIO $ putStrLn "Finished startup."
      liftIO $ putStrLn "Running...\n"

      let loop = do
            drawFrame pipeline vertexBuffer
            swap
            startTime <- liftIO $ readIORef timeRef
            endTime <- liftIO getCurrentTime
            liftIO $ writeIORef timeRef endTime
            let frameTime = realToFrac . diffUTCTime endTime
                              $ startTime :: Float
            trace . printf "Frame time: %.5fms" . (* 1000) $ frameTime
            trace . printf "FPS: %.5f" . (1/) $ frameTime
            liftIO pollEvents
            close <- liftIO $ windowShouldClose window
            unless close loop

      loop

      liftIO $ putStrLn "Exiting..."
      debug "Awaiting graphics idle."
      awaitIdle

  liftIO $ putStrLn "Exited."
