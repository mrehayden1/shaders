module Main (
  main
) where

import Control.Monad
import Data.Function
import Data.Maybe
import Text.Printf

import Graphics
import Window

appName :: String
appName = "Bagatelle"

appVersion :: String
appVersion = "0.1.0"

main :: IO ()
main = do
  printf "\nStarting %s v%s...\n\n" appName appVersion
  putStrLn "Copyright 2024 Categorical Industries.\n"

  window <- createWindow appName

  putStrLn "Initialising graphics."

  mGraphics <- Graphics.initialise window
  unless (isJust mGraphics) $
    putStrLn "Failed to create Vulkan Instance"
  let graphics = fromJust mGraphics

  putStrLn "Finished startup."
  putStrLn "Running...\n"

  fix $ \loop -> do
    pollEvents
    close <- windowShouldClose window
    unless close loop

  destroyWindow window
  Graphics.cleanup graphics

  putStrLn "Exiting..."
