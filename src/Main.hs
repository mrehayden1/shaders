module Main (
  main
) where

import Control.Monad
import Data.Function
import Text.Printf

import Graphics
import Window

appName :: String
appName = "Caracal"

appVersion :: String
appVersion = "0.1.0"

main :: IO ()
main = do
  printf "\nStarting %s v%s...\n\n" appName appVersion
  putStrLn "Copyright 2024 Categorical Industries.\n"

  win <- createWindow appName

  vkInstance <- Graphics.initialise

  putStrLn "Finished startup."
  putStrLn "Running...\n"

  fix $ \loop -> do
    pollEvents
    close <- windowShouldClose win
    unless close loop

  destroyWindow win
  Graphics.cleanup vkInstance

  putStrLn "Exiting..."
