module Window (
  Window,

  createWindow,
  destroyWindow,
  pollEvents,
  windowShouldClose
) where

import Control.Monad
import Graphics.UI.GLFW hiding (createWindow, destroyWindow)
import qualified Graphics.UI.GLFW as GLFW
import System.Exit

createWindow :: String -> IO Window
createWindow title = do
  initSuccess <- GLFW.init
  unless initSuccess $ do
    putStrLn "GLFW failed to initialise."
    exitFailure
  windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  windowHint $ WindowHint'Resizable False
  mWin <- GLFW.createWindow 1920 1080 title Nothing Nothing
  case mWin of
    Just win -> return win
    Nothing  -> do
      putStrLn "GLFW failed to create window."
      exitFailure

destroyWindow :: Window -> IO ()
destroyWindow win = do
  GLFW.destroyWindow win
  terminate
