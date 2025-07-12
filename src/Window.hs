module Window (
  Window,

  createWindow,

  destroyWindow,

  pollEvents,
  windowShouldClose
) where

import Control.Monad
import Control.Monad.Exception
import Graphics.UI.GLFW hiding (createWindow, destroyWindow)
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Shaders.Exception

createWindow :: String -> IO Window
createWindow title = do
  initSuccess <- GLFW.init
  unless initSuccess $ do
    let msg = "GLFW failed to initialise."
    putStrLn msg
    throw $ ShadersInitializationException msg
  windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
  windowHint $ WindowHint'Resizable False
  mWin <- GLFW.createWindow 1920 1080 title Nothing Nothing
  case mWin of
    Just win -> return win
    Nothing  -> do
      let msg = "GLFW failed to create window."
      putStrLn msg
      throw $ ShadersInitializationException msg

destroyWindow :: Window -> IO ()
destroyWindow win = do
  GLFW.destroyWindow win
  terminate
