module Window (
  Window,

  withWindow,
  pollEvents,
  windowShouldClose
) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Codensity
import Graphics.UI.GLFW hiding (createWindow, destroyWindow)
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Shaders.Exception
import Graphics.Shaders.Logger.Class

withWindow :: (MonadAsyncException m, MonadLogger m)
  => String
  -> Codensity m Window
withWindow name = do
  debug "Creating window."
  Codensity $ bracket
    (liftIO $ createWindow name)
    (\window -> do
       debug "Destroying window."
       liftIO $ destroyWindow window
    )

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
