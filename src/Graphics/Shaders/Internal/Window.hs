module Graphics.Shaders.Internal.Window (
  HasWindow(..),

  Window,
  createWindow,
  createWindowSurface,

  pollEvents,
  windowShouldClose
) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Resource
import Foreign
import Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import Vulkan.CStruct
import Vulkan.Core10.AllocationCallbacks
import qualified Vulkan.Exception as Vk

import Graphics.Shaders.Exception
import Graphics.Shaders.Logger.Class

class HasWindow m where
  getWindow :: m Window

pollEvents :: MonadIO m => m ()
pollEvents = liftIO GLFW.pollEvents

windowShouldClose :: (MonadIO m, HasWindow m) => m Bool
windowShouldClose = do
  window <- getWindow
  liftIO $ GLFW.windowShouldClose window


createWindow :: MonadIO m => String -> ResourceT m Window
createWindow title = do
  fmap snd . allocate createWindow' $ destroyWindow
 where
  createWindow' = do
    initSuccess <- GLFW.init
    unless initSuccess $ do
      let msg = "GLFW failed to initialise."
      putStrLn msg
      throw $ ShadersInitializationException msg
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
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
    GLFW.terminate


createWindowSurface :: (MonadAsyncException m, MonadLogger m)
  => Maybe AllocationCallbacks
  -> Window
  -> Vk.Instance
  -> ResourceT m Vk.SurfaceKHR
createWindowSurface allocator window vkInstance = do
  debug "Creating surface..."
  fmap snd . allocate createWindowSurface' $ destroyWindowSurface
 where
  createWindowSurface' = evalContT $ do
    let instancePtr = castPtr $ Vk.instanceHandle vkInstance
    allocatorPtr <- case allocator of
      Nothing -> pure nullPtr
      Just a -> ContT $ withCStruct a
    surfacePtr <- ContT alloca
    res <- fmap Vk.Result
      . liftIO . GLFW.createWindowSurface instancePtr window allocatorPtr
      $ surfacePtr
    -- Throw an exception on error the same way our Vulkan bindings do.
    when (res < Vk.SUCCESS) . liftIO . throw . Vk.VulkanException $ res
    liftIO $ peek surfacePtr

  destroyWindowSurface surface = do
    Vk.destroySurfaceKHR vkInstance surface allocator
