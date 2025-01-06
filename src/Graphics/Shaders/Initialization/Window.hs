module Graphics.Shaders.Initialization.Window (
  createWindowSurface
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.IO.Class
import Foreign
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Exception as Vk

import Graphics.Shaders.Logger.Class

createWindowSurface :: (MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> Vk.Instance
  -> Codensity m Vk.SurfaceKHR
createWindowSurface window vkInstance = do
  Codensity $ bracket createWindowSurface' destroyWindowSurface
 where
  createWindowSurface' = do
    debug "Creating surface..."
    let instancePtr = castPtr $ Vk.instanceHandle vkInstance
    liftIO . alloca $ \surfacePtr -> do
      res <- fmap Vk.Result
        . liftIO . GLFW.createWindowSurface instancePtr window nullPtr
        $ surfacePtr
      -- Throw an exception on error the same way our Vulkan bindings do.
      when (res < Vk.SUCCESS) . liftIO . throw . Vk.VulkanException $ res
      peek surfacePtr

  destroyWindowSurface surface = do
    debug "Destroying surface."
    Vk.destroySurfaceKHR vkInstance surface Nothing
