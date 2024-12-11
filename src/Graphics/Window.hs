module Graphics.Window (
  getWindowSurface
) where

import Control.Exception
import Control.Monad
import Foreign
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Exception as Vk

getWindowSurface :: GLFW.Window -> Vk.Instance -> IO Vk.SurfaceKHR
getWindowSurface window vkInstance = do
  let instPtr = castPtr $ Vk.instanceHandle vkInstance
  alloca $ \surfacePtr -> do
    res <- Vk.Result <$> GLFW.createWindowSurface instPtr window nullPtr surfacePtr
    when (res < Vk.SUCCESS) . throwIO . Vk.VulkanException $ res
    peek surfacePtr
