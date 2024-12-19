module Graphics (
  module Graphics.Class,
  GraphicsEnv,

  initialise
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk

import Graphics.Class
import Graphics.Device
import Graphics.Instance
import Graphics.Window

data GraphicsEnv = GraphicsEnv {
    graphicsDevice :: Device,
    graphicsSurface :: Vk.SurfaceKHR,
    graphicsVkInstance :: Vk.Instance
  }

initialise :: (MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> Codensity m (Maybe GraphicsEnv)
initialise window = do
  vkInstance <- createInstance
  surface <- createWindowSurface window vkInstance
  debug "Creating logical device..."
  mDevice <- createDevice vkInstance window surface
  return $ (\device -> GraphicsEnv device surface vkInstance) <$> mDevice
