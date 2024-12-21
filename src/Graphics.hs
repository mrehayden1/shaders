module Graphics (
  module Graphics.Class,
  GraphicsEnv,

  initialise
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk

import Graphics.Class
import Graphics.Device
import Graphics.Instance
import Graphics.Pipeline
import Graphics.Window

data GraphicsEnv = GraphicsEnv {
    graphicsDevice :: Device,
    graphicsSurface :: Vk.SurfaceKHR,
    graphicsVkInstance :: Vk.Instance
  }

initialise :: (MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> Codensity m (Maybe GraphicsEnv)
initialise window = runMaybeT $ do
  vkInstance <- lift createInstance
  vkSurface <- lift $ createWindowSurface window vkInstance
  device <- MaybeT $ createDevice vkInstance window vkSurface
  pipeline <- lift . createPipeline $ device
  return $ GraphicsEnv device vkSurface vkInstance
