module Graphics (
  module Graphics.Class,

  initialise,
  cleanup
) where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
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

initialise :: (MonadIO m, Logger m) => GLFW.Window -> m (Maybe GraphicsEnv)
initialise window = runMaybeT $ do
  lift $ info "Creating Vulkan instance..."
  vkInstance <- liftIO createInstance
  lift $ info "Creating surface..."
  surface <- liftIO $ getWindowSurface window vkInstance
  lift $ info "Creating logical device..."
  device <- MaybeT $ createDevice vkInstance surface
  return $ GraphicsEnv device surface vkInstance

cleanup :: (MonadIO m, Logger m) => GraphicsEnv -> m ()
cleanup GraphicsEnv{..} = do
  info "Cleaning up graphics..."
  Vk.destroySurfaceKHR graphicsVkInstance graphicsSurface Nothing
  destroyDevice graphicsDevice
  Vk.destroyInstance graphicsVkInstance Nothing
