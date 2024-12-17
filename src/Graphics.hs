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

initialise :: (MonadIO m, MonadLogger m) => GLFW.Window -> m (Maybe GraphicsEnv)
initialise window = runMaybeT $ do
  lift $ debug "Creating Vulkan instance..."
  vkInstance <- liftIO createInstance
  lift $ debug "Creating surface..."
  surface <- liftIO $ getWindowSurface window vkInstance
  lift $ debug "Creating logical device..."
  device <- MaybeT $ createDevice vkInstance window surface
  return $ GraphicsEnv device surface vkInstance

cleanup :: (MonadIO m, MonadLogger m) => GraphicsEnv -> m ()
cleanup GraphicsEnv{..} = do
  info "Cleaning up graphics..."
  debug "Destroying surface."
  Vk.destroySurfaceKHR graphicsVkInstance graphicsSurface Nothing
  debug "Destroying logical device."
  destroyDevice graphicsDevice
  debug "Destroying Vulkan instance."
  Vk.destroyInstance graphicsVkInstance Nothing
