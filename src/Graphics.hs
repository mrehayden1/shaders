module Graphics (
  initialise,
  cleanup
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk

import Graphics.Device
import Graphics.Instance
import Graphics.Window

data GraphicsEnv = GraphicsEnv {
    graphicsDevice :: Device,
    graphicsSurface :: Vk.SurfaceKHR,
    graphicsVkInstance :: Vk.Instance
  }

initialise :: GLFW.Window -> IO (Maybe GraphicsEnv)
initialise window = runMaybeT $ do
  vkInstance <- liftIO createInstance
  surface <- liftIO $ getWindowSurface window vkInstance
  device <- MaybeT $ createDevice vkInstance surface
  return $ GraphicsEnv device surface vkInstance

cleanup :: GraphicsEnv -> IO ()
cleanup GraphicsEnv{..} = do
  putStrLn "Cleaning up graphics."
  Vk.destroySurfaceKHR graphicsVkInstance graphicsSurface Nothing
  destroyDevice graphicsDevice
  Vk.destroyInstance graphicsVkInstance Nothing
