module Graphics (
  initialise,
  cleanup
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

import Graphics.Device
import Graphics.Window

data GraphicsEnv = GraphicsEnv {
    graphicsDevice :: Device,
    graphicsSurface :: Vk.SurfaceKHR,
    graphicsVkInstance :: Vk.Instance
  }

initialise :: GLFW.Window -> IO (Maybe GraphicsEnv)
initialise window = runMaybeT $ do
  -- Create Vulkan instance
  let appInfo = Vk.ApplicationInfo Nothing 0 Nothing 0 Vk.API_VERSION_1_3
  extensions <- liftIO $
    fmap V.fromList . mapM packCString =<< GLFW.getRequiredInstanceExtensions
  let instanceInfo = Vk.InstanceCreateInfo () Vk.zero (Just appInfo) V.empty
                       extensions
  vkInstance <- liftIO $ Vk.createInstance instanceInfo Nothing

  surface <- liftIO $ getWindowSurface window vkInstance

  -- Create logical device
  device <- MaybeT $ createDevice vkInstance

  return $ GraphicsEnv device surface vkInstance

cleanup :: GraphicsEnv -> IO ()
cleanup GraphicsEnv{..} = do
  putStrLn "Cleaning up graphics."
  Vk.destroySurfaceKHR graphicsVkInstance graphicsSurface Nothing
  destroyDevice graphicsDevice
  Vk.destroyInstance graphicsVkInstance Nothing
