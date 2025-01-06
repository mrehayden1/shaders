module Graphics.Shaders.Initialization.Instance (
  withInstance
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.IO.Class
import Data.ByteString
import Data.List (union)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class

withInstance :: (MonadAsyncException m, MonadLogger m)
  => Codensity m Vk.Instance
withInstance = do
  Codensity $ bracket createInstance' destroyInstance
 where
  createInstance' = do
    let appInfo = Vk.ApplicationInfo Nothing 0 Nothing 0 Vk.API_VERSION_1_3

    windowInstanceExtensions <- liftIO $
      mapM packCString =<< GLFW.getRequiredInstanceExtensions

    -- Window extensions *should* always contain VK_KHR_surface, but we'll add
    -- it anyway since we'll need it later.
    let extraInstanceExtensions = [Vk.KHR_SURFACE_EXTENSION_NAME]
        requiredExtensions = V.fromList
          . (extraInstanceExtensions `union`) $ windowInstanceExtensions
        instanceInfo = Vk.InstanceCreateInfo () Vk.zero (Just appInfo) V.empty
                         requiredExtensions

    debug "Creating Vulkan instance..."
    Vk.createInstance instanceInfo Nothing

  destroyInstance vkInstance = do
    debug "Destroying Vulkan instance."
    Vk.destroyInstance vkInstance Nothing
