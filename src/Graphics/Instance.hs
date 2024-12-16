module Graphics.Instance (
  createInstance
) where

import Control.Monad.IO.Class
import Data.ByteString
import Data.List (union)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

createInstance :: IO Vk.Instance
createInstance = do
  let appInfo = Vk.ApplicationInfo Nothing 0 Nothing 0 Vk.API_VERSION_1_3

  windowInstanceExtensions <- mapM packCString
    =<< GLFW.getRequiredInstanceExtensions

  -- Window extensions *should* always contain VK_KHR_surface, but we'll add it
  -- anyway just in case, because our code needs it later.
  let extraInstanceExtensions = [Vk.KHR_SURFACE_EXTENSION_NAME]

  let requiredExtensions = V.fromList
        . (extraInstanceExtensions `union`) $ windowInstanceExtensions

  let instanceInfo = Vk.InstanceCreateInfo () Vk.zero (Just appInfo) V.empty
                       requiredExtensions

  liftIO $ Vk.createInstance instanceInfo Nothing
