module Graphics.Instance (
  createInstance
) where

import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

createInstance :: IO Vk.Instance
createInstance = do
  let appInfo = Vk.ApplicationInfo Nothing 0 Nothing 0 Vk.API_VERSION_1_3

  --availableExtensions <- Vk.enumerateInstanceExtensionProperties Nothing

  requiredExtensions <- liftIO $
    fmap V.fromList . mapM packCString =<< GLFW.getRequiredInstanceExtensions

  let instanceInfo = Vk.InstanceCreateInfo () Vk.zero (Just appInfo) V.empty
                       requiredExtensions

  liftIO $ Vk.createInstance instanceInfo Nothing
