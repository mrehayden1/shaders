module Graphics.Shaders.Internal.Instance (
  withInstance
) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString
import Data.ByteString.Char8 as BS8
import Data.List (union)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import Vulkan.Core10.AllocationCallbacks
import Vulkan.Core10.DeviceInitialization as VkApp (
  ApplicationInfo(..))
import qualified Vulkan.Core10.DeviceInitialization as VkInit
import Vulkan.Core10.ExtensionDiscovery as VkExt
import Vulkan.Extensions.VK_EXT_validation_features as VkValidation
import Vulkan.Extensions.VK_KHR_surface as VkSurface
import Vulkan.Core10.Handles as Vk
import Vulkan.Core10.LayerDiscovery as VkLayer
import Vulkan.Core13 as Vk13
import Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class

withInstance :: (MonadAsyncException m, MonadLogger m, MonadResource m)
  => Maybe AllocationCallbacks
  -> m Vk.Instance
withInstance allocator = do
  logLevel <- loggerLevel

  when (logLevel <= LogTrace) $ do
    logTrace "Dumping available instance layers."
    (_, layers) <- VkLayer.enumerateInstanceLayerProperties
    forM_ layers $ \layer -> do
      logTrace . printf "Layer discovered: %s" . show $ layer

    logTrace "Dumping available instance extensions."
    (_, extensions) <- VkExt.enumerateInstanceExtensionProperties Nothing
    forM_ extensions $ \ext -> do
      logTrace . printf "Extension discovered: %s" . show $ ext

    forM_ layers $ \layer -> do
      (_, exts) <- VkExt.enumerateInstanceExtensionProperties . Just
                     . VkLayer.layerName $ layer

      when (V.length exts > 0) $ do
        logTrace . printf "Dumping extensions provided by layer %s."
          . BS8.unpack . VkLayer.layerName $ layer

        forM_ exts $ \ext -> do
          logTrace . printf "Extension discovered: %s" . show $ ext

  let appInfo = Vk.zero {
    VkApp.apiVersion = Vk13.API_VERSION_1_3
  }

  windowInstanceExtensions <- liftIO $
    mapM packCString =<< GLFW.getRequiredInstanceExtensions

  -- Window extensions *should* always contain VK_KHR_surface, but we'll
  -- add it anyway since we'll need it later.
  let extraInstanceExtensions = [
          VkSurface.KHR_SURFACE_EXTENSION_NAME
        ] <> [
          VkValidation.EXT_VALIDATION_FEATURES_EXTENSION_NAME
        | logLevel <= LogTrace
        ]
      requiredExtensions = V.fromList
        . (extraInstanceExtensions `union`) $ windowInstanceExtensions
      requiredLayers = V.fromList [
          "VK_LAYER_KHRONOS_validation" :: String
        | logLevel <= LogTrace
        ]
      instanceInfo = Vk.zero {
        VkInit.applicationInfo = Just appInfo,
        VkInit.enabledExtensionNames = requiredExtensions,
        VkInit.enabledLayerNames = V.fromList [
            "VK_LAYER_KHRONOS_validation"
          | logLevel <= LogTrace
          ]
      }

  logTrace . printf "Requesting extensions: %s" . show $ requiredExtensions
  logTrace . printf "Requesting layers: %s" . show $ requiredLayers

  debug "Creating Vulkan instance..."
  snd <$> allocate
    (do
      VkInit.createInstance instanceInfo allocator
    )
    (flip VkInit.destroyInstance allocator)
