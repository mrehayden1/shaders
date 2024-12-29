module Graphics.Shaders.Device (
  Device(..),
  SwapChain(..),

  createDevice
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.ByteString.UTF8 as UTF8
import qualified Data.Vector as V
import Data.Word
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Core10.Device as VkDevice
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends
import qualified Vulkan.Extensions.VK_KHR_swapchain as VkSwapChain
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Device.Physical
import Graphics.Shaders.Device.SwapChain

-- A graphics enabled logical device.
data Device = Device {
  deviceHandle :: Vk.Device,
  deviceMemoryProperties :: VkDevice.PhysicalDeviceMemoryProperties,
  deviceQueueHandle :: Vk.Queue,
  deviceSwapChain :: SwapChain
} deriving (Show)

requiredDeviceExtensions :: [ByteString]
requiredDeviceExtensions = [VkSwapChain.KHR_SWAPCHAIN_EXTENSION_NAME]

createDevice :: (MonadAsyncException m, MonadLogger m)
  => Vk.Instance
  -> GLFW.Window
  -> VkSurface.SurfaceKHR
  -> Codensity m (Maybe (Device, Word32))
createDevice vkInstance window surface = runMaybeT $ do
  debug "Creating logical device..."
  devices <- lift $
    getSuitableDevices vkInstance window surface requiredDeviceExtensions

  when (null devices) $ do
    let msg = "No suitable physical device found"
    info msg
    fail msg

  let PhysicalDevice{..} = head devices
      deviceName = VkDevice.deviceName physicalDeviceProperties
  info $ "Chosen physical device: " <> UTF8.toString deviceName

  -- Create the logical device and queue.
  debug "Creating logical device."
  let queueCreateInfo = pure . SomeStruct $
        VkDevice.DeviceQueueCreateInfo
          ()
          Vk.zero
          physicalDeviceQueueFamilyIndex
          (V.singleton 1)

      deviceCreateInfo =
        VkDevice.DeviceCreateInfo
          ()
          Vk.zero
          queueCreateInfo
          V.empty
          (V.fromList requiredDeviceExtensions)
          Nothing

  let createDevice' = VkDevice.createDevice physicalDeviceHandle
                        deviceCreateInfo Nothing

  vkDevice <- lift $ Codensity $ bracket createDevice' destroyDevice
  queue <- VkQueue.getDeviceQueue vkDevice physicalDeviceQueueFamilyIndex 0

  swapChain <- lift $
    createSwapChain surface vkDevice physicalDeviceSwapChainSettings

  let device = Device {
          deviceHandle = vkDevice,
          deviceMemoryProperties = physicalDeviceMemoryProperties,
          deviceQueueHandle = queue,
          deviceSwapChain = swapChain
        }

  return (device, physicalDeviceQueueFamilyIndex)

destroyDevice :: (MonadAsyncException m, MonadLogger m) => Vk.Device -> m ()
destroyDevice device = do
  debug "Destroying logical device."
  VkDevice.destroyDevice device Nothing

