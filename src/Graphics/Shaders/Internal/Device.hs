module Graphics.Shaders.Internal.Device (
  Device(..),
  SwapChain(..),

  withDevice
) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.ByteString.UTF8 as UTF8
import qualified Data.Vector as V
import Data.Word
import Graphics.UI.GLFW as GLFW
import Vulkan.Core10.AllocationCallbacks
import qualified Vulkan.Core10.CommandPool as VkPool
import qualified Vulkan.Core10.Device as VkDevice
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Pass as VkPass hiding (FramebufferCreateInfo(..))
import Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapChain
import Vulkan.Extensions.VK_KHR_surface as VkSurface
import Vulkan.Zero as Vk

import Graphics.Shaders.Exception
import Graphics.Shaders.Internal.Device.Physical
import Graphics.Shaders.Internal.Device.SwapChain
import Graphics.Shaders.Logger.Class

-- A graphics enabled logical device.
data Device = Device {
  deviceCommandPool :: Vk.CommandPool,
  deviceHandle :: Vk.Device,
  deviceMemoryProperties :: VkDevice.PhysicalDeviceMemoryProperties,
  deviceQueueHandle :: Vk.Queue,
  deviceRenderPass :: Vk.RenderPass,
  deviceSwapChain :: SwapChain
} deriving (Show)

requiredDeviceExtensions :: [ByteString]
requiredDeviceExtensions = [VkSwapChain.KHR_SWAPCHAIN_EXTENSION_NAME]

withDevice :: (MonadAsyncException m, MonadLogger m)
  => Vk.Instance
  -> Maybe AllocationCallbacks
  -> GLFW.Window
  -> VkSurface.SurfaceKHR
  -> ResourceT m Device
withDevice vkInstance allocator window surface = do
  debug "Creating logical device..."
  devices <- lift $
    getSuitableDevices vkInstance window surface requiredDeviceExtensions

  when (null devices) $ do
    let msg = "No suitable physical device found"
    info msg
    lift . throw . ShadersInitializationException $ msg

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

  (_, vkDevice) <- allocate
    (VkDevice.createDevice physicalDeviceHandle deviceCreateInfo allocator)
    (\device -> VkDevice.destroyDevice device allocator)

  queue <- VkQueue.getDeviceQueue vkDevice physicalDeviceQueueFamilyIndex 0

  commandPool <- withCommandPool allocator vkDevice
    physicalDeviceQueueFamilyIndex

  let SwapChainSettings{..} = physicalDeviceSwapChainSettings
      surfaceFormat = VkSurface.format swapSettingsSurfaceFormat
  renderPass <- withRenderPass allocator vkDevice surfaceFormat

  swapChain <-
    withSwapChain allocator surface vkDevice renderPass
      physicalDeviceSwapChainSettings

  let device = Device {
    deviceCommandPool = commandPool,
    deviceHandle = vkDevice,
    deviceMemoryProperties = physicalDeviceMemoryProperties,
    deviceQueueHandle = queue,
    deviceRenderPass = renderPass,
    deviceSwapChain = swapChain
  }

  return device

withRenderPass :: (MonadAsyncException m, MonadLogger m)
  => Maybe AllocationCallbacks
  -> Vk.Device
  -> Vk.Format
  -> ResourceT m Vk.RenderPass
withRenderPass allocator device format = do
  debug "Creating render pass."
  let passCreateInfo = Vk.zero {
    VkPass.attachments = V.fromList [
      Vk.zero {
        VkPass.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR,
        VkPass.format = format,
        VkPass.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
        VkPass.loadOp = VkPass.ATTACHMENT_LOAD_OP_CLEAR,
        VkPass.samples = Vk.SAMPLE_COUNT_1_BIT,
        VkPass.stencilLoadOp = VkPass.ATTACHMENT_LOAD_OP_DONT_CARE,
        VkPass.stencilStoreOp = VkPass.ATTACHMENT_STORE_OP_DONT_CARE,
        VkPass.storeOp = VkPass.ATTACHMENT_STORE_OP_STORE
      }
    ],
    VkPass.subpasses = V.fromList [
      Vk.zero {
        VkPass.colorAttachments = V.singleton $ Vk.zero {
          VkPass.attachment = 0,
          VkPass.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        },
        VkPass.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
      }
    ]
  }
  snd <$> allocate
    (VkPass.createRenderPass device passCreateInfo allocator)
    (\p -> VkPass.destroyRenderPass device p allocator)

withCommandPool :: (MonadAsyncException m, MonadLogger m)
  => Maybe AllocationCallbacks
  -> Vk.Device
  -> Word32
  -> ResourceT m Vk.CommandPool
withCommandPool allocator device queueFamilyIndex = do
  let poolCreateInfo = Vk.zero {
    VkPool.flags = VkPool.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
    VkPool.queueFamilyIndex = queueFamilyIndex
  }

  debug "Creating command pool."
  snd <$> allocate
    (VkPool.createCommandPool device poolCreateInfo allocator)
    (\pool -> VkPool.destroyCommandPool device pool allocator)
