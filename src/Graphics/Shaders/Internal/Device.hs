module Graphics.Shaders.Internal.Device (
  HasVulkanDevice(..),

  DeviceReaderT(..),
  runDeviceReaderT,

  Device(..),
  createDevice,

  awaitIdle
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.UTF8 as UTF8
import qualified Data.Vector as V
import Data.Word
import Vulkan.Core10.AllocationCallbacks
import qualified Vulkan.Core10.CommandPool as VkPool
import qualified Vulkan.Core10.Device as VkDevice
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Pass as VkPass hiding (
  FramebufferCreateInfo(..))
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapchain
import Vulkan.Extensions.VK_KHR_surface as VkSurface
import Vulkan.Zero as Vk

import Graphics.Shaders.Exception
import Graphics.Shaders.Internal.Device.Physical
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Window
import Graphics.Shaders.Logger.Class

class Monad m => HasVulkanDevice m where
  getCommandPool :: m Vk.CommandPool
  default getCommandPool :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m Vk.CommandPool
  getCommandPool = lift getCommandPool

  getDevice :: m Vk.Device
  default getDevice :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m Vk.Device
  getDevice = lift getDevice

  getMemoryProperties :: m VkDevice.PhysicalDeviceMemoryProperties
  default getMemoryProperties :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m VkDevice.PhysicalDeviceMemoryProperties
  getMemoryProperties = lift getMemoryProperties

  getQueue :: m Vk.Queue
  default getQueue :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m Vk.Queue
  getQueue = lift getQueue

  getRenderPass :: m Vk.RenderPass
  default getRenderPass :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m Vk.RenderPass
  getRenderPass = lift getRenderPass

  getSwapchainSettings :: m SwapchainSettings
  default getSwapchainSettings :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m SwapchainSettings
  getSwapchainSettings = lift getSwapchainSettings

instance HasVulkanDevice m => HasVulkanDevice (VulkanReaderT m)
instance HasVulkanDevice m => HasVulkanDevice (Codensity m)


newtype DeviceReaderT m a = DeviceReaderT {
  unDeviceReaderT :: ReaderT Device m a
} deriving (Functor, Applicative, Monad, MonadIO, MonadException,
    MonadAsyncException, MonadTrans, MonadFix, MonadLogger, MonadResource,
    HasVulkan)

-- A graphics enabled logical device.
data Device = Device {
  deviceCommandPool :: Vk.CommandPool,
  deviceHandle :: Vk.Device,
  deviceMemoryProperties :: VkDevice.PhysicalDeviceMemoryProperties,
  deviceQueueHandle :: Vk.Queue,
  deviceRenderPass :: Vk.RenderPass,
  deviceSwapchainSettings :: SwapchainSettings
} deriving (Show)


instance Monad m => HasVulkanDevice (DeviceReaderT m) where
  getCommandPool = DeviceReaderT . asks $ deviceCommandPool
  getDevice = DeviceReaderT . asks $ deviceHandle
  getMemoryProperties =
    DeviceReaderT . asks $ deviceMemoryProperties
  getQueue = DeviceReaderT . asks $ deviceQueueHandle
  getRenderPass = DeviceReaderT . asks $ deviceRenderPass
  getSwapchainSettings = DeviceReaderT . asks $ deviceSwapchainSettings

runDeviceReaderT :: DeviceReaderT m a -> Device -> m a
runDeviceReaderT (DeviceReaderT m) = runReaderT m


requiredDeviceExtensions :: [ByteString]
requiredDeviceExtensions = [VkSwapchain.KHR_SWAPCHAIN_EXTENSION_NAME]

createDevice :: (MonadAsyncException m, MonadLogger m, MonadResource m,
    HasWindow m, HasVulkan m)
  => VkSurface.SurfaceKHR
  -> m Device
createDevice surface = do
  vkInstance <- getVulkanInstance
  allocator <- getVulkanAllocator

  debug "Creating logical device..."
  devices <- getSuitableDevices vkInstance surface requiredDeviceExtensions

  when (null devices) $ do
    let msg = "No suitable physical device found"
    info msg
    throw . ShadersInitializationException $ msg

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

  commandPool <- createCommandPool allocator vkDevice
    physicalDeviceQueueFamilyIndex

  let swapSettings = physicalDeviceSwapchainSettings

  renderPass <- createRenderPass allocator vkDevice swapSettings

  let device = Device {
    deviceCommandPool = commandPool,
    deviceHandle = vkDevice,
    deviceMemoryProperties = physicalDeviceMemoryProperties,
    deviceQueueHandle = queue,
    deviceRenderPass = renderPass,
    deviceSwapchainSettings = swapSettings
  }

  return device

createRenderPass :: (MonadLogger m, MonadResource m)
  => Maybe AllocationCallbacks
  -> Vk.Device
  -> SwapchainSettings
  -> m Vk.RenderPass
createRenderPass allocator device SwapchainSettings{..} = do
  let surfaceFormat = VkSurface.format swapSettingsSurfaceFormat

  debug "Creating render pass."
  let passCreateInfo = Vk.zero {
    VkPass.attachments = V.fromList [
      Vk.zero {
        VkPass.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR,
        VkPass.format = surfaceFormat,
        VkPass.initialLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR,
        VkPass.loadOp = VkPass.ATTACHMENT_LOAD_OP_LOAD,
        VkPass.samples = Vk.SAMPLE_COUNT_1_BIT,
        VkPass.stencilLoadOp = VkPass.ATTACHMENT_LOAD_OP_DONT_CARE,
        VkPass.stencilStoreOp = VkPass.ATTACHMENT_STORE_OP_DONT_CARE,
        VkPass.storeOp = VkPass.ATTACHMENT_STORE_OP_STORE
      },
      Vk.zero {
        VkPass.finalLayout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
        VkPass.format = deviceDepthImageFormat,
        VkPass.initialLayout =
          Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
        VkPass.loadOp = VkPass.ATTACHMENT_LOAD_OP_LOAD,
        VkPass.samples = Vk.SAMPLE_COUNT_1_BIT,
        VkPass.stencilLoadOp = VkPass.ATTACHMENT_LOAD_OP_DONT_CARE,
        VkPass.stencilStoreOp = VkPass.ATTACHMENT_STORE_OP_DONT_CARE,
        VkPass.storeOp = VkPass.ATTACHMENT_STORE_OP_STORE
      }
    ],
    VkPass.subpasses = V.fromList [
      Vk.zero {
        VkPass.colorAttachments = V.fromList [
          Vk.zero {
            VkPass.attachment = 0,
            VkPass.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          }
        ],
        VkPass.depthStencilAttachment = Just $ Vk.zero {
          VkPass.attachment = 1,
          VkPass.layout = Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
        },
        VkPass.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
      }
    ]
  }
  snd <$> allocate
    (VkPass.createRenderPass device passCreateInfo allocator)
    (\p -> VkPass.destroyRenderPass device p allocator)

createCommandPool :: (MonadLogger m, MonadResource m)
  => Maybe AllocationCallbacks
  -> Vk.Device
  -> Word32
  -> m Vk.CommandPool
createCommandPool allocator device queueFamilyIndex = do
  let poolCreateInfo = Vk.zero {
    VkPool.flags = VkPool.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
    VkPool.queueFamilyIndex = queueFamilyIndex
  }

  debug "Creating command pool."
  snd <$> allocate
    (VkPool.createCommandPool device poolCreateInfo allocator)
    (\pool -> VkPool.destroyCommandPool device pool allocator)


awaitIdle :: (MonadIO m, HasVulkanDevice m) => m ()
awaitIdle = do
  VkQueue.deviceWaitIdle =<< getDevice
