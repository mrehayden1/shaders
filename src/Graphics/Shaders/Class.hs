module Graphics.Shaders.Class (
  HasVulkan(..),
  HasVulkanDevice(..),
  HasSwapchain(..),

  awaitIdle,

  Frame(..),
  SyncObjects(..)
) where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Vulkan.Core10.AllocationCallbacks
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import Vulkan.Core10.FundamentalTypes (Extent2D)
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapchain

import Graphics.Shaders.Internal.Frames

class Monad m => HasVulkan m where
  getVulkanAllocator :: m (Maybe AllocationCallbacks)
  default getVulkanAllocator :: (t m' ~ m, MonadTrans t, HasVulkan m')
    => m (Maybe AllocationCallbacks)
  getVulkanAllocator = lift getVulkanAllocator
  getVulkanInstance :: m Vk.Instance
  default getVulkanInstance :: (t m' ~ m, MonadTrans t, HasVulkan m')
    => m Vk.Instance
  getVulkanInstance = lift getVulkanInstance

class Monad m => HasVulkanDevice m where
  getCommandPool :: m Vk.CommandPool
  getDevice :: m Vk.Device
  getDeviceMemoryProperties :: m VkDevice.PhysicalDeviceMemoryProperties
  getDeviceQueue :: m Vk.Queue

class Monad m => HasSwapchain m where
  getCurrentFrame :: m (Int, Frame, Vk.Framebuffer)
  getNumFrames :: m Int
  getRenderPass :: m Vk.RenderPass
  getSwapChain :: m VkSwapchain.SwapchainKHR
  getSwapChainExtent :: m Extent2D
  swap :: m ()

awaitIdle :: (MonadIO m, HasVulkanDevice m) => m ()
awaitIdle = do
  VkQueue.deviceWaitIdle =<< getDevice
