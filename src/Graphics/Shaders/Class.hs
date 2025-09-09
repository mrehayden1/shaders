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
  default getCommandPool :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m Vk.CommandPool
  getCommandPool = lift getCommandPool

  getDevice :: m Vk.Device
  default getDevice :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m Vk.Device
  getDevice = lift getDevice

  getDeviceMemoryProperties :: m VkDevice.PhysicalDeviceMemoryProperties
  default getDeviceMemoryProperties :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m VkDevice.PhysicalDeviceMemoryProperties
  getDeviceMemoryProperties = lift getDeviceMemoryProperties

  getDeviceQueue :: m Vk.Queue
  default getDeviceQueue :: (t m' ~ m, MonadTrans t, HasVulkanDevice m')
    => m Vk.Queue
  getDeviceQueue = lift getDeviceQueue


class Monad m => HasSwapchain m where
  getCurrentFrame :: m (Int, Frame)
  default getCurrentFrame :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m (Int, Frame)
  getCurrentFrame = lift getCurrentFrame

  getCurrentSwapImage :: m (Vk.Image, Vk.Framebuffer)
  default getCurrentSwapImage :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m (Vk.Image, Vk.Framebuffer)
  getCurrentSwapImage = lift getCurrentSwapImage

  getNumFrames :: m Int
  default getNumFrames :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m Int
  getNumFrames = lift getNumFrames

  -- TODO Isn't this actually a property of the device?
  getRenderPass :: m Vk.RenderPass
  default getRenderPass :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m Vk.RenderPass
  getRenderPass = lift getRenderPass

  getSwapChain :: m VkSwapchain.SwapchainKHR
  default getSwapChain :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m VkSwapchain.SwapchainKHR
  getSwapChain = lift getSwapChain

  getSwapChainExtent :: m Extent2D
  default getSwapChainExtent :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m Extent2D
  getSwapChainExtent = lift getSwapChainExtent

  swap :: m ()
  default swap :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m ()
  swap = lift swap


awaitIdle :: (MonadIO m, HasVulkanDevice m) => m ()
awaitIdle = do
  VkQueue.deviceWaitIdle =<< getDevice
