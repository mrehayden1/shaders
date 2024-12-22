module Graphics.Shaders.Device.SwapChain (
  SwapChain(..),
  createSwapChain
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.IO.Class
import Data.Vector (Vector)
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.ImageView as VkImageView
import qualified Vulkan.Extensions.VK_KHR_swapchain as VkSwapChain
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Device.Physical

-- Information about the swap chain.
data SwapChain = SwapChain {
  swapChainHandle :: VkSwapChain.SwapchainKHR,
  swapChainExtent :: VkExtent2D.Extent2D,
  swapChainFormat :: VkSurface.SurfaceFormatKHR,
  swapChainImages :: Vector Vk.Image,
  swapChainImageViews :: Vector VkImageView.ImageView
} deriving (Show)

createSwapChain :: (MonadAsyncException m, MonadLogger m)
  => VkSurface.SurfaceKHR
  -> Vk.Device
  -> SwapChainSettings
  -> Codensity m SwapChain
createSwapChain surface vkDevice physicalDeviceSwapChainSettings = do
  debug "Creating swap chain."
  let swapChainImageFormat = VkSurface.format
        . swapSettingsSurfaceFormat $ physicalDeviceSwapChainSettings

  let swapChainCreateInfo = Vk.zero {
          VkSwapChain.flags = Vk.zero,
          VkSwapChain.surface = surface,
          VkSwapChain.minImageCount =
            swapSettingsImageCount physicalDeviceSwapChainSettings,
          VkSwapChain.imageFormat = swapChainImageFormat,
          VkSwapChain.imageColorSpace = VkSurface.colorSpace
            . swapSettingsSurfaceFormat $ physicalDeviceSwapChainSettings,
          VkSwapChain.imageExtent = swapSettingsExtent
            physicalDeviceSwapChainSettings,
          VkSwapChain.imageArrayLayers = 1,
          VkSwapChain.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
          VkSwapChain.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE,
          VkSwapChain.preTransform =
            swapSettingsTransform physicalDeviceSwapChainSettings,
          VkSwapChain.compositeAlpha =
            VkSwapChain.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
          VkSwapChain.presentMode =
            swapSettingsPresentMode physicalDeviceSwapChainSettings,
          VkSwapChain.clipped = True
        }

  let createSwapChain' =
        VkSwapChain.createSwapchainKHR vkDevice swapChainCreateInfo Nothing

  vkSwapChain <- Codensity $
    bracket createSwapChain' (destroySwapChain vkDevice)

  -- Create swap chain images and image views.
  debug "Retrieving swap chain images."
  (result, swapImages) <- VkSwapChain.getSwapchainImagesKHR
                            vkDevice
                            vkSwapChain
  when (result == Vk.INCOMPLETE) $
    warn "Vulkan API returned incomplete swap chain images list."

  debug "Creating swap chain image views."

  let createImageViews =
        forM swapImages $ \image -> do
        let imageViewCreateInfo = Vk.zero {
                VkImageView.format = swapChainImageFormat,
                VkImageView.image = image,
                VkImageView.subresourceRange = Vk.zero {
                    VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                    VkImageView.layerCount = 1,
                    VkImageView.levelCount = 1
                  },
                VkImageView.viewType = VkImageView.IMAGE_VIEW_TYPE_2D
              }
        Vk.createImageView vkDevice imageViewCreateInfo Nothing

  swapImageViews <- Codensity $ bracket createImageViews (destroyImageViews vkDevice)

  return $ SwapChain {
      swapChainHandle = vkSwapChain,
      swapChainExtent = swapSettingsExtent physicalDeviceSwapChainSettings,
      swapChainFormat =
        swapSettingsSurfaceFormat physicalDeviceSwapChainSettings,
      swapChainImages = swapImages,
      swapChainImageViews = swapImageViews
    }

destroyImageViews :: (MonadIO m, MonadLogger m)
  => Vk.Device
  -> Vector Vk.ImageView
  -> m ()
destroyImageViews vkDevice imageViews = do
  debug "Destroying swap chain image views."
  forM_ imageViews $ \i ->
    VkImageView.destroyImageView vkDevice i Nothing

destroySwapChain :: (MonadIO m, MonadLogger m)
  => Vk.Device
  -> VkSwapChain.SwapchainKHR
  -> m ()
destroySwapChain vkDevice swapChain = do
  debug "Destroying swap chain."
  VkSwapChain.destroySwapchainKHR vkDevice swapChain Nothing
