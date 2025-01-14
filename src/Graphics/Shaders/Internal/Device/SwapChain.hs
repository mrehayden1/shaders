module Graphics.Shaders.Internal.Device.SwapChain (
  SwapChain(..),

  withSwapChain
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.Core10.Enums as Vk
import Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import Vulkan.Core10.Handles as Vk
import Vulkan.Core10.ImageView as VkImageView
import qualified Vulkan.Core10.Pass as VkFramebuffer hiding (RenderPassCreateInfo(..))
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapChain
import Vulkan.Extensions.VK_KHR_surface as VkSurface
import Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Device.Physical
import Graphics.Shaders.Logger.Class

-- Information about the swap chain.
data SwapChain = SwapChain {
  swapChainHandle :: VkSwapChain.SwapchainKHR,
  -- Swap chain image size and format
  swapChainExtent :: VkExtent2D.Extent2D,
  swapChainFormat :: VkSurface.SurfaceFormatKHR,
  -- Swap chain image framebuffers
  swapChainFramebuffers :: Vector Vk.Framebuffer
} deriving (Show)

withSwapChain :: (MonadAsyncException m, MonadLogger m)
  => VkSurface.SurfaceKHR
  -> Vk.Device
  -> Vk.RenderPass
  -> SwapChainSettings
  -> Codensity m SwapChain
withSwapChain surface device renderPass SwapChainSettings{..} = do
  debug "Creating swap chain."
  let swapChainImageFormat = VkSurface.format swapSettingsSurfaceFormat

  let swapChainCreateInfo = Vk.zero {
    VkSwapChain.flags = Vk.zero,
    VkSwapChain.surface = surface,
    VkSwapChain.minImageCount = swapSettingsImageCount,
    VkSwapChain.imageFormat = swapChainImageFormat,
    VkSwapChain.imageColorSpace =
      VkSurface.colorSpace swapSettingsSurfaceFormat,
    VkSwapChain.imageExtent = swapSettingsExtent,
    VkSwapChain.imageArrayLayers = 1,
    VkSwapChain.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
    VkSwapChain.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE,
    VkSwapChain.preTransform = swapSettingsTransform,
    VkSwapChain.compositeAlpha = VkSwapChain.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
    VkSwapChain.presentMode = swapSettingsPresentMode,
    VkSwapChain.clipped = True
  }

  swapChain <- Codensity $
    bracket
      (VkSwapChain.createSwapchainKHR device swapChainCreateInfo Nothing)
      (\swapChain -> do
        debug "Destroying swap chain."
        VkSwapChain.destroySwapchainKHR device swapChain Nothing
      )

  -- Create swap chain images and image views.
  debug "Retrieving swap chain images."
  (result, swapImages) <- VkSwapChain.getSwapchainImagesKHR
                            device
                            swapChain
  when (result == Vk.INCOMPLETE) $
    warn "Vulkan API returned incomplete swap chain images list."

  debug "Creating swap chain image views."

  swapImageViews <- Codensity $ bracket
    (forM swapImages $ \image -> do
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
      VkImageView.createImageView device imageViewCreateInfo Nothing
    )
    (\imageViews -> do
      debug "Destroying swap chain image views."
      forM_ imageViews $ \i ->
        VkImageView.destroyImageView device i Nothing
    )

  framebuffers <-
    withFramebuffers device renderPass swapImageViews swapSettingsExtent

  return $ SwapChain {
    swapChainHandle = swapChain,
    swapChainExtent = swapSettingsExtent,
    swapChainFormat = swapSettingsSurfaceFormat,
    swapChainFramebuffers = framebuffers
  }

withFramebuffers :: (MonadAsyncException m, MonadLogger m)
  => Vk.Device
  -> Vk.RenderPass
  -> Vector Vk.ImageView
  -> Extent2D
  -> Codensity m (Vector Vk.Framebuffer)
withFramebuffers device renderPass imageViews imageExtent = do
  debug "Creating framebuffers."
  Codensity $ bracket
    (forM imageViews $ \imageView -> do
        let framebufferCreateInfo = Vk.zero {
          VkFramebuffer.attachments = V.singleton imageView,
          VkFramebuffer.height = VkExtent2D.height imageExtent,
          VkFramebuffer.layers = 1,
          VkFramebuffer.renderPass = renderPass,
          VkFramebuffer.width = VkExtent2D.width imageExtent
        }
        VkFramebuffer.createFramebuffer device framebufferCreateInfo Nothing
    )
    (\framebuffers -> do
       debug "Destroying framebuffers."
       forM_ framebuffers $ \framebuffer ->
         VkFramebuffer.destroyFramebuffer device framebuffer Nothing
    )
