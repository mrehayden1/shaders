module Graphics.Shaders.Internal.Swapchain (
  HasSwapchain(..),

  SwapchainStateT(..),
  runSwapchainStateT,

  Swapchain(..),
  Frame(..),
  SyncObjects(..),

  createSwapchain
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Vulkan.Core10.Enums as Vk
import Vulkan.Core10.Fence as VkFence
import Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import Vulkan.Core10.FundamentalTypes as VkExtent3D (Extent3D(..))
import Vulkan.Core10.Handles as Vk
import Vulkan.Core10.Image as VkImage
import Vulkan.Core10.ImageView as VkImageView
import qualified Vulkan.Core10.Pass as VkFramebuffer hiding (
  RenderPassCreateInfo(..))
import Vulkan.Extensions.VK_KHR_surface as VkSurface
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapchain
import Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Device.Physical
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Frames
import Graphics.Shaders.Logger.Class


class Monad m => HasSwapchain m where
  getCurrentFrame :: m (Int, Frame)
  default getCurrentFrame :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m (Int, Frame)
  getCurrentFrame = lift getCurrentFrame

  getCurrentSwapImage :: m ((Vk.Image, Vk.Image), Vk.Framebuffer)
  default getCurrentSwapImage :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m ((Vk.Image, Vk.Image), Vk.Framebuffer)
  getCurrentSwapImage = lift getCurrentSwapImage

  getNumFrames :: m Int
  default getNumFrames :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m Int
  getNumFrames = lift getNumFrames

  getSwapchain :: m VkSwapchain.SwapchainKHR
  default getSwapchain :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m VkSwapchain.SwapchainKHR
  getSwapchain = lift getSwapchain

  getSwapchainExtent :: m Extent2D
  default getSwapchainExtent :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m Extent2D
  getSwapchainExtent = lift getSwapchainExtent

  swap :: m ()
  default swap :: (t m' ~ m, MonadTrans t, HasSwapchain m')
    => m ()
  swap = lift swap

instance HasSwapchain m => HasSwapchain (Codensity m)


newtype SwapchainStateT m a = SwapchainStateT {
  unSwapchainStateT :: StateT SwapchainState m a
} deriving (Functor, Applicative, Monad, MonadIO, MonadException,
    MonadAsyncException, MonadTrans, MonadFix, MonadLogger, MonadResource)

instance HasVulkan m => HasVulkan (SwapchainStateT m)
instance HasVulkanDevice m => HasVulkanDevice (SwapchainStateT m)


data SwapchainState = SwapchainState {
  swapchainStateSwapchain :: Swapchain,
  swapchainStateFrames :: Vector Frame,
  swapchainStateCurrentFrameIndex :: Int,
  swapchainStateCurrentImageIndex :: Word32
}

data Swapchain = Swapchain {
  swapchainHandle :: VkSwapchain.SwapchainKHR,
  -- Image size and format
  swapchainExtent :: VkExtent2D.Extent2D,
  swapchainFormat :: VkSurface.SurfaceFormatKHR,
  -- Image framebuffers
  swapchainFramebuffers :: Vector Vk.Framebuffer,
  -- Images, pairs of color and depth images.
  swapchainImages :: Vector (Image, Image)
}


runSwapchainStateT :: (MonadAsyncException m, MonadLogger m, MonadResource m,
       HasVulkan m, HasVulkanDevice m)
  => VkSurface.SurfaceKHR
  -> SwapchainStateT m a
  -> m a
runSwapchainStateT surface (SwapchainStateT m) = do
  device <- getDevice
  frames <- createFrames

  swapchain@Swapchain{..} <- createSwapchain surface

  -- Get the first swap chain image
  (_, nextImageIndex) <- VkSwapchain.acquireNextImageKHR device
    swapchainHandle maxBound Vk.zero Vk.zero

  let swapchainState = SwapchainState {
        swapchainStateSwapchain = swapchain,
        swapchainStateFrames = frames,
        swapchainStateCurrentFrameIndex = 0,
        swapchainStateCurrentImageIndex = nextImageIndex
      }

  evalStateT m swapchainState


instance (MonadIO m, MonadLogger m, HasVulkanDevice m)
    => HasSwapchain (SwapchainStateT m) where
  getCurrentFrame = do
    frames <- SwapchainStateT $ gets swapchainStateFrames
    frameNumber <- SwapchainStateT $ gets swapchainStateCurrentFrameIndex
    return (frameNumber, frames V.! frameNumber)

  getCurrentSwapImage = do
    Swapchain{..} <- SwapchainStateT . gets $ swapchainStateSwapchain
    swapchainImageIndex <- SwapchainStateT . gets
      $ fromIntegral . swapchainStateCurrentImageIndex
    let images = swapchainImages V.! swapchainImageIndex
        framebuffer =
          swapchainFramebuffers V.! swapchainImageIndex
    return (images, framebuffer)

  getNumFrames = SwapchainStateT . gets $ V.length . swapchainStateFrames

  getSwapchain = SwapchainStateT . gets $
    swapchainHandle . swapchainStateSwapchain

  getSwapchainExtent = SwapchainStateT . gets $
    swapchainExtent . swapchainStateSwapchain

  swap = do
    device <- getDevice
    queue <- getQueue
    swapchain <- getSwapchain
    numFrames <- getNumFrames

    (_, Frame{..}) <- getCurrentFrame
    let SyncObjects{..} = frameSyncObjects

    currentImageIndex <- SwapchainStateT $
      gets swapchainStateCurrentImageIndex

    logTrace "Waiting for GPU to render current frame"
    -- Wait for the GPU to finish rendering the last frame.
    -- Ignore TIMEOUT since we're waiting so long anyway.
    let fences = V.singleton syncInFlightFence
    _ <- VkFence.waitForFences device fences True maxBound
    VkFence.resetFences device fences

    logTrace "Presenting image"
    -- Queue the presentation of the current swap chain image
    let presentInfo = Vk.zero {
      VkSwapchain.imageIndices = V.singleton currentImageIndex,
      VkSwapchain.swapchains = V.singleton swapchain,
      VkSwapchain.waitSemaphores = V.singleton syncRenderFinishedSemaphore
    }
    _ <- VkSwapchain.queuePresentKHR queue presentInfo

    -- Acquire the next swap chain image
    -- Ignore TIMEOUT and NOT_READY since we're not using a fence or semaphore
    -- and ignore SUBOPTIMAL_KHR.
    logTrace "Acquiring next image from swapchain"
    (_, nextImageIndex) <- VkSwapchain.acquireNextImageKHR device swapchain
                             maxBound Vk.zero Vk.zero

    -- Increment the frame
    SwapchainStateT $ modify (\s@SwapchainState{..} -> s {
        swapchainStateCurrentFrameIndex =
          (`mod` numFrames) . (+ 1) $ swapchainStateCurrentFrameIndex,
        swapchainStateCurrentImageIndex = nextImageIndex
      })


createSwapchain :: (MonadAsyncException m, MonadLogger m, MonadResource m,
       HasVulkan m, HasVulkanDevice m)
  => VkSurface.SurfaceKHR
  -> m Swapchain
createSwapchain surface = do
  allocator <- getVulkanAllocator
  device <- getDevice
  SwapchainSettings{..} <- getSwapchainSettings

  debug "Creating swap chain."
  let swapchainImageFormat = VkSurface.format swapSettingsSurfaceFormat

  let swapchainCreateInfo = Vk.zero {
    VkSwapchain.flags = Vk.zero,
    VkSwapchain.surface = surface,
    VkSwapchain.minImageCount = swapSettingsImageCount,
    VkSwapchain.imageFormat = swapchainImageFormat,
    VkSwapchain.imageColorSpace =
      VkSurface.colorSpace swapSettingsSurfaceFormat,
    VkSwapchain.imageExtent = swapSettingsExtent,
    VkSwapchain.imageArrayLayers = 1,
    VkSwapchain.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      .|. Vk.IMAGE_USAGE_TRANSFER_DST_BIT,
    VkSwapchain.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE,
    VkSwapchain.preTransform = swapSettingsTransform,
    VkSwapchain.compositeAlpha = VkSwapchain.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
    VkSwapchain.presentMode = swapSettingsPresentMode,
    VkSwapchain.clipped = True
  }

  (_, swapchain) <- allocate
    (VkSwapchain.createSwapchainKHR device swapchainCreateInfo allocator)
    (\swapchain -> do
      VkSwapchain.destroySwapchainKHR device swapchain allocator
    )

  -- Get swap chain images and create image views.
  debug "Retrieving swap chain images."
  (result, swapImages) <- VkSwapchain.getSwapchainImagesKHR device swapchain
  when (result == Vk.INCOMPLETE) $
    warn "Vulkan API returned incomplete swap chain images list."

  debug "Creating swap chain image views."
  (_, swapImageViews) <- allocate
    (forM swapImages $ \image -> do
      let imageViewCreateInfo = Vk.zero {
        VkImageView.format = swapchainImageFormat,
        VkImageView.image = image,
        VkImageView.subresourceRange = Vk.zero {
          VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
          VkImageView.layerCount = 1,
          VkImageView.levelCount = 1
        },
        VkImageView.viewType = VkImageView.IMAGE_VIEW_TYPE_2D
      }
      VkImageView.createImageView device imageViewCreateInfo allocator
    )
    (mapM_ $ \i -> VkImageView.destroyImageView device i allocator)

  framebuffers <- createFramebuffers swapImageViews swapSettingsExtent

  depthImages <- V.replicateM (V.length swapImages) $
    createDepthImage swapSettingsExtent

  return $ Swapchain {
    swapchainHandle = swapchain,
    swapchainExtent = swapSettingsExtent,
    swapchainFormat = swapSettingsSurfaceFormat,
    swapchainFramebuffers = framebuffers,
    swapchainImages = V.zip swapImages depthImages
  }

createDepthImage :: (MonadResource m, HasVulkan m, HasVulkanDevice m)
  => Extent2D
  -> m Vk.Image
createDepthImage imageExtent = do
  allocator <- getVulkanAllocator
  device <- getDevice

  let imageInfo = Vk.zero {
    VkImage.arrayLayers = 1,
    VkImage.extent = Vk.zero {
      VkExtent3D.depth = 1,
      VkExtent3D.height = VkExtent2D.height imageExtent,
      VkExtent3D.width = VkExtent2D.width imageExtent
    },
    VkImage.format = deviceDepthImageFormat,
    VkImage.imageType = Vk.IMAGE_TYPE_2D,
    VkImage.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
    VkImage.mipLevels = 1,
    VkImage.samples = Vk.SAMPLE_COUNT_1_BIT,
    VkImage.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
    VkImage.tiling = Vk.IMAGE_TILING_OPTIMAL,
    VkImage.usage = Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  }

  snd <$> allocate
    (VkImage.createImage device imageInfo allocator)
    (\image -> VkImage.destroyImage device image allocator)

{-
  memoryRequirements <-
    VkMemory.getImageMemoryRequirements device image

  memory <- snd <$> allocateMemory memoryRequirements
    memoryPropertyFlags

  VkMemory.bindImageMemory device image memory 0
-}

createFramebuffers :: (MonadLogger m, MonadResource m, HasVulkan m,
       HasVulkanDevice m)
  => Vector Vk.ImageView
  -> Extent2D
  -> m (Vector Vk.Framebuffer)
createFramebuffers imageViews imageExtent = do
  allocator <- getVulkanAllocator
  device <- getDevice
  renderPass <- getRenderPass

  debug "Creating framebuffers."
  snd <$> allocate
    (forM imageViews $ \imageView -> do
        let framebufferCreateInfo = Vk.zero {
          VkFramebuffer.attachments = V.singleton imageView,
          VkFramebuffer.height = VkExtent2D.height imageExtent,
          VkFramebuffer.layers = 1,
          VkFramebuffer.renderPass = renderPass,
          VkFramebuffer.width = VkExtent2D.width imageExtent
        }
        VkFramebuffer.createFramebuffer device framebufferCreateInfo allocator
    )
    (mapM_ $ \framebuffer ->
       VkFramebuffer.destroyFramebuffer device framebuffer allocator
    )
