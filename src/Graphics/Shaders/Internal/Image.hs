module Graphics.Shaders.Internal.Image (
  createImage,
  createImageView,
  transitionImageLayout,
  copyBufferToImage
) where

import Control.Monad.Exception
import Control.Monad.Trans.Resource
import Data.Vector as V
import Vulkan.Core10.APIConstants as Vk
import Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.CommandBufferBuilding as VkImageCopy
  (BufferImageCopy(..), ImageSubresourceLayers(..))
import Vulkan.Core10.Enums as Vk
import Vulkan.Core10.FundamentalTypes as VkExtent3D (Extent3D(..))
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Image as VkImage
import qualified Vulkan.Core10.ImageView as VkImageView
import Vulkan.Core10.MemoryManagement as VkMemory
import Vulkan.Core10.OtherTypes as VkBarrier (ImageMemoryBarrier(..))
import Vulkan.CStruct.Extends
import Vulkan.Zero as Vk

import Graphics.Shaders.Exception
import Graphics.Shaders.Internal.Command
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Memory
import Graphics.Shaders.Logger.Class

createImage :: (MonadAsyncException m, MonadLogger m, MonadResource m,
    HasVulkan m, HasVulkanDevice m)
  => Int
  -> Int
  -> Vk.Format
  -> Vk.ImageTiling
  -> Vk.ImageUsageFlags
  -> Vk.MemoryPropertyFlags
  -> m (Vk.Image, ReleaseKey, VkMemory.DeviceMemory, ReleaseKey)
createImage width height format tiling usageFlags memoryPropertyFlags = do
  allocator <- getVulkanAllocator
  device <- getDevice

  let imageInfo = Vk.zero {
    VkImage.arrayLayers = 1,
    VkImage.extent = Vk.zero {
      VkExtent3D.depth = 1,
      VkExtent3D.height = fromIntegral height,
      VkExtent3D.width = fromIntegral width
    },
    VkImage.format = format,
    VkImage.imageType = Vk.IMAGE_TYPE_2D,
    VkImage.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
    VkImage.mipLevels = 1,
    VkImage.samples = Vk.SAMPLE_COUNT_1_BIT,
    VkImage.sharingMode = Vk.SHARING_MODE_EXCLUSIVE,
    VkImage.tiling = tiling,
    VkImage.usage = usageFlags
  }
  (imageReleaseKey, image) <- allocate
    (VkImage.createImage device imageInfo allocator)
    (\image -> VkImage.destroyImage device image allocator)

  memoryRequirements <-
    VkMemory.getImageMemoryRequirements device image

  (memoryReleaseKey, memory) <- allocateMemory memoryRequirements
    memoryPropertyFlags

  VkMemory.bindImageMemory device image memory 0

  return (image, imageReleaseKey, memory, memoryReleaseKey)

createImageView :: (MonadResource m, HasVulkan m, HasVulkanDevice m)
  => Vk.Image
  -> Vk.Format
  -> m (ReleaseKey, Vk.ImageView)
createImageView image format = do
  allocator <- getVulkanAllocator
  device <- getDevice

  allocate
    (do
      let imageViewCreateInfo = Vk.zero {
        VkImageView.format = format,
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
    (\v -> VkImageView.destroyImageView device v allocator)

transitionImageLayout :: (MonadAsyncException m, MonadLogger m,
    HasVulkanDevice m)
  => Vk.Image
  -> Vk.ImageLayout
  -> Vk.ImageLayout
  -> m ()
transitionImageLayout image oldLayout newLayout = do
  device <- getDevice
  queue <- getQueue
  commandPool <- getCommandPool

  withOneTimeSubmitCommandBuffer device queue commandPool $
    \commandBuffer -> do
      (srcStage, dstStage, srcAccessMask, dstAccessMask) <-
        case (oldLayout, newLayout) of
          (Vk.IMAGE_LAYOUT_UNDEFINED,
           Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) ->
            return (
              Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT,
              Vk.PIPELINE_STAGE_TRANSFER_BIT,
              Vk.zero,
              Vk.ACCESS_TRANSFER_WRITE_BIT
            )
          (Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
           Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) ->
            return (
              Vk.PIPELINE_STAGE_TRANSFER_BIT,
              Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
              Vk.ACCESS_TRANSFER_WRITE_BIT,
              Vk.ACCESS_SHADER_READ_BIT
            )
          _ -> throw ShadersUnsupportedTransitionException

      let imageMemoryBarrier = Vk.zero {
        VkBarrier.dstAccessMask = dstAccessMask,
        VkBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
        VkBarrier.image = image,
        VkBarrier.newLayout = newLayout,
        VkBarrier.oldLayout = oldLayout,
        VkBarrier.srcAccessMask = srcAccessMask,
        VkBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
        VkBarrier.subresourceRange = Vk.zero {
          VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
          VkImageView.layerCount = 1,
          VkImageView.levelCount = 1
        }
      }

      VkCmd.cmdPipelineBarrier commandBuffer srcStage dstStage Vk.zero mempty
        mempty . V.singleton . SomeStruct $ imageMemoryBarrier

copyBufferToImage :: (MonadAsyncException m, MonadLogger m, HasVulkanDevice m)
  => Vk.Buffer
  -> Vk.Image
  -> Int
  -> Int
  -> m ()
copyBufferToImage buffer image width height = do
  device <- getDevice
  queue <- getQueue
  commandPool <- getCommandPool

  withOneTimeSubmitCommandBuffer device queue commandPool $
    \commandBuffer -> do
      let imageCopy = Vk.zero {
        VkImageCopy.imageSubresource = Vk.zero {
          VkImageCopy.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
          VkImageCopy.layerCount = 1
        },
        VkImageCopy.imageExtent = Vk.zero {
          VkExtent3D.depth = 1,
          VkExtent3D.height = fromIntegral height,
          VkExtent3D.width = fromIntegral width
        }
      }
      VkCmd.cmdCopyBufferToImage commandBuffer buffer image
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL . V.singleton $ imageCopy
