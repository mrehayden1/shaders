module Graphics.Shaders.Internal.Image (
  createImage,
  createImageView
) where

import Control.Monad.Exception
import Control.Monad.Trans.Resource

import Vulkan.Core10.Enums as Vk
import Vulkan.Core10.FundamentalTypes as VkExtent3D (Extent3D(..))
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Image as VkImage
import qualified Vulkan.Core10.ImageView as VkImageView
import Vulkan.Core10.MemoryManagement as VkMemory
import Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Memory
import Graphics.Shaders.Logger.Class

createImage
  :: (MonadAsyncException m, MonadLogger m, MonadResource m, HasVulkan m,
      HasVulkanDevice m)
  => Word
  -> Word
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

createImageView
  :: (MonadResource m, HasVulkan m, HasVulkanDevice m)
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
