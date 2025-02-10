module Graphics.Shaders.Internal.Texture (
  loadTexture,
  Texture(..)
) where

import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Bits
import Data.ByteString.Internal as BS
import Data.Vector as V
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Vulkan.Core10.APIConstants as Vk
import Vulkan.Core10.Buffer as VkBuffer
import Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.CommandBufferBuilding as VkImageCopy
  (BufferImageCopy(..), ImageSubresourceLayers(..))
import Vulkan.Core10.Enums as Vk
import Vulkan.Core10.FundamentalTypes as VkExtent3D (Extent3D(..))
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Image as VkImage
import qualified Vulkan.Core10.ImageView as VkImageView
import qualified Vulkan.Core10.Memory as VkMemory
import Vulkan.Core10.MemoryManagement as VkMemory
import Vulkan.Core10.OtherTypes as VkBarrier (ImageMemoryBarrier(..))
import qualified Vulkan.Core10.Sampler as VkSampler
import Vulkan.CStruct.Extends
import Vulkan.Zero as Vk

import Graphics.Shaders.Base
import Graphics.Shaders.Exception
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Texture.Loader.TGA

data Texture = Texture {
  textureImageView :: Vk.ImageView,
  textureSampler :: Vk.Sampler
}

loadTexture :: (MonadAsyncException m, MonadLogger m)
  => FilePath
  -> ShadersT m Texture
loadTexture fileName = do
  deviceHandle <- getDeviceHandle

  debug "Loading texture image."
  TGA{..} <- decodeFile fileName
  let BS.BS imageBytes imageNumBytes = tgaData
      bufferSize = fromIntegral imageNumBytes

  -- Create a staging buffer
  -- TODO Clean up the staging buffer after it's used.
  debug "Creating staging buffer."
  let stagingBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
  (stagingBuffer, stagingBufferMemory) <- withBuffer' bufferSize
    stagingBufferUsageFlags stagingMemoryPropertyFlags

  -- Fill the buffer
  debug "Filling staging buffer."
  bufferPtr <-
    VkMemory.mapMemory deviceHandle stagingBufferMemory 0 bufferSize Vk.zero
  liftIO . withForeignPtr imageBytes $ \ptr ->
    copyBytes (castPtr bufferPtr) ptr imageNumBytes
  VkMemory.unmapMemory deviceHandle stagingBufferMemory

  debug "Creating image."
  (image, _) <- createImage
    tgaWidth
    tgaHeight
    Vk.FORMAT_R8G8B8A8_SRGB
    Vk.IMAGE_TILING_OPTIMAL
    (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT)
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  debug "Copying image."
  transitionImageLayout image Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  copyBufferToImage stagingBuffer image tgaWidth tgaHeight
  transitionImageLayout image Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  imageView <- withImageView image Vk.FORMAT_R8G8B8A8_SRGB

  let samplerInfo = Vk.zero {
    VkSampler.magFilter = Vk.FILTER_LINEAR,
    VkSampler.minFilter = Vk.FILTER_LINEAR,
    VkSampler.mipmapMode = Vk.SAMPLER_MIPMAP_MODE_LINEAR
  }

  debug "Creating sampler."
  sampler <- fromCps $ bracket
    (VkSampler.createSampler deviceHandle samplerInfo Nothing)
    (\s -> do
      debug "Destroying sampler."
      VkSampler.destroySampler deviceHandle s Nothing
    )

  return $ Texture imageView sampler

withImageView :: (MonadAsyncException m, MonadLogger m)
  => Vk.Image
  -> Vk.Format
  -> ShadersT m Vk.ImageView
withImageView image format = do
  deviceHandle <- getDeviceHandle
  fromCps $
    bracket
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
        VkImageView.createImageView deviceHandle imageViewCreateInfo Nothing
      )
      (\imageView -> do
        debug "Destroying swap chain image views."
        VkImageView.destroyImageView deviceHandle imageView Nothing
      )

createImage :: (MonadAsyncException m, MonadLogger m)
  => Int
  -> Int
  -> Vk.Format
  -> Vk.ImageTiling
  -> Vk.ImageUsageFlags
  -> Vk.MemoryPropertyFlags
  -> ShadersT m (Vk.Image, VkMemory.DeviceMemory)
createImage width height format tiling usageFlags memoryProperties = do
  deviceHandle <- getDeviceHandle
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
  image <- fromCps $ bracket
    (VkImage.createImage deviceHandle imageInfo Nothing)
    (\image -> do
       debug "Destroying image."
       VkImage.destroyImage deviceHandle image Nothing
    )

  memoryRequirements <-
    VkMemory.getImageMemoryRequirements deviceHandle image

  memory <- allocateMemory memoryRequirements memoryProperties

  VkMemory.bindImageMemory deviceHandle image memory 0

  return (image, memory)


transitionImageLayout :: (MonadAsyncException m, MonadLogger m)
  => Vk.Image
  -> Vk.ImageLayout
  -> Vk.ImageLayout
  -> ShadersT m ()
transitionImageLayout image oldLayout newLayout =
  withOneTimeSubmitCommandBuffer $ \commandBuffer -> do
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
        _ -> lift . throw $ ShadersUnsupportedTransitionException

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


copyBufferToImage :: (MonadAsyncException m, MonadLogger m)
  => Vk.Buffer
  -> Vk.Image
  -> Int
  -> Int
  -> ShadersT m ()
copyBufferToImage buffer image width height =
  withOneTimeSubmitCommandBuffer $ \commandBuffer -> do
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
