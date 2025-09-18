module Graphics.Shaders.Internal.Texture (
  Texture(..),

  createTexture,
  destroyTexture
) where

import Control.Monad.Exception
import Control.Monad.Trans.Resource
import Data.Bits
import Foreign.Ptr
import Vulkan.Core10.Buffer as VkBuffer
import Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Memory as VkMemory
import qualified Vulkan.Core10.Sampler as VkSampler
import Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Image
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Logger.Class

data Texture = Texture {
  textureImage :: Vk.Image,
  textureImageView :: Vk.ImageView,
  textureReleaseKeys :: (ReleaseKey, ReleaseKey, ReleaseKey, ReleaseKey,
    ReleaseKey, ReleaseKey),
  textureSampler :: Vk.Sampler,
  textureStagingBufferHandle :: Vk.Buffer,
  textureStagingBufferPtr :: Ptr ()
}

createTexture
  :: (MonadAsyncException m, MonadLogger m, MonadResource m, HasVulkan m,
      HasVulkanDevice m)
  => Word
  -> Word
  -> m Texture
createTexture width height = do
  allocator <- getVulkanAllocator
  device <- getDevice

  let bufferSize = fromIntegral $ width * height * 4

  debug "Creating image."
  (image, imageReleaseKey, _, memoryReleaseKey) <- createImage
    width
    height
    Vk.FORMAT_R8G8B8A8_SRGB
    Vk.IMAGE_TILING_OPTIMAL
    (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT)
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  (imageViewReleaseKey, imageView)
    <- createImageView image Vk.FORMAT_R8G8B8A8_SRGB

  -- Create a staging buffer
  debug "Creating staging buffer."
  let stagingBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
  (buffer, bufferReleaseKey, bufferMemory, bufferMemoryReleaseKey) <-
    createVkBuffer bufferSize stagingBufferUsageFlags
      stagingMemoryPropertyFlags
  bufferPtr <- VkMemory.mapMemory device bufferMemory 0 bufferSize Vk.zero

  let samplerInfo = Vk.zero {
    VkSampler.magFilter = Vk.FILTER_LINEAR,
    VkSampler.minFilter = Vk.FILTER_LINEAR,
    VkSampler.mipmapMode = Vk.SAMPLER_MIPMAP_MODE_LINEAR
  }

  debug "Creating sampler."
  (samplerReleaseKey, sampler) <- allocate
    (VkSampler.createSampler device samplerInfo allocator)
    (\s -> VkSampler.destroySampler device s allocator)

  let releaseKeys = (imageReleaseKey, memoryReleaseKey, imageViewReleaseKey,
        samplerReleaseKey, bufferMemoryReleaseKey, bufferReleaseKey)

  return $ Texture {
      textureImage = image,
      textureImageView = imageView,
      textureReleaseKeys = releaseKeys,
      textureSampler = sampler,
      textureStagingBufferHandle = buffer,
      textureStagingBufferPtr = bufferPtr
    }

destroyTexture :: MonadResource m => Texture -> m ()
destroyTexture Texture{..} = do
  let (imageReleaseKey, memoryReleaseKey, imageViewReleaseKey,
       samplerReleaseKey, bufferMemoryReleaseKey, bufferReleaseKey)
        = textureReleaseKeys

  release imageReleaseKey
  release memoryReleaseKey
  release imageViewReleaseKey
  release samplerReleaseKey
  release bufferMemoryReleaseKey
  release bufferReleaseKey
