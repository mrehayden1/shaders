module Graphics.Shaders.Internal.Texture (
  loadTexture,
  Texture(..)
) where

import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import Data.ByteString.Internal as BS
import Foreign.ForeignPtr
import Foreign.Marshal
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
import Graphics.Shaders.Texture.Loader.TGA

data Texture = Texture {
  textureImageView :: Vk.ImageView,
  textureReleaseKeys :: (ReleaseKey, ReleaseKey, ReleaseKey, ReleaseKey),
  textureSampler :: Vk.Sampler
}

loadTexture :: (MonadAsyncException m, MonadLogger m, MonadResource m,
    HasVulkan m, HasVulkanDevice m)
  => FilePath
  -> m Texture
loadTexture fileName = do
  allocator <- getVulkanAllocator
  device <- getDevice

  debug "Loading texture image."
  TGA{..} <- decodeFile fileName
  let BS.BS imageBytes imageNumBytes = tgaData
      bufferSize = fromIntegral imageNumBytes

  debug "Creating image."
  (image, imageReleaseKey, _, memoryReleaseKey) <- createImage
    tgaWidth
    tgaHeight
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
  (buffer, bufferReleaseKey, bufferMemory, bufferMemoryReleaseKey)
    <- createVkBuffer bufferSize stagingBufferUsageFlags
         stagingMemoryPropertyFlags

  -- Fill the buffer
  debug "Filling staging buffer."
  bufferPtr <- VkMemory.mapMemory device bufferMemory 0 bufferSize
    Vk.zero
  liftIO . withForeignPtr imageBytes $ \ptr ->
    copyBytes (castPtr bufferPtr) ptr imageNumBytes
  VkMemory.unmapMemory device bufferMemory

  debug "Copying image."
  transitionImageLayout image Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  copyBufferToImage buffer image tgaWidth tgaHeight
  transitionImageLayout image Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  debug "Destryoing staging buffer."
  release bufferMemoryReleaseKey
  release bufferReleaseKey

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
        samplerReleaseKey)

  return $ Texture imageView releaseKeys sampler
