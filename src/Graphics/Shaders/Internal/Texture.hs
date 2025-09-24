module Graphics.Shaders.Internal.Texture (
  TextureSampler(..),
  createTextureSampler,
  destroyTextureSampler,

  TextureFilter(..),
  TextureWrap(..),
  TextureMipmapMode(..),

  Texture(..),

  createTexture,
  destroyTexture
) where

import Control.Monad.IO.Class
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

-- A nicer interface to VkSampler objects, which must be allocated.
data TextureSampler = TextureSampler {
  textureSamplerHandle :: Vk.Sampler,
  textureSamplerReleaseKey :: ReleaseKey
}

data TextureFilter = TextureFilterLinear | TextureFilterNearest

toVkFilter :: TextureFilter -> Vk.Filter
toVkFilter TextureFilterLinear = Vk.FILTER_LINEAR
toVkFilter TextureFilterNearest = Vk.FILTER_NEAREST

data TextureWrap =
    TextureWrapClamp
  | TextureWrapMirroredRepeat
  | TextureWrapRepeat

toVkAddressMode :: TextureWrap -> Vk.SamplerAddressMode
toVkAddressMode = \case
  TextureWrapClamp          -> Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  TextureWrapMirroredRepeat -> Vk.SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  TextureWrapRepeat         -> Vk.SAMPLER_ADDRESS_MODE_REPEAT

data TextureMipmapMode = TextureMipmapLinear | TextureMipmapNearest

toVkMipmapMode :: TextureMipmapMode -> SamplerMipmapMode
toVkMipmapMode TextureMipmapLinear  = Vk.SAMPLER_MIPMAP_MODE_LINEAR
toVkMipmapMode TextureMipmapNearest = Vk.SAMPLER_MIPMAP_MODE_NEAREST

createTextureSampler
  :: (HasVulkan m, HasVulkanDevice m, MonadResource m, MonadLogger m)
  => TextureFilter
  -> TextureFilter
  -> TextureMipmapMode
  -> TextureWrap
  -> TextureWrap
  -> m TextureSampler
createTextureSampler magF minF mipMode wrapU wrapV = do
  device <- getDevice
  allocator <- getVulkanAllocator

  let samplerInfo = Vk.zero {
    VkSampler.addressModeU = toVkAddressMode wrapU,
    VkSampler.addressModeV = toVkAddressMode wrapV,
    VkSampler.magFilter = toVkFilter magF,
    VkSampler.minFilter = toVkFilter minF,
    VkSampler.mipmapMode = toVkMipmapMode mipMode
  }

  debug "Creating sampler."
  (samplerReleaseKey, sampler) <- allocate
    (VkSampler.createSampler device samplerInfo allocator)
    (\s -> VkSampler.destroySampler device s allocator)

  return $ TextureSampler sampler samplerReleaseKey

destroyTextureSampler :: MonadIO m => TextureSampler -> m ()
destroyTextureSampler TextureSampler{..} = do
  release textureSamplerReleaseKey


data Texture = Texture {
  textureImage :: Vk.Image,
  textureImageView :: Vk.ImageView,
  textureReleaseKeys :: (ReleaseKey, ReleaseKey, ReleaseKey,
    ReleaseKey, ReleaseKey),
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

  let releaseKeys = (imageReleaseKey, memoryReleaseKey, imageViewReleaseKey,
        bufferMemoryReleaseKey, bufferReleaseKey)

  return $ Texture {
      textureImage = image,
      textureImageView = imageView,
      textureReleaseKeys = releaseKeys,
      textureStagingBufferHandle = buffer,
      textureStagingBufferPtr = bufferPtr
    }

destroyTexture :: MonadResource m => Texture -> m ()
destroyTexture Texture{..} = do
  let (imageReleaseKey, memoryReleaseKey, imageViewReleaseKey,
       bufferMemoryReleaseKey, bufferReleaseKey)
        = textureReleaseKeys

  release imageReleaseKey
  release memoryReleaseKey
  release imageViewReleaseKey
  release bufferMemoryReleaseKey
  release bufferReleaseKey
