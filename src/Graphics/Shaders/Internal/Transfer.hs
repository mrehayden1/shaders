module Graphics.Shaders.Internal.Transfer (
  MonadTransfer,

  TransferT(..),
  runTransferT,

  writeBuffer,
  writeBufferUnsafe,

  writeTexture,

  copyBufferToImage
) where

import Control.Monad.Except
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.ByteString.Internal as BS
import qualified Data.Vector as V
import Foreign
import qualified Vulkan.Core10.APIConstants as Vk
import qualified Vulkan.Core10.CommandBuffer as VkCmd
import qualified Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.CommandBufferBuilding as VkImageCopy
  (BufferImageCopy(..), ImageSubresourceLayers(..))
import qualified Vulkan.Core10.ImageView as VkImageView
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Fence as VkFence
import Vulkan.Core10.FundamentalTypes as VkExtent3D (Extent3D(..))
import qualified Vulkan.Core10.Handles as Vk
import Vulkan.Core10.OtherTypes as VkBarrier (ImageMemoryBarrier(..))
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Swapchain
import Graphics.Shaders.Internal.Texture
import Graphics.Shaders.Logger.Class


-- A monad that brackets gpu commands using the transfer queue command buffer.
class Monad m => MonadTransfer m where
  getCommandBuffer :: m Vk.CommandBuffer
  default getCommandBuffer
    :: (MonadTransfer m', MonadTrans t, m ~ t m')
    => m Vk.CommandBuffer
  getCommandBuffer = lift getCommandBuffer

instance MonadTransfer m => MonadTransfer (ExceptT e m)
instance MonadTransfer m => MonadTransfer (MaybeT m)
instance MonadTransfer m => MonadTransfer (ReaderT e m)
instance MonadTransfer m => MonadTransfer (StateT e m)

newtype TransferT m a = TransferT { unTransferT :: IdentityT m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadException,
    MonadAsyncException, MonadTrans, MonadReader e, MonadError err,
    MonadResource)

instance HasVulkanDevice m => MonadTransfer (TransferT m) where
  getCommandBuffer = getTransferCommandBuffer
instance HasVulkan m => HasVulkan (TransferT m)
instance HasVulkanDevice m => HasVulkanDevice (TransferT m)
instance HasSwapchain m => HasSwapchain (TransferT m)
instance MonadLogger m => MonadLogger (TransferT m)

runTransferT
  :: (MonadIO m, HasVulkanDevice m, MonadLogger m)
  => TransferT m a
  -> m a
runTransferT (TransferT m) = do
  device <- getDevice
  commandBuffer <- getTransferCommandBuffer

  a <- runIdentityT $
    VkCmd.beginCommandBuffer commandBuffer Vk.zero
      *> m
      <* VkCmd.endCommandBuffer commandBuffer

  logTrace "Submitting transfer queue."
  let submitInfos = fmap SomeStruct . V.singleton $ Vk.zero {
    VkQueue.commandBuffers =
      fmap VkCmd.commandBufferHandle . V.fromList $ [ commandBuffer ]
  }
  queue <- getTransferQueue
  fence <- getTransferFence
  VkQueue.queueSubmit queue submitInfos fence

  let fences = V.singleton fence
  -- Ignore TIMEOUT.
  _ <- VkFence.waitForFences device fences True maxBound
  VkFence.resetFences device fences

  return a

-- Write a buffer synchronously using the transfer queue
writeBuffer
  :: (MonadIO m, MonadTransfer m)
  => Buffer a
  -> [HostFormat a]
  -> m ()
writeBuffer Buffer{..} as = do
  commandBuffer <- getCommandBuffer

  -- Copy data into the staging buffer.
  liftIO . bufferWriter $ as
  -- Transfer data in staging buffers to their writable buffer.
  let copy = V.singleton $ VkCmd.BufferCopy Vk.zero Vk.zero
               . fromIntegral $ bufferStride * bufferNumElems
  VkCmd.cmdCopyBuffer commandBuffer bufferStagingBufferHandle
    bufferHandle copy

-- Efficiently copy raw bytes into a `Buffer a`, bypassing any parsing and
-- re-serialising of data to and from `HostFormat a`.
--
-- Warning: Only use this if you know what you are doing.
writeBufferUnsafe
  :: forall a b m. (MonadIO m, MonadTransfer m)
  => Buffer a
  -> ForeignPtr b
  -> Int
  -> Int
  -> m ()
writeBufferUnsafe Buffer{..} ptr' offset len = do
  commandBuffer <- getCommandBuffer

  -- Copy raw bytes into the staging buffer.
  liftIO . withForeignPtr ptr' $ \ptr ->
    copyBytes bufferStagingBufferPtr (ptr `plusPtr` offset) len

  -- Transfer data in staging buffers to their writable buffer.
  let copy = V.singleton $ VkCmd.BufferCopy Vk.zero Vk.zero
               . fromIntegral $ len
  VkCmd.cmdCopyBuffer commandBuffer bufferStagingBufferHandle
    bufferHandle copy

writeTexture
  :: (MonadAsyncException m, MonadTransfer m)
  => Texture
  -> ByteString
  -> Word
  -> Word
  -> m ()
writeTexture Texture{..} imageData width height = do
  let BS.BS imageBytes imageSize = imageData

  -- Fill the buffer
  liftIO . withForeignPtr imageBytes $ \ptr ->
    copyBytes textureStagingBufferPtr (castPtr ptr) imageSize

  copyBufferToImage textureStagingBufferHandle textureImage width height

copyBufferToImage :: (MonadAsyncException m, MonadTransfer m)
  => Vk.Buffer
  -> Vk.Image
  -> Word
  -> Word
  -> m ()
copyBufferToImage buffer image width height = do
  commandBuffer <- getCommandBuffer

  let imageToWriteMemoryBarrier = Vk.zero {
    VkBarrier.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
    VkBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
    VkBarrier.image = image,
    VkBarrier.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
    VkBarrier.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
    VkBarrier.srcAccessMask = Vk.zero,
    VkBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
    VkBarrier.subresourceRange = Vk.zero {
      VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
      VkImageView.layerCount = 1,
      VkImageView.levelCount = 1
    }
  }
  VkCmd.cmdPipelineBarrier commandBuffer Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
        Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.zero mempty mempty
    . V.singleton . SomeStruct $ imageToWriteMemoryBarrier

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

  let imageToReadMemoryBarrier = Vk.zero {
    VkBarrier.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT,
    VkBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
    VkBarrier.image = image,
    VkBarrier.newLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
    VkBarrier.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
    VkBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
    VkBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
    VkBarrier.subresourceRange = Vk.zero {
      VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
      VkImageView.layerCount = 1,
      VkImageView.levelCount = 1
    }
  }
  VkCmd.cmdPipelineBarrier commandBuffer Vk.PIPELINE_STAGE_TRANSFER_BIT
         Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT Vk.zero mempty mempty
    . V.singleton . SomeStruct $ imageToReadMemoryBarrier
