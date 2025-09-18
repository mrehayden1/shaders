module Graphics.Shaders.Internal.Transfer (
  Transfer(..),
  runTransfer,

  transferBuffer,
  transferTexture,

  copyBufferToImage
) where

import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Resource
import Data.ByteString.Internal as BS
import Data.Function
import qualified Data.Vector as V
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
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

-- A monad that brackets graphics queue commands using the current frame's
-- command buffer and finally displaying any rendered output to the window.
newtype Transfer m a = Transfer { unTransfer :: ContT () m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadResource)

instance HasVulkan m => HasVulkan (Transfer m)
instance HasVulkanDevice m => HasVulkanDevice (Transfer m)
instance HasSwapchain m => HasSwapchain (Transfer m)
instance MonadLogger m => MonadLogger (Transfer m)

runTransfer
  :: (MonadIO m, HasVulkanDevice m, MonadLogger m)
  => Transfer m ()
  -> m ()
runTransfer (Transfer m) = do
  device <- getDevice
  commandBuffer <- getTransferCommandBuffer

  flip runContT return $ do
    ContT $ VkCmd.useCommandBuffer commandBuffer Vk.zero . (&) ()
    m

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

  return ()

-- Write a buffer synchronously using the transfer queue
transferBuffer
  :: (MonadIO m, HasVulkanDevice m)
  => Buffer a
  -> [HostFormat a]
  -> Transfer m ()
transferBuffer Buffer{..} as = do
  commandBuffer <- getTransferCommandBuffer

  -- Copy data into the staging buffer.
  liftIO . bufferWriter $ as
  -- Transfer data in staging buffers to their writable buffer.
  let copy = V.singleton $ VkCmd.BufferCopy Vk.zero Vk.zero
               . fromIntegral $ bufferStride * bufferNumElems
  VkCmd.cmdCopyBuffer commandBuffer bufferStagingBufferHandle
    bufferHandle copy

transferTexture
  :: (MonadAsyncException m, HasVulkanDevice m)
  => Texture
  -> ByteString
  -> Word
  -> Word
  -> Transfer m ()
transferTexture Texture{..} imageData width height = do
  let BS.BS imageBytes imageSize = imageData

  -- Fill the buffer
  liftIO . withForeignPtr imageBytes $ \ptr ->
    copyBytes textureStagingBufferPtr (castPtr ptr) imageSize

  copyBufferToImage textureStagingBufferHandle textureImage width height

copyBufferToImage :: (MonadAsyncException m, HasVulkanDevice m)
  => Vk.Buffer
  -> Vk.Image
  -> Word
  -> Word
  -> Transfer m ()
copyBufferToImage buffer image width height = do
  commandBuffer <- getTransferCommandBuffer

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
