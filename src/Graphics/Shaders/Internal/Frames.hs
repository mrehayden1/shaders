module Graphics.Shaders.Internal.Frames (
  Frame(..),
  SyncObjects(..),

  createFrames
) where

import Control.Monad
import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.CommandBuffer as VkCmdBuffer
import qualified Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.QueueSemaphore as VkSemaphore
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Instance

framesInFlight :: Int
framesInFlight = 2


-- A "frame" is the synchronisation scope within which commands are submitted
-- to the pipeline to render the next available image in the swapchain.
data Frame = Frame {
  frameCommandBuffer :: Vk.CommandBuffer,
  frameSyncObjects :: SyncObjects
}

data SyncObjects = SyncObjects {
  syncInFlightFence :: Vk.Fence,
  syncRenderFinishedSemaphore :: Vk.Semaphore
}

createFrames :: (HasVulkan m, HasVulkanDevice m, MonadLogger m,
       MonadResource m)
  => m (Vector Frame)
createFrames = do
  commandBuffers <- createCommandBuffers framesInFlight
  syncObjects <- createSyncObjects framesInFlight
  return . V.zipWith Frame commandBuffers $ syncObjects

createSyncObjects :: (MonadLogger m, MonadResource m, HasVulkan m,
       HasVulkanDevice m)
  => Int
  -> m (Vector SyncObjects)
createSyncObjects n = do
  allocator <- getVulkanAllocator
  device <- getDevice

  fmap V.fromList . replicateM n $ do
    debug "Creating render finished semaphore."
    (_, renderFinishedSemaphore) <- allocate
      (VkSemaphore.createSemaphore device Vk.zero allocator)
      (\semaphore ->
         VkSemaphore.destroySemaphore device semaphore allocator
      )
    debug "Creating in flight fence."
    -- Unsignalled fence object by default.
    let inFlightFenceCreateInfo = Vk.zero
    (_, inFlightFence) <- allocate
      (VkFence.createFence device inFlightFenceCreateInfo allocator)
      (\fence -> VkFence.destroyFence device fence allocator)
    return $ SyncObjects {
        syncInFlightFence = inFlightFence,
        syncRenderFinishedSemaphore = renderFinishedSemaphore
      }

createCommandBuffers :: (MonadLogger m, MonadResource m, HasVulkanDevice m)
  => Int
  -> m (Vector Vk.CommandBuffer)
createCommandBuffers n = do
  device <- getDevice
  commandPool <- getCommandPool

  let commandBufferCreateInfo = Vk.zero {
    VkCmdBuffer.commandBufferCount = fromIntegral n,
    VkCmdBuffer.commandPool = commandPool,
    VkCmdBuffer.level = VkCmdBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
  }
  debug "Allocating command buffer."
  snd <$> allocate
    (VkCmdBuffer.allocateCommandBuffers device commandBufferCreateInfo)
    (\buffers -> do
       VkCmdBuffer.freeCommandBuffers device commandPool buffers)
