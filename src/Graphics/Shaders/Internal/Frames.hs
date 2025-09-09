module Graphics.Shaders.Internal.Frames (
  Frame(..),
  SyncObjects(..),

  createFrames
) where

import Control.Monad
import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.Core10.AllocationCallbacks
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.CommandBuffer as VkCmdBuffer
import qualified Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.QueueSemaphore as VkSemaphore
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Internal.Device

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

createFrames :: (MonadLogger m, MonadResource m)
  => Maybe AllocationCallbacks
  -> Device
  -> m (Vector Frame)
createFrames allocator device = do
  commandBuffers <- createCommandBuffers device framesInFlight
  syncObjects <- createSyncObjects allocator device framesInFlight
  return . V.zipWith Frame commandBuffers $ syncObjects

createSyncObjects :: (MonadLogger m, MonadResource m)
  => Maybe AllocationCallbacks
  -> Device
  -> Int
  -> m (Vector SyncObjects)
createSyncObjects allocator Device{..} n = do

  fmap V.fromList . replicateM n $ do
    debug "Creating render finished semaphore."
    (_, renderFinishedSemaphore) <- allocate
      (VkSemaphore.createSemaphore deviceHandle Vk.zero allocator)
      (\semaphore ->
         VkSemaphore.destroySemaphore deviceHandle semaphore allocator
      )
    debug "Creating in flight fence."
    let inFlightFenceCreateInfo = Vk.zero {
            VkFence.flags = VkFence.FENCE_CREATE_SIGNALED_BIT
          }
    (_, inFlightFence) <- allocate
      (VkFence.createFence deviceHandle inFlightFenceCreateInfo allocator)
      (\fence -> VkFence.destroyFence deviceHandle fence allocator)
    return $ SyncObjects {
        syncInFlightFence = inFlightFence,
        syncRenderFinishedSemaphore = renderFinishedSemaphore
      }

createCommandBuffers :: (MonadLogger m, MonadResource m)
  => Device
  -> Int
  -> m (Vector Vk.CommandBuffer)
createCommandBuffers Device{..} n = do
  let commandBufferCreateInfo = Vk.zero {
    VkCmdBuffer.commandBufferCount = fromIntegral n,
    VkCmdBuffer.commandPool = deviceCommandPool,
    VkCmdBuffer.level = VkCmdBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
  }
  debug "Allocating command buffer."
  snd <$> allocate
    (VkCmdBuffer.allocateCommandBuffers deviceHandle commandBufferCreateInfo)
    (\buffers -> do
       VkCmdBuffer.freeCommandBuffers deviceHandle deviceCommandPool buffers)
