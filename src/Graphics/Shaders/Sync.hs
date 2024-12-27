module Graphics.Shaders.Sync (
  SyncObjects(..),
  createSyncObjects
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.QueueSemaphore as VkSemaphore
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Device

data SyncObjects = SyncObjects {
   syncImageAvailableSemaphore :: Vk.Semaphore,
   syncInFlightFence :: Vk.Fence,
   syncRenderFinishedSemaphore :: Vk.Semaphore
}

createSyncObjects :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Codensity m SyncObjects
createSyncObjects Device{..} = do
  debug "Creating image available semaphore."
  imageAvailableSemaphore <- Codensity $ bracket
    (VkSemaphore.createSemaphore deviceHandle Vk.zero Nothing)
    (\semaphore -> do
       debug "Destroying image available semaphore."
       VkSemaphore.destroySemaphore deviceHandle semaphore Nothing
    )
  debug "Creating render finished semaphore."
  renderFinishedSemaphore <- Codensity $ bracket
    (VkSemaphore.createSemaphore deviceHandle Vk.zero Nothing)
    (\semaphore -> do
       debug "Destroying render finished semaphore."
       VkSemaphore.destroySemaphore deviceHandle semaphore Nothing
    )
  debug "Creating in flight fence."
  let inFlightFenceCreateInfo = Vk.zero {
          VkFence.flags = VkFence.FENCE_CREATE_SIGNALED_BIT
        }
  inFlightFence <- Codensity $ bracket
    (VkFence.createFence deviceHandle inFlightFenceCreateInfo Nothing)
    (\fence -> do
       debug "Destroying in flight fence."
       VkFence.destroyFence deviceHandle fence Nothing
    )
  return $ SyncObjects {
      syncImageAvailableSemaphore = imageAvailableSemaphore,
      syncInFlightFence = inFlightFence,
      syncRenderFinishedSemaphore = renderFinishedSemaphore
    }
