module Graphics.Shaders.Internal.Sync (
  SyncObjects(..),
  withSyncObjects
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.QueueSemaphore as VkSemaphore
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Internal.Device

data SyncObjects = SyncObjects {
   syncInFlightFence :: Vk.Fence,
   syncRenderFinishedSemaphore :: Vk.Semaphore
}

withSyncObjects :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Int
  -> Codensity m (Vector SyncObjects)
withSyncObjects Device{..} n =
  fmap V.fromList . replicateM n $ do
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
        syncInFlightFence = inFlightFence,
        syncRenderFinishedSemaphore = renderFinishedSemaphore
      }
