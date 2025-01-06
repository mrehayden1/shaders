module Graphics.Shaders.CommandBuffer (
  withCommandBuffers
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.CommandBuffer as VkBuffer
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Logger.Class

withCommandBuffers :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Int
  -> Codensity m (Vector Vk.CommandBuffer)
withCommandBuffers Device{..} n = do
  let commandBufferCreateInfo = Vk.zero {
    VkBuffer.commandBufferCount = fromIntegral n,
    VkBuffer.commandPool = deviceCommandPool,
    VkBuffer.level = VkBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
  }
  debug "Allocating command buffer."
  Codensity $ bracket
    (VkBuffer.allocateCommandBuffers deviceHandle commandBufferCreateInfo)
    (\buffers -> do
       debug "Destroying command buffer."
       VkBuffer.freeCommandBuffers deviceHandle deviceCommandPool buffers)
