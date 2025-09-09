module Graphics.Shaders.Internal.Command (
  withOneTimeSubmitCommandBuffer
) where

import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Vulkan.Core10.CommandBuffer as VkCmdBuffer
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends
import Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class

withOneTimeSubmitCommandBuffer :: (MonadIO m, MonadLogger m)
  => Vk.Device
  -> Vk.Queue
  -> Vk.CommandPool
  -> (Vk.CommandBuffer -> m ())
  -> m ()
withOneTimeSubmitCommandBuffer deviceHandle queueHandle commandPool c = do
  let commandBufferCreateInfo = Vk.zero {
        VkCmdBuffer.commandBufferCount = 1,
        VkCmdBuffer.commandPool = commandPool,
        VkCmdBuffer.level = VkCmdBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
      }

  debug "Allocating temporary command buffer."
  cmdBuffers <- VkCmdBuffer.allocateCommandBuffers deviceHandle
                  commandBufferCreateInfo
  let cmdBuffer = V.head cmdBuffers

  let beginInfo = Vk.zero {
        VkCmdBuffer.flags =
          VkCmdBuffer.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
      }
  VkCmdBuffer.beginCommandBuffer cmdBuffer beginInfo

  c cmdBuffer

  VkCmdBuffer.endCommandBuffer cmdBuffer

  let submitInfo = SomeStruct $ Vk.zero {
        VkQueue.commandBuffers = fmap Vk.commandBufferHandle . V.singleton
          $ cmdBuffer
      }

  VkQueue.queueSubmit queueHandle (V.singleton submitInfo) Vk.zero
  VkQueue.queueWaitIdle queueHandle

  debug "Destroying temporary command buffer."
  VkCmdBuffer.freeCommandBuffers deviceHandle commandPool cmdBuffers
