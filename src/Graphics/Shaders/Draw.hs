module Graphics.Shaders.Draw (
  drawFrame
) where

import Control.Monad.Codensity
import Control.Monad.State
import Data.Function
import qualified Data.Vector as V
import Vulkan.Core10.CommandBuffer as VkCmd hiding (
  CommandBufferInheritanceInfo(..))
import Vulkan.Core10.CommandBufferBuilding as VkCmd
import Vulkan.Core10.Enums as VkEnum
import Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import Vulkan.Core10.FundamentalTypes as VkRect2D (Rect2D(..))
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Pipeline as VkPipeline
import Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends as Vk
import Vulkan.Zero as Vk

import Data.Linear
import Graphics.Shaders.Base
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Pipeline

drawFrame :: (MonadIO m, MonadLogger m)
  => CompiledPipeline a (V3 Float)
  -> Buffer (BufferFormat a)
  -> ShadersT m ()
drawFrame CompiledPipeline{..} vertexBuffer = do
  Frame{..} <- getCurrentFrame
  queueHandle <- getQueueHandle
  let SyncObjects{..} = frameSyncObjects
      commandBuffer = frameCommandBuffer

  -- Get the framebuffer for the next image in the swapchain by getting the
  -- swapchain image index.
  -- Ignore TIMEOUT and NOT_READY since we're not using a fence or semaphore
  -- and ignore SUBOPTIMAL_KHR.
  trace "Acquiring next image from swapchain"
  (_, framebuffer) <- getCurrentSwapChainImage

  trace "Submitting command buffer"
  -- Record and submit the command buffer.
  recordCommandBuffer commandBuffer framebuffer vertexBuffer

  let submitInfos = fmap Vk.SomeStruct . V.singleton $ Vk.zero {
    VkQueue.commandBuffers =
      fmap Vk.commandBufferHandle . V.fromList $ [ commandBuffer ],
    VkQueue.signalSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  VkQueue.queueSubmit queueHandle submitInfos syncInFlightFence

  return ()
 where
  recordCommandBuffer :: MonadIO m
    => Vk.CommandBuffer
    -> Vk.Framebuffer
    -> Buffer a
    -> ShadersT m ()
  recordCommandBuffer commandBuffer framebuffer Buffer{..} =
    -- Use Codensity to bracket command buffer recording and render pass.
    flip runCodensity return $ do
      extent <- lift getExtent
      renderPass <- lift getRenderPass
      Codensity $ VkCmd.useCommandBuffer commandBuffer Vk.zero . (&) ()
      let renderPassBeginInfo = Vk.zero {
        VkCmd.clearValues = V.fromList [
          VkCmd.Color (VkCmd.Float32 0 0 0 1)
        ],
        VkCmd.framebuffer = framebuffer,
        VkCmd.renderArea = Vk.zero {
          VkRect2D.extent = extent
        },
        VkCmd.renderPass = renderPass
      }
      Codensity $
        VkCmd.cmdUseRenderPass commandBuffer renderPassBeginInfo
          VkCmd.SUBPASS_CONTENTS_INLINE . (&) ()
      lift $ VkCmd.cmdBindPipeline commandBuffer
               VkEnum.PIPELINE_BIND_POINT_GRAPHICS pipelineHandle

      let viewport = Vk.zero {
        VkPipeline.height = fromIntegral . VkExtent2D.height $ extent,
        VkPipeline.width = fromIntegral . VkExtent2D.width $ extent,
        VkPipeline.maxDepth = 1
      }
      VkCmd.cmdSetViewport commandBuffer 0 . V.singleton $ viewport

      let scissor = Vk.zero {
        VkRect2D.extent = extent
      }
      VkCmd.cmdSetScissor commandBuffer 0 . V.singleton $ scissor

      VkCmd.cmdBindVertexBuffers commandBuffer 0
        (V.singleton bufferHandle) (V.singleton 0)

      VkCmd.cmdDraw commandBuffer bufferNumVertices 1 0 0
