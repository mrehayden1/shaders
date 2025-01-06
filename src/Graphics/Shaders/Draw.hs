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
import Vulkan.Core10.Fence as VkFence
import Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import Vulkan.Core10.FundamentalTypes as VkRect2D (Rect2D(..))
import qualified Vulkan.Core10.Handles as Vk
import Vulkan.Core10.Pipeline as VkPipeline
import Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends as Vk
import Vulkan.Extensions.VK_KHR_swapchain as VkSwap
import Vulkan.Zero as Vk

import Graphics.Shaders.Base
import Graphics.Shaders.Buffer
import Graphics.Shaders.Logger.Class

drawFrame :: (MonadIO m, MonadLogger m)
  => Vk.Pipeline
  -> VertexBuffer a
  -> ShadersT m ()
drawFrame pipeline vertexBuffer = do
  Frame{..} <- getNextFrame

  Device{..} <- getDevice
  let SwapChain{..} = deviceSwapChain
      SyncObjects{..} = frameSyncObjects
      commandBuffer = frameCommandBuffer

  trace "Waiting for GPU to render current frame"
  -- Wait for the GPU to finish rendering the last frame.
  -- Ignore TIMEOUT since we're waiting so long anyway.
  let fences = V.singleton syncInFlightFence
  _ <- VkFence.waitForFences deviceHandle fences True maxBound
  VkFence.resetFences deviceHandle fences

  -- Get the framebuffer for the next image in the swapchain by getting the
  -- swapchain image index.
  -- Ignore TIMEOUT and NOT_READY since we're not using a fence or semaphore
  -- and ignore SUBOPTIMAL_KHR.
  trace "Acquiring next image from swapchain"
  (_, nextImageIndex) <- VkSwap.acquireNextImageKHR deviceHandle
    swapChainHandle maxBound Vk.zero Vk.zero
  let frameBuffer = swapChainFramebuffers V.! fromIntegral nextImageIndex

  trace "Submitting command buffer"
  -- Record and submit the command buffer.
  recordCommandBuffer commandBuffer frameBuffer vertexBuffer

  let submitInfos = fmap Vk.SomeStruct . V.singleton $ Vk.zero {
    VkQueue.commandBuffers =
      fmap Vk.commandBufferHandle . V.fromList $ [ commandBuffer ],
    VkQueue.signalSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  VkQueue.queueSubmit deviceQueueHandle submitInfos syncInFlightFence

  trace "Presenting image"
  let presentInfo = Vk.zero {
    VkSwap.imageIndices = V.singleton nextImageIndex,
    VkSwap.swapchains = V.singleton swapChainHandle,
    VkSwap.waitSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  _ <- VkSwap.queuePresentKHR deviceQueueHandle presentInfo

  return ()
 where
  recordCommandBuffer :: (MonadIO m, MonadShaders m)
    => Vk.CommandBuffer
    -> Vk.Framebuffer
    -> VertexBuffer a
    -> m ()
  recordCommandBuffer commandBuffer framebuffer
      VertexBuffer{..} =
    -- Use Codensity to bracket command buffer recording and render pass.
    flip runCodensity return $ do
      Device{..} <- lift getDevice
      Codensity $ VkCmd.useCommandBuffer commandBuffer Vk.zero . (&) ()
      let renderPassBeginInfo = Vk.zero {
        VkCmd.clearValues = V.fromList [
          VkCmd.Color (VkCmd.Float32 0 0 0 1)
        ],
        VkCmd.framebuffer = framebuffer,
        VkCmd.renderArea = Vk.zero {
          VkRect2D.extent = swapChainExtent deviceSwapChain
        },
        VkCmd.renderPass = deviceRenderPass
      }
      Codensity $
        VkCmd.cmdUseRenderPass commandBuffer renderPassBeginInfo
          VkCmd.SUBPASS_CONTENTS_INLINE . (&) ()
      lift $ VkCmd.cmdBindPipeline commandBuffer
               VkEnum.PIPELINE_BIND_POINT_GRAPHICS pipeline
      VkCmd.cmdBindVertexBuffers commandBuffer 0
        (V.singleton vertexBufferHandle) (V.singleton 0)

      let SwapChain{..} = deviceSwapChain

      let viewport = Vk.zero {
        VkPipeline.height = fromIntegral . VkExtent2D.height $ swapChainExtent,
        VkPipeline.width = fromIntegral . VkExtent2D.width $ swapChainExtent,
        VkPipeline.maxDepth = 1
      }
      VkCmd.cmdSetViewport commandBuffer 0 . V.singleton $ viewport

      let scissor = Vk.zero {
        VkRect2D.extent = swapChainExtent
      }
      VkCmd.cmdSetScissor commandBuffer 0 . V.singleton $ scissor

      VkCmd.cmdDraw commandBuffer vertexBufferNumVertices 1 0 0
