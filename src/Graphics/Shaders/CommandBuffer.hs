module Graphics.Shaders.CommandBuffer (
  createCommandBuffer,

  recordCommandBuffer
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Function
import Data.Vector as V
import Data.Word
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.CommandBuffer as VkBuffer
import qualified Vulkan.Core10.CommandBufferBuilding as VkClear (
  ClearValue(..), ClearColorValue(..))
import qualified Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.CommandPool as VkPool
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.FundamentalTypes as VkRect2D (Rect2D(..))
import qualified Vulkan.Core10.Pipeline as VkPipeline
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Device

createCommandPool :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Word32
  -> Codensity m Vk.CommandPool
createCommandPool Device{..} queueFamilyIndex = do
  let poolCreateInfo = Vk.zero {
          VkPool.flags = VkPool.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
          VkPool.queueFamilyIndex = queueFamilyIndex
        }

  debug "Creating command pool."
  Codensity $ bracket
    (VkPool.createCommandPool deviceHandle poolCreateInfo Nothing)
    (\pool -> do
       debug "Destroying command pool."
       VkPool.destroyCommandPool deviceHandle pool Nothing)

createCommandBuffer :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Word32
  -> Codensity m Vk.CommandBuffer
createCommandBuffer device@Device{..} queueFamilyIndex = do
  commandPool <- createCommandPool device queueFamilyIndex

  let commandBufferCreateInfo = Vk.zero {
          VkBuffer.commandBufferCount = 1,
          VkBuffer.commandPool = commandPool,
          VkBuffer.level = VkBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
        }

  debug "Allocating command buffer."
  fmap V.head . lift
    $ VkBuffer.allocateCommandBuffers deviceHandle commandBufferCreateInfo

recordCommandBuffer :: MonadIO m
  => Device
  -> Vk.Pipeline
  -> Vk.CommandBuffer
  -> Vk.RenderPass
  -> Vk.Framebuffer
  -> m ()
recordCommandBuffer Device{..} pipeline commandBuffer renderPass framebuffer =
  lowerCodensity $ do
    Codensity $ VkBuffer.useCommandBuffer commandBuffer Vk.zero . (&) ()
    let renderPassBeginInfo = Vk.zero {
            VkCmd.clearValues = V.fromList [
                VkClear.Color (VkClear.Float32 0 0 0 1)
              ],
            VkCmd.renderArea = Vk.zero {
              VkRect2D.extent = swapChainExtent deviceSwapChain
            },
            VkCmd.framebuffer = framebuffer,
            VkCmd.renderPass = renderPass
          }
    Codensity $
      VkCmd.cmdUseRenderPass commandBuffer renderPassBeginInfo
        VkCmd.SUBPASS_CONTENTS_INLINE . (&) ()
    lift $ VkCmd.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS
             pipeline

    let SwapChain{..} = deviceSwapChain

    let viewport = Vk.zero {
            VkPipeline.height = fromIntegral . VkExtent2D.height
                                  $ swapChainExtent,
            VkPipeline.width = fromIntegral . VkExtent2D.width
                                  $ swapChainExtent,
            VkPipeline.maxDepth = 1
          }
    Vk.cmdSetViewport commandBuffer 0 . V.singleton $ viewport

    let scissor = Vk.zero {
            VkRect2D.extent = swapChainExtent
          }
    VkCmd.cmdSetScissor commandBuffer 0 . V.singleton $ scissor

    VkCmd.cmdDraw commandBuffer 3 1 0 0
