module Graphics.Shaders (
  module Graphics.Shaders.Class,
  GraphicsEnv,

  initialise,

  drawFrame,
  awaitIdle
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Queue as VkQueue
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as VkSwap
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.CommandBuffer
import Graphics.Shaders.Device
import Graphics.Shaders.Framebuffer
import Graphics.Shaders.Instance
import Graphics.Shaders.Pipeline
import Graphics.Shaders.Sync
import Graphics.Shaders.Window

data GraphicsEnv = GraphicsEnv {
    graphicsCommandBuffer :: Vk.CommandBuffer,
    graphicsDevice :: Device,
    graphicsFramebuffers :: Vector Vk.Framebuffer,
    graphicsPipeline :: Vk.Pipeline,
    graphicsRenderPass :: Vk.RenderPass,
    graphicsSyncObjects :: SyncObjects
  }

initialise :: (MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> Codensity m (Maybe GraphicsEnv)
initialise window = runMaybeT $ do
  vkInstance <- lift createInstance
  vkSurface <- lift $ createWindowSurface window vkInstance
  (device, queueFamilyIndex) <-
    MaybeT $ createDevice vkInstance window vkSurface
  renderPass <- lift $ createRenderPass device
  pipeline <- lift . createPipeline device $ renderPass
  framebuffers <- lift . createFramebuffers device $ renderPass
  commandBuffer <- lift $ createCommandBuffer device queueFamilyIndex
  syncObjects <- lift $ createSyncObjects device
  return $ GraphicsEnv {
      graphicsCommandBuffer = commandBuffer,
      graphicsDevice = device,
      graphicsFramebuffers = framebuffers,
      graphicsPipeline = pipeline,
      graphicsRenderPass = renderPass,
      graphicsSyncObjects = syncObjects
    }

drawFrame :: (MonadIO m, MonadLogger m) => GraphicsEnv -> m ()
drawFrame GraphicsEnv{..} = do
  let Device{..} = graphicsDevice
      SwapChain{..} = deviceSwapChain
      SyncObjects{..} = graphicsSyncObjects

  let fences = V.singleton syncInFlightFence

  trace "Waiting for GPU to render frame"
  -- Wait for the GPU to finish rendering the last frame.
  -- Ignore TIMEOUT since we're waiting so long anyway.
  _ <- VkFence.waitForFences deviceHandle fences True maxBound
  VkFence.resetFences deviceHandle fences

  -- Get the framebuffer for the next image in swapchain.
  -- Ignore TIMEOUT and NOT_READY since we're not using a fence and
  -- SUBOPTIMAL_KHR since it's only a warning.
  trace "Acquiring next image from swapchain"
  (_, nextImageIndex) <- VkSwap.acquireNextImageKHR deviceHandle
    swapChainHandle maxBound syncImageAvailableSemaphore Vk.zero
  let frameBuffer = graphicsFramebuffers V.! fromIntegral nextImageIndex

  trace "Submitting command buffer"
  -- Record and submit the command buffer.
  recordCommandBuffer graphicsDevice graphicsPipeline graphicsCommandBuffer
    graphicsRenderPass frameBuffer

  let submitInfos = fmap Vk.SomeStruct . V.singleton $ Vk.zero {
          VkQueue.commandBuffers = fmap Vk.commandBufferHandle . V.fromList
            $ [ graphicsCommandBuffer ],
          VkQueue.signalSemaphores = V.singleton syncRenderFinishedSemaphore,
          VkQueue.waitSemaphores = V.singleton syncImageAvailableSemaphore,
          VkQueue.waitDstStageMask =
            V.singleton VkQueue.PIPELINE_STAGE_TOP_OF_PIPE_BIT
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

awaitIdle :: MonadIO m => GraphicsEnv -> m ()
awaitIdle GraphicsEnv{..} =
  VkQueue.deviceWaitIdle (deviceHandle graphicsDevice)
