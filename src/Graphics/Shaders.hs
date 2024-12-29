module Graphics.Shaders (
  module Graphics.Shaders.Class,
  GraphicsEnv,

  initialise,

  drawFrame,
  awaitIdle
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import Linear hiding (trace)
import qualified Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Queue as VkQueue
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as VkSwap
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Buffer
import Graphics.Shaders.Class
import Graphics.Shaders.CommandBuffer
import Graphics.Shaders.Device
import Graphics.Shaders.Framebuffer
import Graphics.Shaders.Instance
import Graphics.Shaders.Pipeline
import Graphics.Shaders.Sync
import Graphics.Shaders.Window

framesInFlight :: Int
framesInFlight = 2

data GraphicsEnv = GraphicsEnv {
    -- One command buffer per frame in flight
    graphicsCommandBuffers :: Vector Vk.CommandBuffer,
    graphicsDevice :: Device,
    -- One framebuffer per image in the swapchain
    graphicsFramebuffers :: Vector Vk.Framebuffer,
    graphicsPipeline :: Vk.Pipeline,
    graphicsRenderPass :: Vk.RenderPass,
    -- One `SyncObject` per frame in flight
    graphicsSyncObjects :: Vector SyncObjects,
    graphicsVertexBuffer :: VertexBuffer (V2 Float, V3 Float)
  }

type GraphicsState = Int

initialise :: (MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> Codensity m (Maybe GraphicsEnv)
initialise window = runMaybeT $ do
  vkInstance <- lift createInstance
  vkSurface <- lift $ createWindowSurface window vkInstance
  (device, queueFamilyIndex) <-
    MaybeT $ createDevice vkInstance window vkSurface
  renderPass <- lift $ createRenderPass device
  vertexBuffer <- MaybeT $ toVertexBuffer device vertexData
  pipeline <- lift . createPipeline device renderPass $ vertexBuffer
  framebuffers <- lift . createFramebuffers device $ renderPass
  commandBuffers <- fmap V.fromList . lift . replicateM framesInFlight
    $ createCommandBuffer device queueFamilyIndex
  syncObjects <- fmap V.fromList . lift . replicateM framesInFlight
    $ createSyncObjects device
  return $ GraphicsEnv {
      graphicsCommandBuffers = commandBuffers,
      graphicsDevice = device,
      graphicsFramebuffers = framebuffers,
      graphicsPipeline = pipeline,
      graphicsRenderPass = renderPass,
      graphicsSyncObjects = syncObjects,
      graphicsVertexBuffer = vertexBuffer
    }

drawFrame :: (MonadIO m, MonadLogger m, MonadState GraphicsState m)
  => GraphicsEnv
  -> m ()
drawFrame GraphicsEnv{..} = do
  modify ((`mod` framesInFlight) . (+ 1))
  frameNumber <- get

  let Device{..} = graphicsDevice
      SwapChain{..} = deviceSwapChain
      SyncObjects{..} = graphicsSyncObjects V.! frameNumber
      commandBuffer = graphicsCommandBuffers V.! frameNumber

  trace "Waiting for GPU to render current frame"
  -- Wait for the GPU to finish rendering the last frame.
  -- Ignore TIMEOUT since we're waiting so long anyway.
  let fences = V.singleton syncInFlightFence
  _ <- VkFence.waitForFences deviceHandle fences True maxBound
  VkFence.resetFences deviceHandle fences

  -- Get the framebuffer for the next image in the swapchain by getting the
  -- swapchain image index.
  -- Ignore TIMEOUT (as above), also NOT_READY since we're not using a fence
  -- and also SUBOPTIMAL_KHR.
  trace "Acquiring next image from swapchain"
  (_, nextImageIndex) <- VkSwap.acquireNextImageKHR deviceHandle
    swapChainHandle maxBound syncImageAvailableSemaphore Vk.zero
  let frameBuffer = graphicsFramebuffers V.! fromIntegral nextImageIndex

  trace "Submitting command buffer"
  -- Record and submit the command buffer.
  recordCommandBuffer graphicsDevice graphicsPipeline commandBuffer
    graphicsRenderPass frameBuffer graphicsVertexBuffer

  let submitInfos = fmap Vk.SomeStruct . V.singleton $ Vk.zero {
          VkQueue.commandBuffers = fmap Vk.commandBufferHandle . V.fromList
            $ [ commandBuffer ],
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
