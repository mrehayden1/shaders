module Graphics.Shaders.Base (
  ShadersT,
  runShadersT,
  fromCps,

  Frame(..),
  SyncObjects(..),

  getCommandPool,
  getCurrentFrame,
  getCurrentSwapChainImage,
  getDeviceHandle,
  getDeviceMemoryProperties,
  getExtent,
  getQueueHandle,
  getRenderPass,
  getSwapChainHandle,

  swap,

  awaitIdle
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Vector (Vector)
import Data.Word
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Core10.CommandBuffer as VkBuffer
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.Extensions.VK_KHR_swapchain as VkSwap
import Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Sync
import Graphics.Shaders.Internal.Window
import Graphics.Shaders.Logger.Base

framesInFlight :: Int
framesInFlight = 2

newtype ShadersT m a = ShadersT {
  unShadersT :: ReaderT GraphicsEnv (StateT DrawState (Codensity m)) a
} deriving (Functor, Applicative, Monad, MonadIO)

data GraphicsEnv = GraphicsEnv {
  graphicsDevice :: Device,
  graphicsFrames :: Vector Frame
}

data Frame = Frame {
  frameCommandBuffer :: Vk.CommandBuffer,
  frameSyncObjects :: SyncObjects
}

data DrawState = DrawState {
  drawStateFrameIndex :: Int,
  drawStateSwapImage :: (Word32, Vk.Framebuffer)
}

fromCps :: (forall b. (a -> m b) -> m b) -> ShadersT m a
fromCps cps = ShadersT . lift . lift $ Codensity cps

instance MonadTrans ShadersT where
  lift = ShadersT . lift . lift . lift
  {-# INLINE lift #-}

instance (Monad m, MonadLogger m) => MonadLogger (ShadersT m) where
  loggerLevel = lift loggerLevel
  {-# INLINE loggerLevel #-}
  loggerLog lvl msg = lift $ loggerLog lvl msg
  {-# INLINE loggerLog #-}

runShadersT :: (MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> ShadersT m a
  -> Codensity m a
runShadersT window m = do
  vkInstance <- withInstance
  surface <- createWindowSurface window vkInstance
  device@Device{..} <- withDevice vkInstance window surface
  commandBuffers <- withCommandBuffers device framesInFlight
  syncObjects <- withSyncObjects device framesInFlight

  -- Get the first swap chain image
  let SwapChain{..} = deviceSwapChain
  (_, nextImageIndex) <- VkSwap.acquireNextImageKHR deviceHandle
    swapChainHandle maxBound Vk.zero Vk.zero
  let framebuffer = swapChainFramebuffers V.! fromIntegral nextImageIndex

  -- Create the environment and initial state
  let env = GraphicsEnv {
        graphicsDevice = device,
        graphicsFrames = V.zipWith Frame commandBuffers syncObjects
      }
      drawState = DrawState {
        drawStateFrameIndex = 0,
        drawStateSwapImage = (nextImageIndex, framebuffer)
      }
  flip evalStateT drawState . flip runReaderT env . unShadersT $ m
 where
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

getCommandPool :: ShadersT m Vk.CommandPool
getCommandPool = ShadersT $ asks (deviceCommandPool . graphicsDevice)
{-# INLINE getCommandPool #-}

getCurrentFrame :: ShadersT m Frame
getCurrentFrame = ShadersT $ do
  frames <- asks graphicsFrames
  frameNumber <- gets drawStateFrameIndex
  return $ frames V.! frameNumber

getCurrentSwapChainImage :: ShadersT m (Word32, Vk.Framebuffer)
getCurrentSwapChainImage = ShadersT $ gets drawStateSwapImage
{-# INLINE getCurrentSwapChainImage #-}

getDeviceHandle :: ShadersT m Vk.Device
getDeviceHandle = ShadersT $ asks (deviceHandle . graphicsDevice)
{-# INLINE getDeviceHandle #-}

getDeviceMemoryProperties :: ShadersT m VkDevice.PhysicalDeviceMemoryProperties
getDeviceMemoryProperties = ShadersT $
  asks (deviceMemoryProperties . graphicsDevice)
{-# INLINE getDeviceMemoryProperties #-}

getExtent :: ShadersT m VkExtent2D.Extent2D
getExtent = ShadersT $
  asks (swapChainExtent . deviceSwapChain . graphicsDevice)
{-# INLINE getExtent #-}

getQueueHandle :: ShadersT m Vk.Queue
getQueueHandle = ShadersT $ asks (deviceQueueHandle . graphicsDevice)
{-# INLINE getQueueHandle #-}

getRenderPass :: ShadersT m Vk.RenderPass
getRenderPass = ShadersT $ asks (deviceRenderPass . graphicsDevice)
{-# INLINE getRenderPass #-}

getSwapChainHandle :: ShadersT m VkSwap.SwapchainKHR
getSwapChainHandle = ShadersT $
  asks (swapChainHandle . deviceSwapChain . graphicsDevice)

swap :: (MonadIO m, MonadLogger m) => ShadersT m ()
swap = do
  Device{..} <- ShadersT $ asks graphicsDevice
  let SwapChain{..} = deviceSwapChain
  Frame{..} <- getCurrentFrame
  let SyncObjects{..} = frameSyncObjects
  (currentImageIndex, _) <- getCurrentSwapChainImage

  trace "Waiting for GPU to render current frame"
  -- Wait for the GPU to finish rendering the last frame.
  -- Ignore TIMEOUT since we're waiting so long anyway.
  let fences = V.singleton syncInFlightFence
  _ <- VkFence.waitForFences deviceHandle fences True maxBound
  VkFence.resetFences deviceHandle fences

  -- Queue the presentation of the current swap chain image
  let presentInfo = Vk.zero {
    VkSwap.imageIndices = V.singleton currentImageIndex,
    VkSwap.swapchains = V.singleton swapChainHandle,
    VkSwap.waitSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  trace "Presenting image"
  _ <- VkSwap.queuePresentKHR deviceQueueHandle presentInfo

  -- Increment the frame and swap chain image
  frames <- ShadersT $ asks graphicsFrames
  (_, nextImageIndex) <- VkSwap.acquireNextImageKHR deviceHandle
    swapChainHandle maxBound Vk.zero Vk.zero
  let framebuffer = swapChainFramebuffers V.! fromIntegral nextImageIndex

  ShadersT $ modify (\drawState@DrawState{..} -> drawState {
      drawStateFrameIndex =
        (`mod` V.length frames) . (+ 1) $ drawStateFrameIndex,
      drawStateSwapImage = (nextImageIndex, framebuffer)
    })
  return ()

awaitIdle :: MonadIO m => ShadersT m ()
awaitIdle = do
  VkQueue.deviceWaitIdle =<< getDeviceHandle
