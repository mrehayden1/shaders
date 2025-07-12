module Graphics.Shaders.Base (
  ShadersT(..),
  GraphicsEnv(..),
  DrawState(..),
  runShadersT,

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

import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
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
import Graphics.Shaders.Orphans ()

framesInFlight :: Int
framesInFlight = 2

newtype ShadersT m a = ShadersT {
  unShadersT :: ReaderT GraphicsEnv (StateT DrawState (ResourceT m)) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadAsyncException,
    MonadException, MonadFix, MonadReader GraphicsEnv, MonadResource,
    MonadState DrawState)

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

instance MonadTrans ShadersT where
  lift = ShadersT . lift . lift . lift
  {-# INLINE lift #-}

instance (Monad m, MonadLogger m) => MonadLogger (ShadersT m) where
  loggerLevel = lift loggerLevel
  {-# INLINE loggerLevel #-}
  loggerLog lvl msg = lift $ loggerLog lvl msg
  {-# INLINE loggerLog #-}

runShadersT :: (MonadUnliftIO m, MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> ShadersT m a
  -> m a
runShadersT window m = runResourceT $ do
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
  withCommandBuffers :: (MonadAsyncException m, MonadLogger m, MonadResource m)
    => Device
    -> Int
    -> m (Vector Vk.CommandBuffer)
  withCommandBuffers Device{..} n = do
    let commandBufferCreateInfo = Vk.zero {
      VkBuffer.commandBufferCount = fromIntegral n,
      VkBuffer.commandPool = deviceCommandPool,
      VkBuffer.level = VkBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
    }
    debug "Allocating command buffer."
    snd <$> allocate
      (VkBuffer.allocateCommandBuffers deviceHandle commandBufferCreateInfo)
      (\buffers -> do
         VkBuffer.freeCommandBuffers deviceHandle deviceCommandPool buffers)

getCommandPool :: (MonadReader GraphicsEnv m) => m Vk.CommandPool
getCommandPool = asks (deviceCommandPool . graphicsDevice)
{-# INLINE getCommandPool #-}

getCurrentFrame :: (MonadReader GraphicsEnv m, MonadState DrawState m)
  => m Frame
getCurrentFrame = do
  frames <- asks graphicsFrames
  frameNumber <- gets drawStateFrameIndex
  return $ frames V.! frameNumber

getCurrentSwapChainImage :: MonadState DrawState m => m (Word32, Vk.Framebuffer)
getCurrentSwapChainImage = gets drawStateSwapImage
{-# INLINE getCurrentSwapChainImage #-}

getDeviceHandle :: MonadReader GraphicsEnv m => m Vk.Device
getDeviceHandle = asks (deviceHandle . graphicsDevice)
{-# INLINE getDeviceHandle #-}

getDeviceMemoryProperties :: MonadReader GraphicsEnv m
  => m VkDevice.PhysicalDeviceMemoryProperties
getDeviceMemoryProperties = asks (deviceMemoryProperties . graphicsDevice)
{-# INLINE getDeviceMemoryProperties #-}

getExtent :: MonadReader GraphicsEnv m => m VkExtent2D.Extent2D
getExtent = asks (swapChainExtent . deviceSwapChain . graphicsDevice)
{-# INLINE getExtent #-}

getQueueHandle :: MonadReader GraphicsEnv m => m Vk.Queue
getQueueHandle = asks (deviceQueueHandle . graphicsDevice)
{-# INLINE getQueueHandle #-}

getRenderPass :: MonadReader GraphicsEnv m => m Vk.RenderPass
getRenderPass = asks (deviceRenderPass . graphicsDevice)
{-# INLINE getRenderPass #-}

getSwapChainHandle :: MonadReader GraphicsEnv m => m VkSwap.SwapchainKHR
getSwapChainHandle =
  asks (swapChainHandle . deviceSwapChain . graphicsDevice)

swap :: (MonadIO m, MonadLogger m) => ShadersT m ()
swap = do
  Device{..} <- ShadersT $ asks graphicsDevice
  let SwapChain{..} = deviceSwapChain
  Frame{..} <- getCurrentFrame
  let SyncObjects{..} = frameSyncObjects
  (currentImageIndex, _) <- getCurrentSwapChainImage

  logTrace "Waiting for GPU to render current frame"
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
  logTrace "Presenting image"
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
