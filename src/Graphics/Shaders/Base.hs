module Graphics.Shaders.Base (
  HasVulkan(..),

  ShadersT(..),
  runShadersT,

  Frame(..),
  SyncObjects(..),
  SwapChain(..),

  swap,
  windowShouldClose,

  awaitIdle
) where

import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import Data.Word
import qualified Data.Vector as V
import Vulkan.Core10.AllocationCallbacks
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapChain
import Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Frames
import Graphics.Shaders.Internal.Window
import Graphics.Shaders.Logger.Base
import Graphics.Shaders.Orphans ()

newtype ShadersT m a = ShadersT {
  unShadersT :: ReaderT GraphicsEnv (StateT DrawState (ResourceT m)) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadAsyncException,
    MonadException, MonadFix, MonadResource)

instance MonadTrans ShadersT where
  lift = ShadersT . lift . lift . lift
  {-# INLINE lift #-}

instance (Monad m, MonadLogger m) => MonadLogger (ShadersT m) where
  loggerLevel = lift loggerLevel
  {-# INLINE loggerLevel #-}
  loggerLog lvl msg = lift $ loggerLog lvl msg
  {-# INLINE loggerLog #-}


data GraphicsEnv = GraphicsEnv {
  graphicsAllocator :: Maybe AllocationCallbacks,
  graphicsDevice :: Device,
  graphicsFrames :: Vector Frame,
  graphicsWindow :: Window
}

data DrawState = DrawState {
  drawStateFrameIndex :: Int,
  drawStateSwapChainImageIndex :: Word32
}

instance Monad m => HasWindow (ShadersT m) where
  getWindow = ShadersT $ asks graphicsWindow


class HasVulkan m where
  getAllocator :: m (Maybe AllocationCallbacks)
  getCommandPool :: m Vk.CommandPool
  getCurrentFrame :: m (Int, Frame, Vk.Framebuffer)
  getDevice :: m Vk.Device
  getDeviceMemoryProperties :: m VkDevice.PhysicalDeviceMemoryProperties
  getNumFrames :: m Int
  getQueue :: m Vk.Queue
  getRenderPass :: m Vk.RenderPass
  getSwapChain :: m VkSwapChain.SwapchainKHR
  getSwapChainExtent :: m Extent2D

instance Monad m => HasVulkan (ShadersT m) where
  getAllocator = ShadersT $ asks graphicsAllocator
  getCommandPool = ShadersT . asks $ deviceCommandPool . graphicsDevice
  getCurrentFrame = do
    SwapChain{..} <- ShadersT . asks $ deviceSwapChain . graphicsDevice
    frames <- ShadersT $ asks graphicsFrames
    frameNumber <- ShadersT $ gets drawStateFrameIndex
    swapChainImageIndex <- ShadersT $ gets drawStateSwapChainImageIndex
    let framebuffer =
          swapChainFramebuffers V.! fromIntegral swapChainImageIndex
    return (frameNumber, frames V.! frameNumber, framebuffer)
  getDevice = ShadersT . asks $ deviceHandle . graphicsDevice
  getDeviceMemoryProperties =
    ShadersT . asks $ deviceMemoryProperties . graphicsDevice
  getNumFrames = ShadersT . asks $ V.length . graphicsFrames
  getQueue = ShadersT . asks $ deviceQueueHandle . graphicsDevice
  getRenderPass = ShadersT . asks $ deviceRenderPass . graphicsDevice
  getSwapChain =
    ShadersT . asks $ swapChainHandle . deviceSwapChain . graphicsDevice
  getSwapChainExtent =
    ShadersT . asks $ swapChainExtent . deviceSwapChain . graphicsDevice


runShadersT :: (MonadUnliftIO m, MonadAsyncException m, MonadLogger m)
  => String
  -> ShadersT m a
  -> m a
runShadersT windowTitle m = runResourceT $ do
  let allocator = Nothing
  -- Initialise GLFW before Vulkan so extensions are available.
  window <- createWindow windowTitle
  vkInstance <- withInstance allocator
  surface <- createWindowSurface allocator window vkInstance
  device@Device{..} <- withDevice vkInstance allocator window surface
  frames <- createFrames allocator device

  -- Get the first swap chain image
  let SwapChain{..} = deviceSwapChain
  (_, nextImageIndex) <- VkSwapChain.acquireNextImageKHR deviceHandle
    swapChainHandle maxBound Vk.zero Vk.zero

  -- Create the environment and initial state
  let env = GraphicsEnv {
        graphicsAllocator = allocator,
        graphicsDevice = device,
        graphicsFrames = frames,
        graphicsWindow = window
      }
      drawState = DrawState {
        drawStateFrameIndex = 0,
        drawStateSwapChainImageIndex = nextImageIndex
      }
  flip evalStateT drawState . flip runReaderT env . unShadersT $ m


swap :: (MonadIO m, MonadLogger m) => ShadersT m ()
swap = do
  device <- getDevice
  queue <- getQueue
  swapChain <- getSwapChain
  numFrames <- getNumFrames

  (_, Frame{..}, _) <- getCurrentFrame
  let SyncObjects{..} = frameSyncObjects

  currentImageIndex <- ShadersT $ gets drawStateSwapChainImageIndex

  logTrace "Waiting for GPU to render current frame"
  -- Wait for the GPU to finish rendering the last frame.
  -- Ignore TIMEOUT since we're waiting so long anyway.
  let fences = V.singleton syncInFlightFence
  _ <- VkFence.waitForFences device fences True maxBound
  VkFence.resetFences device fences

  logTrace "Presenting image"
  -- Queue the presentation of the current swap chain image
  let presentInfo = Vk.zero {
    VkSwapChain.imageIndices = V.singleton currentImageIndex,
    VkSwapChain.swapchains = V.singleton swapChain,
    VkSwapChain.waitSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  _ <- VkSwapChain.queuePresentKHR queue presentInfo

  -- Acquire the next swap chain image
  -- Ignore TIMEOUT and NOT_READY since we're not using a fence or semaphore
  -- and ignore SUBOPTIMAL_KHR.
  logTrace "Acquiring next image from swapchain"
  (_, nextImageIndex) <- VkSwapChain.acquireNextImageKHR device swapChain
                           maxBound Vk.zero Vk.zero

  -- Increment the frame
  ShadersT $ modify (\drawState@DrawState{..} -> drawState {
      drawStateFrameIndex = (`mod` numFrames) . (+ 1) $ drawStateFrameIndex,
      drawStateSwapChainImageIndex = nextImageIndex
    })

  pollEvents

  return ()


awaitIdle :: (MonadIO m, HasVulkan m) => m ()
awaitIdle = do
  VkQueue.deviceWaitIdle =<< getDevice
