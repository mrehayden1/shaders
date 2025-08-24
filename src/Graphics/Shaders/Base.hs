module Graphics.Shaders.Base (
  MonadShaders,

  HasVulkan(..),

  ShadersT(..),
  runShadersT,

  awaitIdle
) where

import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Vulkan.Core10.AllocationCallbacks
import Vulkan.Core10.Fence as VkFence
import qualified Vulkan.Core10.Handles as Vk
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapChain
import Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Frames
import Graphics.Shaders.Internal.Window
import Graphics.Shaders.Logger.Base
import Graphics.Shaders.Orphans ()


type MonadShaders m = (MonadAsyncException m, MonadIO m, MonadResource m,
  HasVulkan m, HasVulkanDevice m, HasSwapchain m)

newtype ShadersT m a = ShadersT {
  unShadersT :: ReaderT GraphicsEnv (StateT DrawState (ResourceT m)) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadAsyncException,
    MonadException, MonadFix, MonadResource)

instance MonadTrans ShadersT where
  lift = ShadersT . lift . lift . lift
  {-# INLINE lift #-}

instance MonadLogger m => MonadLogger (ShadersT m) where
instance HasWindow m => HasWindow (ShadersT m)


data GraphicsEnv = GraphicsEnv {
  graphicsAllocator :: Maybe AllocationCallbacks,
  graphicsDevice :: Device,
  graphicsFrames :: Vector Frame,
  graphicsVkInstance :: Vk.Instance
}

data DrawState = DrawState {
  drawStateFrameIndex :: Int,
  drawStateSwapChainImageIndex :: Word32
}

instance Monad m => HasVulkan (ShadersT m) where
  getVulkanAllocator = ShadersT $ asks graphicsAllocator
  getVulkanInstance = ShadersT $ asks graphicsVkInstance

instance Monad m => HasVulkanDevice (ShadersT m) where
  getCommandPool = ShadersT . asks $ deviceCommandPool . graphicsDevice
  getDevice = ShadersT . asks $ deviceHandle . graphicsDevice
  getDeviceMemoryProperties =
    ShadersT . asks $ deviceMemoryProperties . graphicsDevice
  getDeviceQueue = ShadersT . asks $ deviceQueueHandle . graphicsDevice

instance (MonadIO m, MonadLogger m) => HasSwapchain (ShadersT m) where
  getCurrentFrame = do
    SwapChain{..} <- ShadersT . asks $ deviceSwapChain . graphicsDevice
    frames <- ShadersT $ asks graphicsFrames
    frameNumber <- ShadersT $ gets drawStateFrameIndex
    swapChainImageIndex <- ShadersT $ gets drawStateSwapChainImageIndex
    let framebuffer =
          swapChainFramebuffers V.! fromIntegral swapChainImageIndex
    return (frameNumber, frames V.! frameNumber, framebuffer)
  getNumFrames = ShadersT . asks $ V.length . graphicsFrames
  getRenderPass = ShadersT . asks $ deviceRenderPass . graphicsDevice
  getSwapChain =
    ShadersT . asks $ swapChainHandle . deviceSwapChain . graphicsDevice
  getSwapChainExtent =
    ShadersT . asks $ swapChainExtent . deviceSwapChain . graphicsDevice
  swap = do
    device <- getDevice
    queue <- getDeviceQueue
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


runShadersT :: (MonadUnliftIO m, MonadAsyncException m, MonadLogger m,
    HasWindow m)
  => ShadersT m a
  -> m a
runShadersT m = runResourceT $ do
  let allocator = Nothing
  -- Initialise GLFW before Vulkan so extensions are available.
  vkInstance <- withInstance allocator
  debug "Creating surface..."
  -- TODO Flatten this out with ContT?
  withWindowSurface allocator vkInstance $ \surface -> do
    device@Device{..} <- withDevice vkInstance surface allocator
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
          graphicsVkInstance = vkInstance
        }
        drawState = DrawState {
          drawStateFrameIndex = 0,
          drawStateSwapChainImageIndex = nextImageIndex
        }
    flip evalStateT drawState . flip runReaderT env . unShadersT $ m
