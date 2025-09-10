module Graphics.Shaders.Base (
  MonadShaders,

  ShadersT(..),
  runShadersT,

  awaitIdle,
  swap
) where

import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Swapchain
import Graphics.Shaders.Internal.Window
import Graphics.Shaders.Logger.Base
import Graphics.Shaders.Orphans ()

type MonadShaders m = (MonadIO m, MonadUnliftIO m, MonadAsyncException m,
  MonadLogger m, HasWindow m)

newtype ShadersT m a = ShadersT {
  unShadersT :: SwapchainStateT (DeviceReaderT (VulkanReaderT (ResourceT m))) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadAsyncException,
    MonadException, MonadFix, MonadResource, HasVulkan, HasVulkanDevice,
    HasSwapchain)

instance MonadTrans ShadersT where
  lift = ShadersT . lift . lift . lift . lift
  {-# INLINE lift #-}

instance MonadLogger m => MonadLogger (ShadersT m) where
instance HasWindow m => HasWindow (ShadersT m)


runShadersT :: MonadShaders m
  => ShadersT m a
  -> m a
runShadersT (ShadersT m) = runResourceT $ do
  let allocator = Nothing
  -- Initialise GLFW before Vulkan so extensions are available.
  vkInstance <- createInstance allocator
  debug "Creating surface..."

  runVulkanReaderT vkInstance allocator $ do
    withWindowSurface allocator vkInstance $ \surface -> do
      device <- createDevice surface

      flip runDeviceReaderT device $
        runSwapchainStateT surface m
