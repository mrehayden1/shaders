module Graphics.Shaders.Class (
  MonadShaders(..),

  Device(..),
  SwapChain(..),

  Frame(..),
  SyncObjects(..)
) where

import Control.Monad.Trans
import qualified Vulkan.Core10.Handles as Vk

import Graphics.Shaders.Initialization.Device
import Graphics.Shaders.Initialization.Sync
import Graphics.Shaders.Logger.Base

class Monad m => MonadShaders m where
  getDevice :: m Device
  getNextFrame :: m Frame

instance MonadShaders m => MonadShaders (LoggerT m) where
  getDevice = lift getDevice
  {-# INLINE getDevice #-}
  getNextFrame = lift getNextFrame
  {-# INLINE getNextFrame #-}

data Frame = Frame {
    frameCommandBuffer :: Vk.CommandBuffer,
    frameSyncObjects :: SyncObjects
  }
