module Graphics.Shaders (
  ShadersT,
  runShadersT,

  awaitIdle
) where

import Control.Monad.IO.Class
import qualified Vulkan.Core10.Queue as VkQueue

import Graphics.Shaders.Base

awaitIdle :: (MonadIO m, MonadShaders m) => m ()
awaitIdle = do
  Device{..} <- getDevice
  VkQueue.deviceWaitIdle deviceHandle
