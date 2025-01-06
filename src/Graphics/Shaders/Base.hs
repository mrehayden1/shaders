module Graphics.Shaders.Base (
  module Graphics.Shaders.Class,
  ShadersT,

  runShadersT,

  fromCps
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Shaders.Class
import Graphics.Shaders.CommandBuffer
import Graphics.Shaders.Initialization.Instance
import Graphics.Shaders.Initialization.Device
import Graphics.Shaders.Initialization.Sync
import Graphics.Shaders.Initialization.Window
import Graphics.Shaders.Logger.Class

framesInFlight :: Int
framesInFlight = 2 -- Double buffering

data GraphicsEnv = GraphicsEnv {
    graphicsDevice :: Device,
    graphicsFrames :: Vector Frame
  }

type DrawState = Int

newtype ShadersT m a = ShadersT {
  unShadersT :: ReaderT GraphicsEnv (StateT DrawState (Codensity m)) a
} deriving (Functor, Applicative, Monad, MonadIO)

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

instance MonadShaders (ShadersT m) where
  getDevice = ShadersT $ asks graphicsDevice
  {-# INLINE getDevice #-}
  getNextFrame = ShadersT $ do
    frames <- asks graphicsFrames
    frameNumber <- get
    modify ((`mod` V.length frames) . (+ 1))
    return $ frames V.! frameNumber
  {-# INLINE getNextFrame #-}

runShadersT :: (MonadAsyncException m, MonadLogger m)
  => GLFW.Window
  -> ShadersT m a
  -> Codensity m a
runShadersT window m = do
  vkInstance <- withInstance
  surface <- createWindowSurface window vkInstance
  device <- withDevice vkInstance window surface
  commandBuffers <- withCommandBuffers device framesInFlight
  syncObjects <- withSyncObjects device framesInFlight
  let env = GraphicsEnv {
      graphicsDevice = device,
      graphicsFrames = V.zipWith Frame commandBuffers syncObjects
    }
  flip evalStateT 0 . flip runReaderT env . unShadersT $ m
