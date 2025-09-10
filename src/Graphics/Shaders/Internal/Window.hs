module Graphics.Shaders.Internal.Window (
  WindowEvents(..),
  GLFW.Key(..),
  GLFW.KeyState(..),
  GLFW.ModifierKeys(..),

  HasWindow(..),
  withWindowSurface,

  Window,

  GlfwWindowT(..),
  runGlfwWindowReaderT
) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Resource
import Data.IORef
import Foreign
import Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.CStruct
import Vulkan.Core10.AllocationCallbacks
import Vulkan.Core10.Enums
import qualified Vulkan.Core10.Handles as Vk
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_surface as VkSurface

import Graphics.Shaders.Exception
import Graphics.Shaders.Logger.Class

data WindowEvents = WindowEvents {
  windowCursorPosEvent :: Maybe (Float, Float),
  windowKeyEvents :: [(GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys)]
} deriving (Show)

emptyWindowEvents :: WindowEvents
emptyWindowEvents = WindowEvents Nothing []


class Monad m => HasWindow m where
  createWindowSurface :: Maybe AllocationCallbacks
    -> Vk.Instance
    -> m VkSurface.SurfaceKHR
  default createWindowSurface :: (t m' ~ m, HasWindow m',
      MonadTrans t)
    => Maybe AllocationCallbacks
    -> Vk.Instance
    -> m VkSurface.SurfaceKHR
  createWindowSurface allocator = lift . createWindowSurface allocator

  getWindowBufferSize :: m (Int, Int)
  default getWindowBufferSize :: (t m' ~ m, HasWindow m', MonadTrans t)
    => m (Int, Int)
  getWindowBufferSize = lift getWindowBufferSize

  pollWindowEvents :: m WindowEvents
  default pollWindowEvents :: (t m' ~ m, HasWindow m', MonadTrans t)
    => m WindowEvents
  pollWindowEvents = lift pollWindowEvents

  setCursorInputMode :: GLFW.CursorInputMode -> m ()
  default setCursorInputMode :: (t m' ~ m, HasWindow m', MonadTrans t)
    => GLFW.CursorInputMode -> m ()
  setCursorInputMode = lift . setCursorInputMode

  windowShouldClose :: m Bool
  default windowShouldClose :: (t m' ~ m, HasWindow m', MonadTrans t)
    => m Bool
  windowShouldClose = lift windowShouldClose


instance HasWindow m => HasWindow (ReaderT e m)
instance HasWindow m => HasWindow (ResourceT m)


withWindowSurface :: (MonadAsyncException m, HasWindow m)
  => Maybe AllocationCallbacks
  -> Vk.Instance
  -> (VkSurface.SurfaceKHR -> m r) -> m r
withWindowSurface allocator vkInstance =
  bracket
    (createWindowSurface allocator vkInstance)
    (\s -> VkSurface.destroySurfaceKHR vkInstance s allocator)


data GlfwWindowState = GlfwWindowState {
  glfwWindow :: Window,
  glfwWindowEvents :: IORef WindowEvents
}

newtype GlfwWindowT m a = GlfwWindowT {
  unGlfwWindowT :: ReaderT GlfwWindowState m a
} deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadException,
    MonadAsyncException, MonadUnliftIO, MonadLogger)


instance MonadIO m => HasWindow (GlfwWindowT m) where
  createWindowSurface allocator vkInstance = do
    window <- GlfwWindowT $ asks glfwWindow
    liftIO $ evalContT $ do
      let instancePtr = castPtr $ Vk.instanceHandle vkInstance
      allocatorPtr <- case allocator of
        Nothing -> pure nullPtr
        Just a -> ContT $ withCStruct a
      surfacePtr <- ContT alloca
      res <- fmap Result . liftIO
        . GLFW.createWindowSurface instancePtr window allocatorPtr
        $ surfacePtr
      -- Throw an exception on error the same way our Vulkan bindings do.
      when (res < SUCCESS) . liftIO . throw . VulkanException $ res
      liftIO $ peek surfacePtr

  getWindowBufferSize = GlfwWindowT $ do
    window <- asks glfwWindow
    liftIO $ GLFW.getFramebufferSize window

  pollWindowEvents = GlfwWindowT $ do
    liftIO GLFW.pollEvents
    eventsRef <- asks glfwWindowEvents
    events <- liftIO $ readIORef eventsRef
    liftIO $ writeIORef eventsRef emptyWindowEvents
    return events

  setCursorInputMode mode = GlfwWindowT $ do
    window <- asks glfwWindow
    liftIO . GLFW.setCursorInputMode window $ mode

  windowShouldClose = GlfwWindowT $ do
    window <- asks glfwWindow
    liftIO $ GLFW.windowShouldClose window


runGlfwWindowReaderT :: forall m a. (MonadAsyncException m)
  => String
  -> GlfwWindowT m a
  -> m a
runGlfwWindowReaderT title m = evalContT $ do
  window <- ContT $ bracket createWindow' destroyWindow
  windowEvents <- liftIO . newIORef $ emptyWindowEvents

  -- Bind event listeners
  -- Keys
  liftIO . GLFW.setKeyCallback window . Just $
    \_ key _ keyState modifierKeys ->
      modifyIORef windowEvents $
        \es -> es {
            windowKeyEvents =
              (key, keyState, modifierKeys) : windowKeyEvents es
          }
  -- Cursor
  liftIO . GLFW.setCursorPosCallback window . Just $
    \_ x y ->
      modifyIORef windowEvents $
        \es -> es {
            windowCursorPosEvent = Just (realToFrac x, realToFrac y)
          }

  let windowState = GlfwWindowState window windowEvents
  ContT . const . flip runReaderT windowState . unGlfwWindowT $ m
 where
  createWindow' :: m Window
  createWindow' = liftIO $ do
    initSuccess <- GLFW.init
    unless initSuccess $ do
      let msg = "GLFW failed to initialise."
      putStrLn msg
      throw $ ShadersInitializationException msg
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    mWin <- GLFW.createWindow 1920 1080 title Nothing Nothing
    case mWin of
      Just win -> return win
      Nothing  -> do
        let msg = "GLFW failed to create window."
        putStrLn msg
        throw $ ShadersInitializationException msg

  destroyWindow :: Window -> m ()
  destroyWindow win = liftIO $ do
    GLFW.destroyWindow win
    GLFW.terminate
