module Graphics.Shaders.Buffer (
  VertexBuffer(..),
  B(..),

  Bufferable(..),
  ToBuffer(..),

  withVerticesAsBuffer
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Vector as V
import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import qualified Vulkan.Core10.Buffer as VkBuffer
import qualified Vulkan.Core10.CommandBuffer as VkCmdBuffer
import qualified Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.CommandBufferBuilding as VkCopy (BufferCopy(..))
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.FundamentalTypes as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Memory as VkMemory
import qualified Vulkan.Core10.MemoryManagement as VkManagement
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends
import qualified Vulkan.Zero as Vk

import Data.Bits.Extra
import Data.Linear
import Graphics.Shaders.Base
import Graphics.Shaders.Exception
import Graphics.Shaders.Logger.Class

data VertexBuffer a = VertexBuffer {
  vertexBufferHandle :: Vk.Buffer,
  vertexBufferNumVertices :: Word32
}

withVerticesAsBuffer :: forall a m. (MonadAsyncException m, MonadLogger m,
    Bufferable a)
  => [a]
  -> ShadersT m (VertexBuffer (BufferFormat a))
withVerticesAsBuffer vertices = do
  deviceHandle <- getDeviceHandle

  let ToBuffer (Kleisli calcStride) (Kleisli bufferer)
        = toBuffer :: ToBuffer a (BufferFormat a)

  let stride = flip execState 0 . calcStride $ (undefined :: a)
      numElems = length vertices
      bufferSize = stride * fromIntegral numElems

  -- Create a vertex buffer
  debug "Creating vertex buffer."
  let vertexBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_DST_BIT
        .|. VkBuffer.BUFFER_USAGE_VERTEX_BUFFER_BIT
      vertexBufferPropertyFlags = Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  (vertexBuffer, _) <- withBuffer bufferSize vertexBufferUsageFlags
    vertexBufferPropertyFlags

  -- Create a staging buffer
  -- TODO Clean up the staging buffer after it's used.
  debug "Creating staging buffer."
  let stagingBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
  (stagingBuffer, stagingBufferMemory) <- withBuffer bufferSize
    stagingBufferUsageFlags stagingMemoryPropertyFlags

  -- Fill the buffer
  ptr <-
    VkMemory.mapMemory deviceHandle stagingBufferMemory 0 bufferSize Vk.zero
  _ <- liftIO . flip runStateT ptr . mapM bufferer $ vertices
  VkMemory.unmapMemory deviceHandle stagingBufferMemory

  commandPool <- getCommandPool
  copyBuffer commandPool stagingBuffer vertexBuffer bufferSize

  return $ VertexBuffer {
      vertexBufferHandle = vertexBuffer,
      vertexBufferNumVertices = fromIntegral numElems
    }

withBuffer :: (MonadAsyncException m, MonadLogger m)
  => Vk.DeviceSize
  -> Vk.BufferUsageFlagBits
  -> Vk.MemoryPropertyFlags
  -> ShadersT m (Vk.Buffer, Vk.DeviceMemory)
withBuffer bufferSize bufferUsageFlags memoryPropertyFlags = do
  deviceHandle <- getDeviceHandle
  let bufferInfo = Vk.zero {
          VkBuffer.sharingMode = VkBuffer.SHARING_MODE_EXCLUSIVE,
          VkBuffer.size = bufferSize,
          VkBuffer.usage = bufferUsageFlags
        }
  buffer <- fromCps
    $ VkBuffer.withBuffer deviceHandle bufferInfo Nothing bracket

  -- Find compatible memory
  memoryRequirements <-
    VkManagement.getBufferMemoryRequirements deviceHandle buffer

  let getMemoryTypeIndexIfCompatible i t =
        if VkDevice.propertyFlags t .&&. memoryPropertyFlags
             && testBit (VkManagement.memoryTypeBits memoryRequirements) i
          then Just i
          else Nothing

  memoryProperties <- getDeviceMemoryProperties
  let memoryTypeIndices = V.imapMaybe getMemoryTypeIndexIfCompatible
        . VkDevice.memoryTypes
        $ memoryProperties

  when (V.null memoryTypeIndices) $ do
    let msg = "No suitable memory type found."
    err msg
    lift . throw . ShadersMemoryException $ msg

  let memoryTypeIndex = V.head memoryTypeIndices

  -- Allocate and bind buffer memory
  let allocInfo = Vk.zero {
          VkMemory.allocationSize = VkManagement.size memoryRequirements,
          VkMemory.memoryTypeIndex = fromIntegral memoryTypeIndex
        }
  memory <- fromCps
    $ VkMemory.withMemory deviceHandle allocInfo Nothing bracket

  VkManagement.bindBufferMemory deviceHandle buffer memory 0

  return (buffer, memory)

copyBuffer :: (MonadIO m, MonadLogger m)
  => Vk.CommandPool
  -> Vk.Buffer
  -> Vk.Buffer
  -> Vk.DeviceSize
  -> ShadersT m ()
copyBuffer commandPool src dest size = do
  deviceHandle <- getDeviceHandle
  queueHandle <- getQueueHandle
  let commandBufferCreateInfo = Vk.zero {
          VkCmdBuffer.commandBufferCount = 1,
          VkCmdBuffer.commandPool = commandPool,
          VkCmdBuffer.level = VkCmdBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
        }

  debug "Allocating temporary command buffer."
  cmdBuffers <- VkCmdBuffer.allocateCommandBuffers deviceHandle
                  commandBufferCreateInfo
  let cmdBuffer = V.head cmdBuffers

  let beginInfo = Vk.zero {
          VkCmdBuffer.flags = VkCmdBuffer.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        }
  VkCmdBuffer.beginCommandBuffer cmdBuffer beginInfo

  let bufferCopy = Vk.zero { VkCopy.size = size }
  VkCmd.cmdCopyBuffer cmdBuffer src dest (V.singleton bufferCopy)

  VkCmdBuffer.endCommandBuffer cmdBuffer

  let submitInfo = SomeStruct $ Vk.zero {
          VkQueue.commandBuffers = fmap Vk.commandBufferHandle . V.singleton
                                     $ cmdBuffer
        }

  VkQueue.queueSubmit queueHandle (V.singleton submitInfo) Vk.zero
  VkQueue.queueWaitIdle queueHandle

  debug "Destroying temporary command buffer."
  VkCmdBuffer.freeCommandBuffers deviceHandle commandPool cmdBuffers

class Bufferable a where
  type BufferFormat a
  toBuffer :: ToBuffer a (BufferFormat a)

data ToBuffer a b = ToBuffer
  -- Calculates stride
  (Kleisli (State Word64) a b)
  -- Buffers vertices
  (Kleisli (StateT (Ptr ()) IO) a b)

instance Category ToBuffer where
  id = ToBuffer id id
  (ToBuffer a b) . (ToBuffer a' b') = ToBuffer (a . a') (b . b')

instance Arrow ToBuffer where
  arr f = ToBuffer (arr f) (arr f)
  first (ToBuffer a b) = ToBuffer (first a) (first b)

-- Tightly packed 4 byte aligned data.
data B a = B {
  bOffset :: Word32, -- in bytes
  bStride :: Word64
} deriving (Show)

toBufferScalar :: forall a. Storable a => ToBuffer a (B a)
toBufferScalar = ToBuffer (Kleisli (const addStride)) (Kleisli doBuffer)
 where
  addStride = do
    let sz = sizeOf (undefined :: a)
    modify ((+) $ fromIntegral sz)
    return $ B {
        bOffset = fromIntegral sz,
        bStride = fromIntegral sz
      }

  doBuffer a = do
    ptr <- get
    put $ ptr `plusPtr` sizeOf a
    liftIO $ poke (castPtr ptr) a
    return $ B {
        bOffset = fromIntegral $ sizeOf a,
        bStride = fromIntegral $ sizeOf a
      }

instance Bufferable Float where
  type BufferFormat Float = B Float
  toBuffer = toBufferScalar

instance (Bufferable a, Bufferable b) => Bufferable (a, b) where
  type BufferFormat (a, b) = (BufferFormat a, BufferFormat b)
  toBuffer = proc ~(a, b) -> do
    a' <- toBuffer -< a
    b' <- toBuffer -< b
    returnA -< (a', b')

instance (Bufferable a, Bufferable b, Bufferable c)
    => Bufferable (a, b, c) where
  type BufferFormat (a, b, c)
          = (BufferFormat a, BufferFormat b, BufferFormat c)
  toBuffer = proc ~(a, b, c) -> do
    (a', b') <- toBuffer -< (a, b)
    c' <- toBuffer -< c
    returnA -< (a', b', c')

instance Bufferable (V2 Float) where
  type BufferFormat (V2 Float) = B (V2 Float)
  toBuffer = proc ~(V2 a b) -> do
    (a', _) <- toBuffer -< (a, b)
    returnA -< B {
      bOffset = bOffset a',
      bStride = 2 * bStride a'
    }

instance Bufferable (V3 Float) where
  type BufferFormat (V3 Float) = B (V3 Float)
  toBuffer = proc ~(V3 a b c) -> do
    (a', _, _) <- toBuffer -< (a, b, c)
    returnA -< B {
      bOffset = bOffset a',
      bStride = 3 * bStride a'
    }
