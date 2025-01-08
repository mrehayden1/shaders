module Graphics.Shaders.Buffer (
  vertexData,

  VertexBuffer(..),
  Bufferable,

  withVerticesAsBuffer
) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.Trans
import qualified Data.Vector as V
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Linear
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

import Graphics.Shaders
import Graphics.Shaders.Exception
import Graphics.Shaders.Logger.Class
import Util.Bits

vertexData :: [(V2 Float, V3 Float)]
vertexData = [
    (V2   0.0  (-0.5), V3 1.0 0.0 0.0),
    (V2 (-0.5)   0.5 , V3 0.0 0.0 1.0),
    (V2   0.5    0.5 , V3 0.0 1.0 0.0)
  ]

data VertexBuffer a = VertexBuffer {
  vertexBufferHandle :: Vk.Buffer,
  vertexBufferNumVertices :: Word32
}

withVerticesAsBuffer :: forall a m. (Storable (Vertex a),
    MonadAsyncException m, MonadLogger m)
  => [a]
  -> ShadersT m (VertexBuffer a)
withVerticesAsBuffer vertices = do
  deviceHandle <- getDeviceHandle
  let stride = fromIntegral . sizeOf $ (undefined :: Vertex a)
      numElems = fromIntegral . length $ vertices
      bufferSize = stride * numElems

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
  liftIO $ pokeArray (castPtr ptr) . fmap Vertex $ vertices
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

-- Represents tightly packed 4 byte aligned types.
class Bufferable a

instance Bufferable Float
instance Bufferable (V2 Float)
instance Bufferable (V3 Float)
instance Bufferable (V4 Float)
instance (Bufferable a, Bufferable b) => Bufferable (a, b)
instance (Bufferable a, Bufferable b, Bufferable c) => Bufferable (a, b, c)

-- Type wrapper for marshalling vertices into buffers.
newtype Vertex a = Vertex {
  unVertex :: a
} deriving (Show)

instance (Storable a, Storable b, Bufferable a, Bufferable b)
    => Storable (Vertex (a, b)) where
  alignment _ = 4
  peek ptr = do
    a <- peek (castPtr ptr)
    b <- peekByteOff ptr (sizeOf a)
    return $ Vertex (a, b)
  poke ptr (Vertex (a, b)) = do
    poke (castPtr ptr) a
    pokeByteOff ptr (sizeOf a) b
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: b)

instance (Storable a, Storable b, Storable c, Bufferable a, Bufferable b, Bufferable c)
    => Storable (Vertex (a, b, c)) where
  alignment _ = 4
  peek ptr = do
    a <- peek (castPtr ptr)
    b <- peekByteOff ptr (sizeOf a)
    c <- peekByteOff ptr (sizeOf b)
    return $ Vertex (a, b, c)
  poke ptr (Vertex (a, b, c)) = do
    poke (castPtr ptr) a
    pokeByteOff ptr (sizeOf a) b
    pokeByteOff ptr (sizeOf b) c
  sizeOf _ = sizeOf (undefined :: a)
               + sizeOf (undefined :: b)
               + sizeOf (undefined :: c)
