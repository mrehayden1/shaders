module Graphics.Shaders.Buffer (
  vertexData,

  VertexBuffer(..),
  VertexAttribute(..),
  VertexFormat(..),

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

import Graphics.Shaders.Base
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

data VertexAttribute = VertexAttribute {
  attributeFormat :: Vk.Format,
  attributeOffset :: Word32 -- in bytes
} deriving (Show)

withVerticesAsBuffer :: forall a m. (Storable (Vertex a),
    MonadAsyncException m, MonadLogger m)
  => [a]
  -> ShadersT m (VertexBuffer a)
withVerticesAsBuffer vertices = do
  Device{..} <- getDevice
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

  copyBuffer deviceCommandPool stagingBuffer vertexBuffer bufferSize

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
  Device{..} <- getDevice
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

  let memoryTypeIndices = V.imapMaybe getMemoryTypeIndexIfCompatible
        . VkDevice.memoryTypes
        $ deviceMemoryProperties

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

copyBuffer :: (MonadIO m, MonadLogger m, MonadShaders m)
  => Vk.CommandPool
  -> Vk.Buffer
  -> Vk.Buffer
  -> Vk.DeviceSize
  -> m ()
copyBuffer commandPool src dest size = do
  Device{..} <- getDevice
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

  VkQueue.queueSubmit deviceQueueHandle (V.singleton submitInfo) Vk.zero
  VkQueue.queueWaitIdle deviceQueueHandle

  debug "Destroying temporary command buffer."
  VkCmdBuffer.freeCommandBuffers deviceHandle commandPool cmdBuffers

-- Represents 4 byte aligned types. Used to create binding and attribute
-- descriptions.
class VertexFormat a where
  vertexAttributes :: a -> [VertexAttribute]
  vertexStride :: a -> Word32 -- in bytes

instance VertexFormat Float where
  vertexAttributes _ = [VertexAttribute Vk.FORMAT_R32_SFLOAT 0]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: Float)

instance VertexFormat (V2 Float) where
  vertexAttributes _ = [VertexAttribute Vk.FORMAT_R32G32_SFLOAT 0]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V2 Float)

instance VertexFormat (V3 Float) where
  vertexAttributes _ = [VertexAttribute Vk.FORMAT_R32G32B32_SFLOAT 0]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V3 Float)

instance VertexFormat (V4 Float) where
  vertexAttributes _ = [VertexAttribute Vk.FORMAT_R32G32B32A32_SFLOAT 0]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V4 Float)

instance (VertexFormat a, VertexFormat b) => VertexFormat (a, b) where
  vertexAttributes _ =
    let attrs  = vertexAttributes (undefined :: a)
        stride = vertexStride (undefined :: a)
        attrs' = vertexAttributes (undefined :: b)
    in attrs <> fmap (\attr -> attr { attributeOffset = attributeOffset attr + stride }) attrs'

  vertexStride _ = vertexStride (undefined :: a)
                    + vertexStride (undefined :: b)

instance (VertexFormat a, VertexFormat b, VertexFormat c)
    => VertexFormat (a, b, c) where
  vertexAttributes _ =
    let attrs  = vertexAttributes (undefined :: (a, b))
        stride = vertexStride (undefined :: (a, b))
        attrs' = vertexAttributes (undefined :: c)
    in attrs <>
         fmap (\attr -> attr { attributeOffset = attributeOffset attr + stride })
              attrs'

  vertexStride _ = vertexStride (undefined :: (a, b))
                     + vertexStride (undefined :: c)

-- Type wrapper for marshalling vertices into buffers.
newtype Vertex a = Vertex {
  unVertex :: a
} deriving (Show)

instance (Storable a, Storable b, VertexFormat a, VertexFormat b)
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
