module Graphics.Shaders.Buffer (
  vertexData,

  VertexBuffer(..),
  VertexAttribute(..),
  VertexFormat(..),
  toVertexBuffer
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Linear
import qualified Vulkan.Core10.Buffer as VkBuffer
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Memory as VkMemory
import qualified Vulkan.Core10.MemoryManagement as VkManagement
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Device
import Util.Bits

vertexData :: [(V2 Float, V3 Float)]
vertexData = [
    (V2   0.0  (-0.5), V3 1.0 0.0 0.0),
    (V2 (-0.5)   0.5 , V3 0.0 0.0 1.0),
    (V2   0.5    0.5 , V3 0.0 1.0 0.0)
  ]

data VertexBuffer a = VertexBuffer {
  vertexBufferHandle :: Vk.Buffer,
  vertexBufferVertices :: Word32
}

data VertexAttribute = VertexAttribute {
  attributeFormat :: Vk.Format,
  attributeOffset :: Word32 -- in bytes
} deriving (Show)

toVertexBuffer :: forall a m. (Storable (Vertex a), MonadAsyncException m,
    MonadLogger m)
  => Device
  -> [a]
  -> Codensity m (Maybe (VertexBuffer a))
toVertexBuffer Device{..} vertices = runMaybeT $ do
  let stride = fromIntegral . sizeOf $ (undefined :: Vertex a)
      numElems = fromIntegral . length $ vertices
      bufferSize = stride * numElems

  debug "Creating buffer."
  -- Create the buffer
  let bufferInfo = Vk.zero {
          VkBuffer.sharingMode = VkBuffer.SHARING_MODE_EXCLUSIVE,
          VkBuffer.size = bufferSize,
          VkBuffer.usage = VkBuffer.BUFFER_USAGE_VERTEX_BUFFER_BIT
        }
  vertexBuffer <- lift $ Codensity
    $ VkBuffer.withBuffer deviceHandle bufferInfo Nothing bracket

  -- Find compatible memory
  memoryRequirements <-
    VkManagement.getBufferMemoryRequirements deviceHandle vertexBuffer

  let getMemoryTypeIndexIfCompatible i t =
        if VkDevice.propertyFlags t .&&. requiredMemoryPropertyFlags
             && testBit (VkManagement.memoryTypeBits memoryRequirements) i
          then Just i
          else Nothing

  let memoryTypeIndices = V.imapMaybe getMemoryTypeIndexIfCompatible
        . VkDevice.memoryTypes
        $ deviceMemoryProperties

  when (V.null memoryTypeIndices) $ do
    let msg = "No suitable memory type found."
    err msg
    fail msg

  let memoryTypeIndex = V.head memoryTypeIndices

  -- Allocate and bind buffer memory
  let allocInfo = Vk.zero {
          VkMemory.allocationSize = VkManagement.size memoryRequirements,
          VkMemory.memoryTypeIndex = fromIntegral memoryTypeIndex
        }
  memory <- lift $ Codensity
    $ VkMemory.withMemory deviceHandle allocInfo Nothing bracket

  VkManagement.bindBufferMemory deviceHandle vertexBuffer memory 0

  -- Fill the buffer
  ptr <- VkMemory.mapMemory deviceHandle memory 0 bufferSize Vk.zero
  liftIO $ pokeArray (castPtr ptr) . fmap Vertex $ vertices
  VkMemory.unmapMemory deviceHandle memory

  return $ VertexBuffer {
      vertexBufferHandle = vertexBuffer,
      vertexBufferVertices = fromIntegral numElems
    }
 where
  requiredMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
    .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT

-- Used to create binding and attribute descriptions. Will only represent 4
-- byte aligned types.
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
