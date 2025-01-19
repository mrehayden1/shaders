module Graphics.Shaders.Internal.Buffer (
  Buffer(..),
  B(..),
  Uniform(..),

  Bufferable(..),
  ToBuffer(..),

  withBuffer
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
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

data Buffer a = Buffer {
  bufferHandle :: Vk.Buffer,
  bufferNumVertices :: Word32
}

withBuffer :: forall a m. (MonadAsyncException m, MonadLogger m,
    Bufferable a)
  => [a]
  -> ShadersT m (Buffer (BufferFormat a))
withBuffer vertices = do
  deviceHandle <- getDeviceHandle

  let ToBuffer (Kleisli calcStride) (Kleisli bufferer) align
        = toBuffer :: ToBuffer a (BufferFormat a)

  let ((_, pads), stride) = flip runState 0 . runWriterT
                              . flip runReaderT align
                              . calcStride $ (undefined :: a)
      numElems = length vertices
      bufferSize = fromIntegral stride * fromIntegral numElems

  -- Create a vertex buffer
  debug "Creating vertex buffer."
  let vertexBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_DST_BIT
        .|. VkBuffer.BUFFER_USAGE_VERTEX_BUFFER_BIT
      vertexBufferPropertyFlags = Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  (vertexBuffer, _) <- withBuffer' bufferSize vertexBufferUsageFlags
    vertexBufferPropertyFlags

  -- Create a staging buffer
  -- TODO Clean up the staging buffer after it's used.
  debug "Creating staging buffer."
  let stagingBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
  (stagingBuffer, stagingBufferMemory) <- withBuffer' bufferSize
    stagingBufferUsageFlags stagingMemoryPropertyFlags

  -- Fill the buffer
  ptr <-
    VkMemory.mapMemory deviceHandle stagingBufferMemory 0 bufferSize Vk.zero
  _ <- liftIO . flip runStateT (ptr, cycle pads) . mapM bufferer $ vertices
  VkMemory.unmapMemory deviceHandle stagingBufferMemory

  commandPool <- getCommandPool
  copyBuffer commandPool stagingBuffer vertexBuffer bufferSize

  return $ Buffer {
      bufferHandle = vertexBuffer,
      bufferNumVertices = fromIntegral numElems
    }

withBuffer' :: (MonadAsyncException m, MonadLogger m)
  => Vk.DeviceSize
  -> Vk.BufferUsageFlagBits
  -> Vk.MemoryPropertyFlags
  -> ShadersT m (Vk.Buffer, Vk.DeviceMemory)
withBuffer' bufferSize bufferUsageFlags memoryPropertyFlags = do
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
  -- Calculates stride of `b` and padding of elems
  (Kleisli StrideM a b)
  -- Bufferer
  (Kleisli BufferWriterM a b)
  -- Aignment
  Alignment

-- Used to allocate memory, create descriptors, pad elements, etc, so is aware
-- of alignment.
type StrideM =
  ReaderT
    Alignment
    (WriterT
      [Int]    -- Element paddings.
      (State
        Int -- Stride
      )
    )

type BufferWriterM =
  StateT
    (Ptr (), -- Buffer pointer
     [Int]   -- Element paddings, for alignment
    )
    IO

data Alignment = Align4Byte | AlignStd140
 deriving (Show, Eq)

-- Aligns buffered elements to some constraint when writing to an std140
-- aligned buffer.
alignStd140 :: Int -> ToBuffer a a
alignStd140 a =
  ToBuffer (Kleisli calcPadding) (Kleisli alignBufferWriter) Align4Byte
 where
  calcPadding x = do
    alignMode <- ask
    offset <- get
    pad <-
      if alignMode == AlignStd140
        then do
          let p = a - (offset `rem` a)
          put $ offset + p
          return p
        else return 0
    tell [pad]
    return x
  alignBufferWriter x = do
    (ptr, pad : pads) <- get
    put (ptr `plusPtr` pad, pads)
    return x

instance Category ToBuffer where
  id = ToBuffer id id Align4Byte
  (ToBuffer a b c) . (ToBuffer a' b' c') =
    ToBuffer (a . a') (b . b') (combineAlignment c c')
   where
    combineAlignment AlignStd140 _               = AlignStd140
    combineAlignment _               AlignStd140 = AlignStd140
    combineAlignment align           _           = align

instance Arrow ToBuffer where
  arr f = ToBuffer (arr f) (arr f) Align4Byte
  first (ToBuffer a b c) = ToBuffer (first a) (first b) c

-- Tightly packed 4 byte aligned data.
newtype B a = B {
  bOffset :: Word64 -- 4 byte aligned offset
} deriving (Show)

-- newtype wrapper for values buffered with std140 alignment.
newtype Uniform a = Uniform a

instance Bufferable a => Bufferable (Uniform a) where
  type BufferFormat (Uniform a) = Uniform a
  toBuffer =
    let ToBuffer a b _ = toBuffer :: ToBuffer (Uniform a) (Uniform a)
    in ToBuffer a b AlignStd140

-- We only support buffering of 4 byte aligned scalars which are always std140
-- compatible and which itself is always 4 byte aligned, so we won't need to
-- pad offsets when buffering scalars.
toBufferScalar :: forall a. Storable a => ToBuffer a (B a)
toBufferScalar =
  ToBuffer
    (Kleisli (const addOffset))
    (Kleisli doBuffer)
    Align4Byte
 where
  addOffset = do
    let sz = sizeOf (undefined :: a)
    modify (+ sz)
    return $ B {
      bOffset = fromIntegral sz
    }

  doBuffer a = do
    let sz = sizeOf (undefined :: a)
    (ptr, pad) <- get
    put (ptr `plusPtr` sz, pad)
    liftIO $ poke (castPtr ptr) a
    return $ B {
      bOffset = fromIntegral sz
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

instance (Bufferable a, Bufferable b, Bufferable c, Bufferable d)
    => Bufferable (a, b, c, d) where
  type BufferFormat (a, b, c, d)
          = (BufferFormat a, BufferFormat b, BufferFormat c, BufferFormat d)
  toBuffer = proc ~(a, b, c, d) -> do
    (a', b', c') <- toBuffer -< (a, b, c)
    d' <- toBuffer -< d
    returnA -< (a', b', c', d')

instance Bufferable (V2 Float) where
  type BufferFormat (V2 Float) = B (V2 Float)
  toBuffer = proc ~(V2 a b) -> do
    _ <- toBuffer -< (a, b)
    returnA -< B {
      bOffset = 8
    }

instance Bufferable (V3 Float) where
  type BufferFormat (V3 Float) = B (V3 Float)
  toBuffer = proc ~(V3 a b c) -> do
    _ <- toBuffer -< (a, b, c)
    alignStd140 16 -< ()
    returnA -< B {
      bOffset = 12
    }

instance Bufferable (V4 Float) where
  type BufferFormat (V4 Float) = B (V4 Float)
  toBuffer = proc ~(V4 a b c d) -> do
    _ <- toBuffer -< (a, b, c, d)
    returnA -< B {
      bOffset = 16
    }
