module Graphics.Shaders.Internal.Buffer (
  Buffer(..),
  B(..),
  Uniform(..),

  BufferFormat(..),
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
  bufferLength :: Int
}

withBuffer :: forall a m. (MonadAsyncException m, MonadLogger m,
    BufferFormat a)
  => [HostFormat a]
  -> ShadersT m (Buffer a)
withBuffer vertices = do
  deviceHandle <- getDeviceHandle

  let ToBuffer (Kleisli calcAlign) (Kleisli bufferer) _ alignMode
        = toBuffer :: ToBuffer (HostFormat a) a

  let ((_, pads), stride) = flip runState 0 . runWriterT
                              . flip runReaderT alignMode
                              . calcAlign $ undefined
      numElems = length vertices
      bufferSize = fromIntegral stride * fromIntegral numElems

  -- Create a vertex buffer
  debug "Creating buffer."
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
  liftIO . flip evalStateT (ptr, cycle pads) . mapM_ bufferer $ vertices
  VkMemory.unmapMemory deviceHandle stagingBufferMemory

  commandPool <- getCommandPool
  copyBuffer commandPool stagingBuffer vertexBuffer bufferSize

  return $ Buffer {
    bufferHandle = vertexBuffer,
    bufferLength = numElems
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

class BufferFormat a where
  type HostFormat a
  toBuffer :: ToBuffer (HostFormat a) a

data ToBuffer a b = ToBuffer
  -- Calculates stride of `b` and any padding for alignment
  (Kleisli AlignM a b)
  -- Bufferer
  (Kleisli BufferWriterM a b)
  -- Value producer
  (Kleisli ValueProdM a b)
  -- Aignment
  AlignMode

-- Calculates element stride, padding for aligning elements.
type AlignM =
  ReaderT
    AlignMode
    (WriterT
      [Int]  -- std140 paddings.
      (State
        Int  -- offset
      )
    )

type BufferWriterM =
  StateT
    (Ptr (), -- Buffer pointer
     [Int]   -- std140 paddings, for alignment
    )
    IO

type ValueProdM =
  ReaderT
  Int       -- Binding number
  (State
    ([Int], -- std140 paddings
     Int    -- offset
    )
  )

data AlignMode = Align4Byte | AlignStd140
 deriving (Show, Eq)

-- Aligns buffered elements to some constraint when writing to an std140
-- aligned buffer.
alignWhenStd140 :: Int -> ToBuffer a a
alignWhenStd140 a =
  ToBuffer
    (Kleisli calcPadding)
    (Kleisli alignBufferWriter)
    (Kleisli alignValueProd)
    Align4Byte
 where
  calcPadding x = do
    alignMode <- ask
    offset <- get
    pad <-
      if alignMode == AlignStd140
        then do
          let p = (a - (offset `mod` a)) `mod` a
          put $ offset + p
          return p
        else return 0
    tell [pad]
    return x
  alignBufferWriter x = do
    (ptr, pad : pads) <- get
    put (ptr `plusPtr` pad, pads)
    return x
  alignValueProd x = do
    (pads, offset) <- get
    put (tail pads, offset + head pads)
    return x

instance Category ToBuffer where
  id = ToBuffer id id id Align4Byte
  (ToBuffer a b c d) . (ToBuffer a' b' c' d') =
    ToBuffer (a . a') (b . b') (c . c') (combineAlignMode d d')
   where
    combineAlignMode AlignStd140 _           = AlignStd140
    combineAlignMode _           AlignStd140 = AlignStd140
    combineAlignMode alignMode   _           = alignMode

instance Arrow ToBuffer where
  arr f = ToBuffer (arr f) (arr f) (arr f) Align4Byte
  first (ToBuffer a b c d) = ToBuffer (first a) (first b) (first c) d

-- Buffered value
data B a = B {
  bBinding :: Int,
  bOffset :: Int
} deriving (Show)

-- Wrapper for values buffered with std140 alignment.
data Uniform a = Uniform

instance BufferFormat a => BufferFormat (Uniform a) where
  type HostFormat (Uniform a) = HostFormat a
  toBuffer =
    let ToBuffer a b c _ = toBuffer :: ToBuffer (HostFormat a) a
    in ToBuffer a b c AlignStd140 >>> arr (const Uniform)

instance BufferFormat (B ()) where
  type HostFormat (B ()) = ()
  toBuffer = proc ~() ->
    returnA -< B {
      bBinding = undefined,
      bOffset = 0
    }

-- Scalars are always std140 aligned since all our scalars are 4 bytes wide.
toBufferUnaligned :: forall a. Storable a => ToBuffer a (B a)
toBufferUnaligned =
  ToBuffer
    (Kleisli (const addOffset))
    (Kleisli doBuffer)
    (Kleisli (const valueProd))
    Align4Byte
 where
  addOffset = do
    let sz = sizeOf (undefined :: a)
    modify (+ sz)
    return undefined

  doBuffer a = do
    let sz = sizeOf (undefined :: a)
    (ptr, pad) <- get
    put (ptr `plusPtr` sz, pad)
    liftIO $ poke (castPtr ptr) a
    return undefined

  valueProd = do
    binding <- ask
    let sz = sizeOf (undefined :: a)
    (pads, offset) <- get
    put (pads, offset + sz)
    return $ B {
      bBinding = binding,
      bOffset = offset
    }

instance BufferFormat (B Float) where
  type HostFormat (B Float) = Float
  toBuffer = toBufferUnaligned

instance BufferFormat (B Word32) where
  type HostFormat (B Word32) = Word32
  toBuffer = toBufferUnaligned

castB :: B a -> B b
castB B{..} = B{ bBinding = bBinding, bOffset = bOffset }

instance BufferFormat (B (V2 Float)) where
  type HostFormat (B (V2 Float)) = V2 Float
  toBuffer = proc ~(V2 a b) -> do
    alignWhenStd140 (2 * sz) -< () -- align to 2N per std140
    a' <- toBufferUnaligned -< a
    _ <- toBufferUnaligned -< b
    returnA -< castB a'
   where
    sz = sizeOf (undefined :: Float)

instance BufferFormat (B (V3 Float)) where
  type HostFormat (B (V3 Float)) = V3 Float
  toBuffer = proc ~(V3 a b c) -> do
    alignWhenStd140 (4 * sz) -< () -- align to 4N per std140
    a' <- toBufferUnaligned -< a
    _ <- toBufferUnaligned -< b
    _ <- toBufferUnaligned -< c
    returnA -< castB a'
   where
    sz = sizeOf (undefined :: Float)

instance BufferFormat (B (V4 Float)) where
  type HostFormat (B (V4 Float)) = V4 Float
  toBuffer = proc ~(V4 a b c d) -> do
    alignWhenStd140 (4 * sz) -< () -- align to 4N per std140
    a' <- toBufferUnaligned -< a
    _ <- toBufferUnaligned -< b
    _ <- toBufferUnaligned -< c
    _ <- toBufferUnaligned -< d
    returnA -< castB a'
   where
    sz = sizeOf (undefined :: Float)

instance BufferFormat (B (M33 Float)) where
  type HostFormat (B (M33 Float)) = M33 Float
  toBuffer = proc ~(M33 a b c) -> do
    a' <- buffer -< a
    _ <- buffer -< b
    _ <- buffer -< c
    -- ensure the next member is 4N aligned per std140
    alignWhenStd140 (4 * sz) -< ()
    returnA -< castB a'
   where
    sz = sizeOf (undefined :: Float)
    buffer = toBuffer :: ToBuffer (V3 Float) (B (V3 Float))

instance BufferFormat (B (M44 Float)) where
  type HostFormat (B (M44 Float)) = M44 Float
  toBuffer = proc ~(M44 a b c d) -> do
    a' <- buffer -< a
    _ <- buffer -< b
    _ <- buffer -< c
    _ <- buffer -< d
    returnA -< castB a'
   where
    buffer = toBuffer :: ToBuffer (V4 Float) (B (V4 Float))

instance (BufferFormat a, BufferFormat b) => BufferFormat (a, b) where
  type HostFormat (a, b) = (HostFormat a, HostFormat b)
  toBuffer = proc ~(a, b) -> do
    a' <- toBuffer -< a
    b' <- toBuffer -< b
    returnA -< (a', b')

instance (BufferFormat a, BufferFormat b, BufferFormat c)
    => BufferFormat (a, b, c) where
  type HostFormat (a, b, c)
          = (HostFormat a, HostFormat b, HostFormat c)
  toBuffer = proc ~(a, b, c) -> do
    (a', b') <- toBuffer -< (a, b)
    c' <- toBuffer -< c
    returnA -< (a', b', c')

instance (BufferFormat a, BufferFormat b, BufferFormat c, BufferFormat d)
    => BufferFormat (a, b, c, d) where
  type HostFormat (a, b, c, d)
          = (HostFormat a, HostFormat b, HostFormat c, HostFormat d)
  toBuffer = proc ~(a, b, c, d) -> do
    (a', b', c') <- toBuffer -< (a, b, c)
    d' <- toBuffer -< d
    returnA -< (a', b', c', d')

instance (BufferFormat a, BufferFormat b, BufferFormat c, BufferFormat d,
  BufferFormat e)
    => BufferFormat (a, b, c, d, e) where
  type HostFormat (a, b, c, d, e)
          = (HostFormat a, HostFormat b, HostFormat c, HostFormat d,
             HostFormat e)
  toBuffer = proc ~(a, b, c, d, e) -> do
    (a', b', c', d') <- toBuffer -< (a, b, c, d)
    e' <- toBuffer -< e
    returnA -< (a', b', c', d', e')
