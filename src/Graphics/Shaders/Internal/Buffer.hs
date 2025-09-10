module Graphics.Shaders.Internal.Buffer (
  Buffer(..),
  bufferHandle,
  BufferAccess(..),

  B(..),
  B2(..),
  B3(..),
  B4(..),

  Uniform(..),

  BufferFormat(..),
  ToBuffer(..),

  createBuffer,
  createBufferReadOnly,

  destroyBuffer,

  writeBuffer,

  createVkBuffer,

  withOneTimeSubmitCommandBuffer
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import qualified Data.Vector as V
import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Linear
import qualified Vulkan.Core10.Buffer as VkBuffer
import qualified Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.CommandBufferBuilding as VkCopy (BufferCopy(..))
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.FundamentalTypes as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Memory as VkMemory
import qualified Vulkan.Core10.MemoryManagement as VkMemRequirements
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Internal.Command
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Memory

-- BufferAccess - Buffer usage type.
--
-- BufferReadOnly
--  Stored in fast GPU accesible memory and only written to on initialization
-- BufferReadWrite
--  Stored in CPU accessible memory. The buffer memory is also duplicated into
--  sub-buffers, one per frame, so CPU/GPU synchronisation is unnecessary
--  between writes.
data BufferAccess = ReadOnly | ReadWrite

data Buffer (r :: BufferAccess) a where
  BufferReadOnly :: {
    rBufferHandle :: Vk.Buffer,
    rBufferNumElems :: Int,
    rBufferReleaseKeys :: (ReleaseKey, ReleaseKey)
  } -> Buffer 'ReadOnly a
  BufferWritable :: {
    -- Points to the offset of the current front sub-suffer.
    wBufferHandle :: Vk.Buffer,
    wBufferMemory :: Vk.DeviceMemory,
    wBufferNumElems :: Int,
    wBufferPads :: [Int],
    wBufferPtr :: Ptr (),
    wBufferReleaseKeys :: (ReleaseKey, ReleaseKey, ReleaseKey, ReleaseKey),
    wBufferStagingBufferHandle :: Vk.Buffer,
    wBufferStride :: Int,
    wBufferWriter :: HostFormat a -> BufferWriterM a
  } -> Buffer 'ReadWrite a

bufferHandle :: Buffer r a -> Vk.Buffer
bufferHandle (BufferReadOnly h _ _)             = h
bufferHandle (BufferWritable h _ _ _ _ _ _ _ _) = h

-- Allocate a writable buffer with `l` elements of type `a`
createBuffer :: forall m a. (MonadAsyncException m, MonadLogger m,
    MonadResource m, HasVulkan m, HasVulkanDevice m, BufferFormat a)
  => Int -> m (Buffer 'ReadWrite a)
createBuffer numVertices = do
  device <- getDevice

  let ToBuffer (Kleisli calcAlign) (Kleisli bufWriter) _ alignMode usageFlags
        = toBuffer :: ToBuffer (HostFormat a) a

  let ((_, pads), stride) = flip runState 0 . runWriterT
                              . flip runReaderT alignMode
                              . calcAlign $ undefined
      bufferSize = fromIntegral $ stride * numVertices

  debug "Creating writable buffer."
  let vertexBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_DST_BIT
        .|. usageFlags
      vertexBufferPropertyFlags = Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
  (buffer, bufferReleaseKey, memory, memoryReleaseKey)
    <- createVkBuffer bufferSize vertexBufferUsageFlags
         vertexBufferPropertyFlags

  -- Create a staging buffer
  debug "Creating staging buffer."
  let stagingBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
  (stagingBuffer, stagingBufferReleaseKey, stagingMem, stagingMemReleaseKey)
    <- createVkBuffer bufferSize stagingBufferUsageFlags
         stagingMemoryPropertyFlags

  ptr <- VkMemory.mapMemory device stagingMem 0 bufferSize Vk.zero

  return $ BufferWritable {
    wBufferHandle = buffer,
    wBufferMemory = memory,
    wBufferNumElems = numVertices,
    wBufferPads = pads,
    wBufferPtr = ptr,
    wBufferReleaseKeys = (bufferReleaseKey, memoryReleaseKey,
      stagingBufferReleaseKey, stagingMemReleaseKey),
    wBufferStagingBufferHandle = stagingBuffer,
    wBufferStride = stride,
    wBufferWriter = bufWriter
  }

createBufferReadOnly :: forall a m. (MonadAsyncException m, MonadLogger m,
    MonadResource m, HasVulkan m, HasVulkanDevice m, BufferFormat a)
  => [HostFormat a]
  -> m (Buffer 'ReadOnly a)
createBufferReadOnly vertices = do
  deviceHandle <- getDevice
  queueHandle <- getQueue
  commandPool <- getCommandPool

  let ToBuffer (Kleisli calcAlign) (Kleisli bufferer) _ alignMode usageFlags
        = toBuffer :: ToBuffer (HostFormat a) a

  let ((_, pads), stride) = flip runState 0 . runWriterT
                              . flip runReaderT alignMode
                              . calcAlign $ undefined
      numVertices = length vertices
      bufferSize = fromIntegral stride * fromIntegral numVertices

  -- Create a vertex buffer
  debug "Creating read only buffer."
  let vertexBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_DST_BIT
        .|. usageFlags
      vertexBufferPropertyFlags = Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  (vertexBuffer, vertexBufferReleaseKey, _, vertexMemoryReleaseKey)
    <- createVkBuffer bufferSize vertexBufferUsageFlags
         vertexBufferPropertyFlags

  -- Create a staging buffer
  debug "Creating staging buffer."
  let stagingBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
  (stagingBuffer, stagingBufferReleaseKey, stagingMem, stagingMemReleaseKey)
    <- createVkBuffer bufferSize stagingBufferUsageFlags
         stagingMemoryPropertyFlags

  debug "Filling staging buffer."
  ptr <- VkMemory.mapMemory deviceHandle stagingMem 0 bufferSize Vk.zero
  liftIO . flip evalStateT (ptr, cycle pads) . mapM_ bufferer $ vertices
  VkMemory.unmapMemory deviceHandle stagingMem

  debug "Copying staging buffer to destination buffer."
  -- Copies vertices synchronously into the staging buffer memory.
  withOneTimeSubmitCommandBuffer deviceHandle queueHandle commandPool $
    \commandBuffer -> do
      let bufferCopy = Vk.zero { VkCopy.size = bufferSize }
      VkCmd.cmdCopyBuffer commandBuffer stagingBuffer vertexBuffer
        (V.singleton bufferCopy)

  debug "Destroying staging buffer."
  release stagingMemReleaseKey
  release stagingBufferReleaseKey

  return $ BufferReadOnly {
    rBufferHandle = vertexBuffer,
    rBufferNumElems = numVertices,
    rBufferReleaseKeys = (vertexBufferReleaseKey, vertexMemoryReleaseKey)
  }

writeBuffer :: MonadIO m => Buffer 'ReadWrite a -> [HostFormat a]
  -> m ()
writeBuffer BufferWritable{..} =
  -- Put the vertices in the staging buffer to be copied to the front buffer
  -- before draw commands are issued in the next frame.
  liftIO . flip evalStateT (wBufferPtr, wBufferPads) . mapM_ wBufferWriter

-- createVkBuffer - Create a Vk buffer and allocate device memory for it.
createVkBuffer :: (MonadAsyncException m, MonadLogger m, MonadResource m,
    HasVulkan m, HasVulkanDevice m)
  => Vk.DeviceSize
  -> Vk.BufferUsageFlagBits
  -> Vk.MemoryPropertyFlags
  -> m (Vk.Buffer, ReleaseKey, Vk.DeviceMemory, ReleaseKey)
createVkBuffer bufferSize bufferUsageFlags
    memoryPropertyFlags = do
  allocator <- getVulkanAllocator
  device <- getDevice

  let bufferInfo = Vk.zero {
        VkBuffer.sharingMode = VkBuffer.SHARING_MODE_EXCLUSIVE,
        VkBuffer.size = bufferSize,
        VkBuffer.usage = bufferUsageFlags
      }
  (bufferReleaseKey, buffer) <- allocate
    (VkBuffer.createBuffer device bufferInfo allocator)
    (\b -> VkBuffer.destroyBuffer device b allocator)

  memoryRequirements <-
    VkMemRequirements.getBufferMemoryRequirements device buffer

  -- Allocate and bind buffer memory
  (memoryReleaseKey, memory) <- allocateMemory memoryRequirements
    memoryPropertyFlags

  VkMemRequirements.bindBufferMemory device buffer memory 0

  return (buffer, bufferReleaseKey, memory, memoryReleaseKey)

destroyBuffer :: MonadIO m => Buffer t a -> m ()
destroyBuffer b = case b of
  BufferReadOnly {..} -> case rBufferReleaseKeys of
    (bufferReleaseKey, memoryReleaseKey) -> do
      release bufferReleaseKey
      release memoryReleaseKey
  BufferWritable {..} -> case wBufferReleaseKeys of
    (buffer, memory, staging, stagingMem) -> do
      release buffer
      release memory
      release staging
      release stagingMem

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
  -- Vulkan buffer usage
  VkBuffer.BufferUsageFlagBits

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
    Vk.zero
 where
  calcPadding x = do
    alignMode <- ask
    pad <-
      if alignMode == AlignStd140
        then do
          offset <- get
          let p = (a - (offset `mod` a)) `mod` a
          put $ offset + p
          return p
        else return 0
    tell [pad]
    return x
  alignBufferWriter x = do
    (ptr, pads) <- get
    put (ptr `plusPtr` head pads, tail pads)
    return x
  alignValueProd x = do
    (pads, offset) <- get
    put (tail pads, offset + head pads)
    return x

instance Category ToBuffer where
  id = ToBuffer id id id Align4Byte Vk.zero
  (ToBuffer a b c d f) . (ToBuffer a' b' c' d' f') =
    ToBuffer (a . a') (b . b') (c . c') (combineAlignMode d d') (f .|. f')
   where
    combineAlignMode AlignStd140 _           = AlignStd140
    combineAlignMode _           AlignStd140 = AlignStd140
    combineAlignMode alignMode   _           = alignMode

instance Arrow ToBuffer where
  arr f = ToBuffer (arr f) (arr f) (arr f) Align4Byte Vk.zero
  first (ToBuffer a b c d f) = ToBuffer (first a) (first b) (first c) d f

-- Atomic buffered values
data B a = B {
  bBinding :: Int,
  bOffset :: Int
} deriving (Show)

newtype B2 a = B2 { unB2 :: B a }
newtype B3 a = B3 { unB3 :: B a }
newtype B4 a = B4 { unB4 :: B a }

-- Wrapper for values buffered with std140 alignment.
data Uniform a = Uniform

instance BufferFormat a => BufferFormat (Uniform a) where
  type HostFormat (Uniform a) = HostFormat a
  toBuffer =
    let ToBuffer a b c _ usage = toBuffer :: ToBuffer (HostFormat a) a
        usage' = usage .|. VkBuffer.BUFFER_USAGE_UNIFORM_BUFFER_BIT
    in ToBuffer a b c AlignStd140 usage' >>> arr (const Uniform)

instance BufferFormat (B ()) where
  type HostFormat (B ()) = ()
  toBuffer = proc ~() ->
    returnA -< B {
      bBinding = undefined,
      bOffset = 0
    }

toBufferUnaligned :: forall a. Storable a => ToBuffer a (B a)
toBufferUnaligned =
  ToBuffer
    (Kleisli (const addOffset))
    (Kleisli doBuffer)
    (Kleisli (const valueProd))
    Align4Byte
    VkBuffer.BUFFER_USAGE_VERTEX_BUFFER_BIT
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

toBufferB2 :: forall a. Storable a => ToBuffer (V2 a) (B2 a)
toBufferB2 = proc ~(V2 a b) -> do
  alignWhenStd140 (2 * sz) -< ()
  a' <- toBufferUnaligned -< a
  _  <- toBufferUnaligned -< b
  returnA -< B2 a'
 where
  sz = sizeOf (undefined :: a)

toBufferB3 :: forall a. Storable a => ToBuffer (V3 a) (B3 a)
toBufferB3 = proc ~(V3 a b c) -> do
    alignWhenStd140 (4 * sz) -< ()
    a' <- toBufferUnaligned -< a
    _  <- toBufferUnaligned -< b
    _  <- toBufferUnaligned -< c
    returnA -< B3 a'
   where
    sz = sizeOf (undefined :: a)

toBufferB4 :: forall a. Storable a => ToBuffer (V4 a) (B4 a)
toBufferB4 = proc ~(V4 a b c d) -> do
  alignWhenStd140 (4 * sz) -< ()
  a' <- toBufferUnaligned -< a
  _  <- toBufferUnaligned -< b
  _  <- toBufferUnaligned -< c
  _  <- toBufferUnaligned -< d
  returnA -< B4 a'
 where
  sz = sizeOf (undefined :: a)


instance BufferFormat (B Float) where
  type HostFormat (B Float) = Float
  toBuffer = toBufferUnaligned

instance BufferFormat (B Word32) where
  type HostFormat (B Word32) = Word32
  toBuffer = toBufferUnaligned


instance BufferFormat (B2 Float) where
  type HostFormat (B2 Float) = V2 Float
  toBuffer = toBufferB2

instance BufferFormat (B3 Float) where
  type HostFormat (B3 Float) = V3 Float
  toBuffer = toBufferB3

instance BufferFormat (B4 Float) where
  type HostFormat (B4 Float) = V4 Float
  toBuffer = toBufferB4


instance BufferFormat (V0 a) where
  type HostFormat (V0 a) = V0 (HostFormat a)
  toBuffer = proc ~V0 -> returnA -< V0

instance BufferFormat a => BufferFormat (V1 a) where
  type HostFormat (V1 a) = V1 (HostFormat a)
  toBuffer = proc ~(V1 a) -> do
    a' <- toBuffer -< a
    returnA -< V1 a'

instance BufferFormat a => BufferFormat (V2 a) where
  type HostFormat (V2 a) = V2 (HostFormat a)
  toBuffer = proc ~(V2 a b) -> do
    (a', b') <- toBuffer -< (a, b)
    returnA -< V2 a' b'

instance BufferFormat a => BufferFormat (V3 a) where
  type HostFormat (V3 a) = V3 (HostFormat a)
  toBuffer = proc ~(V3 a b c) -> do
    (a', b', c') <- toBuffer -< (a, b, c)
    returnA -< V3 a' b' c'

instance BufferFormat a => BufferFormat (V4 a) where
  type HostFormat (V4 a) = V4 (HostFormat a)
  toBuffer = proc ~(V4 a b c d) -> do
    (a', b', c', d') <- toBuffer -< (a, b, c, d)
    returnA -< V4 a' b' c' d'


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
