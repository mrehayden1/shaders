module Graphics.Shaders.Internal.Buffer (
  Buffer(..),
  BufferValueIn(..),
  InputRate(..),

  B(..),
  BIndex(..),
  B2(..),
  B3(..),
  B4(..),

  Uniform(..),

  BufferFormat(..),
  ToBuffer(..),

  createBuffer,

  destroyBuffer,

  createVkBuffer
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Linear
import qualified Vulkan.Core10.Buffer as VkBuffer
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.FundamentalTypes as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Memory as VkMemory
import qualified Vulkan.Core10.MemoryManagement as VkMemRequirements
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Memory

data Buffer a = Buffer {
  -- Points to the offset of the current front sub-suffer.
  bufferHandle :: Vk.Buffer,
  bufferMemory :: Vk.DeviceMemory,
  bufferNumElems :: Int,
  bufferReleaseKeys :: (ReleaseKey, ReleaseKey, ReleaseKey, ReleaseKey),
  bufferStagingBufferHandle :: Vk.Buffer,
  bufferStride :: Int,
  -- Produces the abstract value which represents the data stored by this
  -- buffer, usually an instance of BufferFormat.
  bufferValueFun :: BufferValueIn -> a,
  bufferWriter :: [HostFormat a] -> IO ()
}

-- See `bufferValueFun`.
data BufferValueIn = BufferValueIn {
  bufferValueInSkipElems :: Int,
  bufferValueInRate :: InputRate
}

data InputRate = InputPerVertex | InputPerInstance
 deriving (Show, Eq, Ord)

-- Allocate a writable buffer with `l` elements of type `a`
createBuffer :: forall m a. (MonadAsyncException m, MonadLogger m,
    MonadResource m, HasVulkan m, HasVulkanDevice m, BufferFormat a)
  => Int -> m (Buffer a)
createBuffer numVertices = do
  device <- getDevice

  let ToBuffer (Kleisli calcAlign) (Kleisli bufWriter) (Kleisli valueProd)
        alignMode usageFlags
          = toBuffer :: ToBuffer (HostFormat a) a

  let (pads, stride) = flip runState 0 . execWriterT
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

  debug "Creating staging buffer."
  let stagingBufferUsageFlags = VkBuffer.BUFFER_USAGE_TRANSFER_SRC_BIT
      stagingMemoryPropertyFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
  (stagingBuffer, stagingBufferReleaseKey, stagingMem, stagingMemReleaseKey)
    <- createVkBuffer bufferSize stagingBufferUsageFlags
         stagingMemoryPropertyFlags

  ptr <- VkMemory.mapMemory device stagingMem 0 bufferSize Vk.zero

  return $ Buffer {
    bufferHandle = buffer,
    bufferMemory = memory,
    bufferNumElems = numVertices,
    bufferReleaseKeys = (bufferReleaseKey, memoryReleaseKey,
      stagingBufferReleaseKey, stagingMemReleaseKey),
    bufferStagingBufferHandle = stagingBuffer,
    bufferStride = stride,
    bufferValueFun = \bIn -> flip evalState 0
      . flip runReaderT (bIn, buffer, stride) . valueProd $ undefined,
    bufferWriter = flip evalStateT (ptr, cycle pads) . mapM_ bufWriter
  }

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

destroyBuffer :: MonadIO m => Buffer a -> m ()
destroyBuffer Buffer{..} = do
  let (buffer, memory, staging, stagingMem) = bufferReleaseKeys
  release buffer
  release memory
  release staging
  release stagingMem

class BufferFormat a where
  type HostFormat a
  toBuffer :: ToBuffer (HostFormat a) a

-- An arrow to turn values of type `b` into bufferable values of type `a`.
-- Only the value producer arrow is expected to produce an `a`, the rest can
-- return undefined.
data ToBuffer a b = ToBuffer
  -- Calculates stride of `b` and padding for any required alignment
  (Kleisli AlignM a b)
  -- Buffer writer
  (Kleisli BufferWriterM a b)
  -- "Value producer" - produces the abstract buffered data `b`.
  (Kleisli ValueProdM a b)
  -- Required aignment
  AlignMode
  -- Vulkan buffer usage flags
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

-- See `bufferValueFun`
type ValueProdM =
  ReaderT
    (BufferValueIn, -- value producer inputs
     Vk.Buffer,     -- buffer handle
     Int            -- stride
    )
    (State
      Int           -- offset
    )

data AlignMode = Align4Byte | AlignStd140 | AlignIndices
 deriving (Show, Eq)

-- Aligns buffered elements to some constraint when writing to an std140
-- aligned buffer.
alignWhenStd140 :: Int -> ToBuffer a a
alignWhenStd140 a =
  ToBuffer
    (Kleisli calcPadding)
    (Kleisli alignBufferWriter)
    (Kleisli return)
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

-- Atomic values that have been buffered.
data B a = B {
  bBufferHandle :: Vk.Buffer,
  bInputRate :: InputRate,
  -- Attribute offset into vertex
  bOffset :: Int,
  bSkipElems :: Int,
  bStride :: Int
} deriving (Eq, Ord, Show)

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

toBufferUnaligned :: forall a. Storable a => ToBuffer a (B a)
toBufferUnaligned =
  ToBuffer
    (Kleisli (const addOffsetToStride))
    (Kleisli doBuffer)
    (Kleisli (const valueProd))
    Align4Byte
    VkBuffer.BUFFER_USAGE_VERTEX_BUFFER_BIT
 where
  addOffsetToStride = do
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
    (BufferValueIn{..}, bufferHandle, stride) <- ask
    let sz = sizeOf (undefined :: a)
    offset <- get
    put $ offset + sz
    return $ B {
      bBufferHandle = bufferHandle,
      bInputRate = bufferValueInRate,
      bOffset = offset,
      bSkipElems = bufferValueInSkipElems,
      bStride = stride
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


instance BufferFormat () where
  type HostFormat () = ()
  toBuffer = arr (const ())

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


-- Buffered data for use as indices only.
--
-- Index data must be tightly packed, but Word8 and Word16 are smaller than the
-- alignment requirements of vertices and uniforms.
newtype BIndex a = BIndex (B a)

instance BufferFormat (BIndex Word16) where
  type HostFormat (BIndex Word16) = Word16
  toBuffer =
    let ToBuffer offset buffer value _ _
          = toBufferUnaligned :: ToBuffer Word16 (B Word16)
        align = AlignIndices
        usage = VkBuffer.BUFFER_USAGE_INDEX_BUFFER_BIT
    in ToBuffer offset buffer value align usage >>> arr BIndex


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
