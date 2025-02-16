module Graphics.Shaders.Internal.Buffer (
  Buffer(..),

  B(..),
  B2(..),
  B3(..),
  B4(..),

  Uniform(..),

  BufferFormat(..),
  ToBuffer(..),

  withBuffer,
  withBuffer',
  allocateMemory,
  withOneTimeSubmitCommandBuffer
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
import qualified Vulkan.Core10.MemoryManagement as VkMemRequirements
import qualified Vulkan.Core10.Queue as VkQueue
import Vulkan.CStruct.Extends
import qualified Vulkan.Zero as Vk

import Data.Bits.Extra
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
  debug "Creating destination buffer."
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
  debug "Filling staging buffer."
  ptr <-
    VkMemory.mapMemory deviceHandle stagingBufferMemory 0 bufferSize Vk.zero
  liftIO . flip evalStateT (ptr, cycle pads) . mapM_ bufferer $ vertices
  VkMemory.unmapMemory deviceHandle stagingBufferMemory

  debug "Copying staging buffer to destination buffer."
  withOneTimeSubmitCommandBuffer $ \commandBuffer -> do
    let bufferCopy = Vk.zero { VkCopy.size = bufferSize }
    VkCmd.cmdCopyBuffer commandBuffer stagingBuffer vertexBuffer
      (V.singleton bufferCopy)

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

  memoryRequirements <-
    VkMemRequirements.getBufferMemoryRequirements deviceHandle buffer

  -- Allocate and bind buffer memory
  memory <- allocateMemory memoryRequirements memoryPropertyFlags

  VkMemRequirements.bindBufferMemory deviceHandle buffer memory 0

  return (buffer, memory)

allocateMemory :: (MonadAsyncException m, MonadLogger m)
  => VkMemRequirements.MemoryRequirements
  -> Vk.MemoryPropertyFlags
  -> ShadersT m Vk.DeviceMemory
allocateMemory memoryRequirements memoryPropertyFlags = do
  deviceHandle <- getDeviceHandle

  -- Find compatible memory
  memoryProperties <- getDeviceMemoryProperties
  let memoryTypeIndices = V.imapMaybe getMemoryTypeIndexIfCompatible
        . VkDevice.memoryTypes
        $ memoryProperties

  when (V.null memoryTypeIndices) $ do
    let msg = "No suitable memory type found."
    err msg
    lift . throw . ShadersMemoryException $ msg

  let memoryTypeIndex = V.head memoryTypeIndices

  let allocInfo = Vk.zero {
        VkMemory.allocationSize = VkMemRequirements.size memoryRequirements,
        VkMemory.memoryTypeIndex = fromIntegral memoryTypeIndex
      }
  fromCps
    $ VkMemory.withMemory deviceHandle allocInfo Nothing bracket
 where
  getMemoryTypeIndexIfCompatible i t =
    if VkDevice.propertyFlags t .&&. memoryPropertyFlags
         && testBit (VkMemRequirements.memoryTypeBits memoryRequirements) i
    then Just i
    else Nothing


withOneTimeSubmitCommandBuffer :: (MonadIO m, MonadLogger m)
  => (Vk.CommandBuffer -> ShadersT m ())
  -> ShadersT m ()
withOneTimeSubmitCommandBuffer c = do
  deviceHandle <- getDeviceHandle
  queueHandle <- getQueueHandle
  commandPool <- getCommandPool
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

  c cmdBuffer

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
    let ToBuffer a b c _ = toBuffer :: ToBuffer (HostFormat a) a
    in ToBuffer a b c AlignStd140 >>> arr (const Uniform)

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
