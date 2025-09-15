module Graphics.Shaders.Internal.PrimitiveArray (
  VertexArray(..),

  dropVertices,
  takeVertices,
  zipVertices,

  toVertexArray,

  IndexArray(..),

  dropIndices,
  takeIndices,

  toIndexArray,

  PrimitiveArray(..),
  PrimitiveArrayDrawCall(..),

  PrimitiveTopology(..),

  Indexed(..),

  Lines,
  Points,
  Triangles,

  BaseTopology(..),

  primitiveTopology,

  toPrimitiveArray,
  toPrimitiveArrayIndexed,
  toPrimitiveArrayInstanced,
  toPrimitiveArrayIndexedInstanced,

  VertexInput(..),
  ToVertex(..)
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as M
import Data.Word
import Linear
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Handles as Vk

import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.DeclM
import Graphics.Shaders.Internal.Expr

-- Wrapper for Buffers that allows us to combine them and truncate them from
-- either end.
data VertexArray a = VertexArray {
  vertexArrayValueFun :: BufferValueIn -> a,
  vertexArrayLength :: Int, -- The number of vertices to take
  vertexArrayStart :: Int -- Pointer to the first vertex in the buffer
}

instance Functor VertexArray where
  fmap f (VertexArray g l s) = VertexArray (f . g) l s

dropVertices :: Int -> VertexArray a -> VertexArray a
dropVertices n (VertexArray f l s) =
  let n' = min (max n 0) l
  in VertexArray f (l - n') (s + n')

takeVertices :: Int -> VertexArray a -> VertexArray a
takeVertices n (VertexArray f l s) = VertexArray f (min (max n 0) l) s

toVertexArray :: Buffer a -> VertexArray a
toVertexArray Buffer{..} = VertexArray bufferValueFun bufferNumElems 0

zipVertices :: VertexArray a -> VertexArray b -> (a -> b -> c) -> VertexArray c
zipVertices (VertexArray aF aL aS) (VertexArray bF bL bS) f =
  let skipElems = min aS bS
      cF x =
        let baseSkip = bufferValueInSkipElems x - skipElems
        in f (aF x { bufferValueInSkipElems = baseSkip + aS })
             (bF x { bufferValueInSkipElems = baseSkip + bS })
  in VertexArray cF (min aL bL) skipElems

-- TODO Restart indices.
data IndexArray = IndexArray {
  indexArrayBufferHandle :: Vk.Buffer,
  indexArrayLength :: Int,
  indexArrayStart :: Int,
  indexArrayType :: Vk.IndexType
}

dropIndices :: Int -> IndexArray -> IndexArray
dropIndices n (IndexArray h l s t) =
  let n' = min (max n 0) l
  in IndexArray h (l - n') (s + n') t

takeIndices :: Int -> IndexArray -> IndexArray
takeIndices n (IndexArray h l s t) = IndexArray h (min (max n 0) l) s t

toIndexArray :: forall b. (IndexInput b)
  => Buffer b
  -> IndexArray
toIndexArray Buffer{..} =
  IndexArray bufferHandle bufferNumElems 0 . indexType $ b
 where b = undefined :: b


-- A typeclass for types that can be used as vertex indices.
class IndexInput a where
  indexType :: a -> Vk.IndexType

{- TODO Support 16-bit indices. We currently only support buffering of 4 byte
 - aligned data.
instance IndexInput (B Word16) where
  indexType _ = Vk.INDEX_TYPE_UINT16
-}

instance IndexInput (B Word32) where
  indexType _ = Vk.INDEX_TYPE_UINT32


-- PrimitiveArrays represent draw calls and their buffer bindings.
--
-- Joining PrimitiveArrays that have the same primitive topology class is
-- possible with the Semigroup and Monoid instances, meaning there's still one
-- draw call per underlying vertex array, but fewer state changes than if
-- they were rendered separately (binding descriptors, etc).

newtype PrimitiveArray t a = PrimitiveArray {
  unPrimitiveArray :: [PrimitiveArrayDrawCall t a]
} deriving (Semigroup, Monoid)

data PrimitiveArrayDrawCall t a = PrimitiveArrayDrawCall {
  primitiveArrayTopology :: PrimitiveTopology t,
  primitiveArrayIndexed :: Indexed,
  primitiveArrayInstances :: Maybe (Int, Int), -- len, start
  primitiveArrayVertexBindingDescrs :: [VertexBindingDescr],
  primitiveArrayVertexAttrDescrs :: [VertexAttributeDescr],
  primitiveArrayVertexStart :: Int
}

-- Buffer, input rate, pointer offset (element skip), and stride
type VertexBindingDescr = (Vk.Buffer, InputRate, Int, Int)

-- Binding no, format, location (for GLSL), vertex offset
type VertexAttributeDescr = (Int, Vk.Format, Int, Int)

-- The IndexArray for the primitive geometry, or otherwise, how many vertices
-- are to be drawn when there is no index.
data Indexed =
    Indexed IndexArray
  | Unindexed Int


-- Primitive topologies - indexed by topology class.
data PrimitiveTopology t where
  LineList :: PrimitiveTopology Lines
  LineStrip :: PrimitiveTopology Lines
  PointList :: PrimitiveTopology Points
  TriangleFan :: PrimitiveTopology Triangles
  TriangleList :: PrimitiveTopology Triangles
  TriangleStrip :: PrimitiveTopology Triangles

data Lines
data Points
data Triangles


primitiveTopology :: PrimitiveTopology t -> Vk.PrimitiveTopology
primitiveTopology LineList      = Vk.PRIMITIVE_TOPOLOGY_LINE_LIST
primitiveTopology LineStrip     = Vk.PRIMITIVE_TOPOLOGY_LINE_STRIP
primitiveTopology PointList     = Vk.PRIMITIVE_TOPOLOGY_POINT_LIST
primitiveTopology TriangleFan   = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
primitiveTopology TriangleList  = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
primitiveTopology TriangleStrip = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP


class BaseTopology a where
  baseTopology :: a -> Vk.PrimitiveTopology

instance BaseTopology Lines where
  baseTopology _ = Vk.PRIMITIVE_TOPOLOGY_LINE_LIST

instance BaseTopology Points where
  baseTopology _ = Vk.PRIMITIVE_TOPOLOGY_POINT_LIST

instance BaseTopology Triangles where
  baseTopology _ = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST


toPrimitiveArray
  :: forall a t. (VertexInput a)
  => PrimitiveTopology t
  -> VertexArray a
  -> PrimitiveArray t a
toPrimitiveArray t (VertexArray f l s) =
  let a = f $ BufferValueIn 0 InputPerVertex
      ToVertex (Kleisli buildInputState) _ = toVertex
        :: ToVertex a (VertexFormat a)
      (input, bindings) = runInputM . buildInputState $ a
  in PrimitiveArray [
       PrimitiveArrayDrawCall t (Unindexed l) Nothing bindings input s
     ]

toPrimitiveArrayIndexed
  :: forall a t. (VertexInput a)
  => PrimitiveTopology t
  -> IndexArray
  -> VertexArray a
  -> PrimitiveArray t a
toPrimitiveArrayIndexed t ixs (VertexArray f _ s) =
  let a = f $ BufferValueIn 0 InputPerVertex
      ToVertex (Kleisli buildInputState) _ = toVertex
        :: ToVertex a (VertexFormat a)
      (input, bindings) = runInputM . buildInputState $ a
  in PrimitiveArray [
       PrimitiveArrayDrawCall t (Indexed ixs) Nothing bindings input s
     ]

toPrimitiveArrayInstanced
  :: forall a b c t. (VertexInput c)
  => PrimitiveTopology t
  -> VertexArray a
  -> VertexArray b
  -> (a -> b -> c)
  -> PrimitiveArray t c
toPrimitiveArrayInstanced t vertices instances f =
  let VertexArray aF aL aS = vertices
      VertexArray bF bL bS = instances
      cF x = f (aF x) (bF (x { bufferValueInRate = InputPerInstance }))
      c = cF $ BufferValueIn 0 InputPerVertex
      ToVertex (Kleisli buildInputState) _ = toVertex
        :: ToVertex c (VertexFormat c)
      (input, bindings) = runInputM . buildInputState $ c
  in PrimitiveArray [
       PrimitiveArrayDrawCall t (Unindexed aL) (Just (bL, bS)) bindings input aS
     ]

toPrimitiveArrayIndexedInstanced
  :: forall a b c t. (VertexInput c)
  => PrimitiveTopology t
  -> IndexArray
  -> VertexArray a
  -> VertexArray b
  -> (a -> b -> c)
  -> PrimitiveArray t c
toPrimitiveArrayIndexedInstanced t ixs vertices instances f =
  let VertexArray aF _  aS = vertices
      VertexArray bF bL bS = instances
      cF x = f (aF x) (bF (x { bufferValueInRate = InputPerInstance }))
      c = cF $ BufferValueIn 0 InputPerVertex
      ToVertex (Kleisli buildInputState) _ = toVertex
        :: ToVertex c (VertexFormat c)
      (input, bindings) = runInputM . buildInputState $ c
  in PrimitiveArray [
       PrimitiveArrayDrawCall t (Indexed ixs) (Just (bL, bS)) bindings input aS
     ]


-- A typeclass for transforming vertex data into vertices for use in a
-- `PrimitiveStream`.
class VertexInput a where
  type VertexFormat a
  toVertex :: ToVertex a (VertexFormat a)

data ToVertex a b = ToVertex
  -- Builds Vulkan attribute descriptions and bindings.
  --
  -- These depend on the value of the abstract vertices because we need to know
  -- the underlying buffers, strides, offsets and rates to build the input
  -- bindings.
  (Kleisli InputM a b)
  -- Builds GLSL vertex input declarations.
  --
  -- These don't depend on the value of the abstract vertices, which allows
  -- static compilation of shaders.
  --
  -- This arrow is expected to return a `VertexFormat a`.
  (Kleisli DeclM a b)

newtype InputM a = InputM {
  unInputM ::
    WriterT
      [VertexAttributeDescr]
      (State
        InputState
      )
      a
} deriving (Functor, Applicative, Monad, MonadState InputState,
    MonadWriter [VertexAttributeDescr])

type InputState = (
    Int, -- Next location
    Int, -- Next binding number
    Map  -- Bindings
      VertexBindingDescr
      Int
  )

runInputM :: InputM a -> ([VertexAttributeDescr], [VertexBindingDescr])
runInputM m =
  let (attrDescrs, (_, _, bindingsMap)) =
        flip runState (0, 0, mempty) . execWriterT . unInputM $ m
      bindings = fmap fst . sortBy (compare `on` snd) . M.assocs $ bindingsMap
  in (attrDescrs, bindings)


instance Category ToVertex where
  id = ToVertex id id
  (ToVertex a b) . (ToVertex a' b') = ToVertex (a . a') (b . b')

instance Arrow ToVertex where
  arr f = ToVertex (arr f) (arr f)
  first (ToVertex a b) = ToVertex (first a) (first b)

instance VertexInput (B ()) where
  type VertexFormat (B ()) = ()
  toVertex = proc _ -> returnA -< ()

bToVertex :: Vk.Format -> ByteString -> ToVertex (B a) (ExprM ByteString)
bToVertex format typ = ToVertex
  (Kleisli makeInputAttrDescr)
  (Kleisli makeGlslInputDecl)
 where
  makeInputAttrDescr B{..} = do
    (lNext, bNext, bindings) <- get
    let k = (bBufferHandle, bInputRate, bSkipElems * bStride, bStride)
        mB = M.lookup k bindings
        (binding', bNext', bindings') = case mB of
          Just binding -> (binding, bNext, bindings)
          Nothing ->
            let binding = bNext
            in (
              binding,
              bNext + 1,
              M.insert k binding bindings
            )
    tell [(binding', format, lNext, bOffset)]
    put (lNext + 1, bNext', bindings')
    return undefined
  makeGlslInputDecl _ = return <$> tellDecl typ

instance VertexInput (B Float) where
  type VertexFormat (B Float) = S V Float
  toVertex = bToVertex Vk.FORMAT_R32_SFLOAT "float" >>> arr S

instance VertexInput (B Word32) where
  type VertexFormat (B Word32) = S V Word32
  toVertex = bToVertex Vk.FORMAT_R32_UINT "uint" >>> arr S


instance VertexInput (B2 Float) where
  type VertexFormat (B2 Float) = V2 (S V Float)
  toVertex =
    arr unB2
      >>> bToVertex Vk.FORMAT_R32G32_SFLOAT "vec2"
      >>> arr toV2S'

instance VertexInput (B3 Float) where
  type VertexFormat (B3 Float) = V3 (S V Float)
  toVertex =
    arr unB3
      >>> bToVertex Vk.FORMAT_R32G32B32_SFLOAT "vec3"
      >>> arr toV3S'

instance VertexInput (B4 Float) where
  type VertexFormat (B4 Float) = V4 (S V Float)
  toVertex =
    arr unB4
      >>> bToVertex Vk.FORMAT_R32G32B32A32_SFLOAT "vec4"
      >>> arr toV4S'


instance VertexInput (V0 a) where
  type VertexFormat (V0 a) = V0 (VertexFormat a)
  toVertex = proc ~V0 -> returnA -< V0

instance VertexInput a => VertexInput (V1 a) where
  type VertexFormat (V1 a) = V1 (VertexFormat a)
  toVertex = proc ~(V1 a) -> do
    a' <- toVertex -< a
    returnA -< V1 a'

instance VertexInput a => VertexInput (V2 a) where
  type VertexFormat (V2 a) = V2 (VertexFormat a)
  toVertex = proc ~(V2 a b) -> do
    (a', b') <- toVertex -< (a, b)
    returnA -< V2 a' b'

instance VertexInput a => VertexInput (V3 a) where
  type VertexFormat (V3 a) = V3 (VertexFormat a)
  toVertex = proc ~(V3 a b c) -> do
    (a', b', c') <- toVertex -< (a, b, c)
    returnA -< V3 a' b' c'

instance VertexInput a => VertexInput (V4 a) where
  type VertexFormat (V4 a) = V4 (VertexFormat a)
  toVertex = proc ~(V4 a b c d) -> do
    (a', b', c', d') <- toVertex -< (a, b, c, d)
    returnA -< V4 a' b' c' d'


instance (VertexInput a, VertexInput b) => VertexInput (a, b) where
  type VertexFormat (a, b) = (VertexFormat a, VertexFormat b)
  toVertex = proc ~(a, b) -> do
    a' <- toVertex -< a
    b' <- toVertex -< b
    returnA -< (a', b')

instance (VertexInput a, VertexInput b, VertexInput c)
    => VertexInput (a, b, c) where
  type VertexFormat (a, b, c) =
    (VertexFormat a, VertexFormat b, VertexFormat c)
  toVertex = proc ~(a, b, c) -> do
    a' <- toVertex -< a
    b' <- toVertex -< b
    c' <- toVertex -< c
    returnA -< (a', b', c')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d)
    => VertexInput (a, b, c, d) where
  type VertexFormat (a, b, c, d) =
    (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d)
  toVertex = proc ~(a, b, c, d) -> do
    a' <- toVertex -< a
    b' <- toVertex -< b
    c' <- toVertex -< c
    d' <- toVertex -< d
    returnA -< (a', b', c', d')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d,
  VertexInput e)
    => VertexInput (a, b, c, d, e) where
  type VertexFormat (a, b, c, d, e) = (VertexFormat a, VertexFormat b,
    VertexFormat c, VertexFormat d, VertexFormat e)
  toVertex = proc ~(a, b, c, d, e) -> do
    a' <- toVertex -< a
    b' <- toVertex -< b
    c' <- toVertex -< c
    d' <- toVertex -< d
    e' <- toVertex -< e
    returnA -< (a', b', c', d', e')
