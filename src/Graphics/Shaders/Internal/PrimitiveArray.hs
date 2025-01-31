module Graphics.Shaders.Internal.PrimitiveArray (
  VertexArray(..),

  dropVertices,
  takeVertices,

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
  toPrimitiveArrayIndexedInstanced
) where

import Data.Word
import qualified Vulkan.Core10.Enums as VkEnum
import qualified Vulkan.Core10.Handles as Vk

import Graphics.Shaders.Internal.Buffer

-- Wrapper for Buffers that allows us to truncate
data VertexArray a = VertexArray {
  vertexArrayBufferHandle :: Vk.Buffer,
  vertexArrayLength :: Int, -- The number of vertices to take
  vertexArrayStart :: Int -- Pointer to the first vertex in the buffer
}

dropVertices :: Int -> VertexArray a -> VertexArray a
dropVertices n (VertexArray b l s) =
  let n' = min (max n 0) l
  in VertexArray b (l - n') (s + n')

takeVertices :: Int -> VertexArray a -> VertexArray a
takeVertices n (VertexArray b l s) = VertexArray b (min (max n 0) l) s

toVertexArray :: Buffer a -> VertexArray a
toVertexArray (Buffer h l) = VertexArray h l 0

-- TODO Restart indices.
data IndexArray = IndexArray {
  indexArrayBufferHandle :: Vk.Buffer,
  indexArrayLength :: Int,
  indexArrayStart :: Int,
  indexArrayType :: VkEnum.IndexType
}

class IndexInput a where
  indexType :: a -> VkEnum.IndexType

{- TODO Currently no support for buffering 16-bit data.
instance IndexInput (B Word16) where
  indexType _ = VkEnum.INDEX_TYPE_UINT16
-}

instance IndexInput (B Word32) where
  indexType _ = VkEnum.INDEX_TYPE_UINT32

dropIndices :: Int -> IndexArray -> IndexArray
dropIndices n (IndexArray h l s t) =
  let n' = min (max n 0) l
  in IndexArray h (l - n') (s + n') t

takeIndices :: Int -> IndexArray -> IndexArray
takeIndices n (IndexArray h l s t) = IndexArray h (min (max n 0) l) s t

toIndexArray :: forall b. (IndexInput b)
  => Buffer b
  -> IndexArray
toIndexArray (Buffer h l) = IndexArray h l 0 . indexType $ b
 where b = undefined :: b

-- PrimitiveArrays represent draw calls and their buffer bindings.
--
-- Joining PrimitiveArrays that have the same primitive topology clas is
-- possible with the Semigroup and Monoid instances results in one draw call
-- per underlying vertex array, but fewer state changes such as binding
-- descriptor sets and setting up the fixed function pipeline etc, than if
-- rendering them individually.

newtype PrimitiveArray t a b = PrimitiveArray {
  unPrimitiveArray :: [PrimitiveArrayDrawCall t a b]
} deriving (Semigroup, Monoid)

data PrimitiveArrayDrawCall t a b = PrimitiveArrayDrawCall {
  primitiveArrayTopology :: PrimitiveTopology t,
  primitiveArrayIndexed :: Indexed,
  primitiveArrayInstances :: Maybe (Vk.Buffer, Int, Int), -- buff, len, start
  primitiveArrayStart :: Int,
  primitiveArrayVertices :: Vk.Buffer
}

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

primitiveTopology :: PrimitiveTopology t -> VkEnum.PrimitiveTopology
primitiveTopology LineList      = VkEnum.PRIMITIVE_TOPOLOGY_LINE_LIST
primitiveTopology LineStrip     = VkEnum.PRIMITIVE_TOPOLOGY_LINE_STRIP
primitiveTopology PointList     = VkEnum.PRIMITIVE_TOPOLOGY_POINT_LIST
primitiveTopology TriangleFan   = VkEnum.PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
primitiveTopology TriangleList  = VkEnum.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
primitiveTopology TriangleStrip = VkEnum.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP


-- Primitive topology classes.
data Lines
data Points
data Triangles

class BaseTopology a where
  baseTopology :: a -> VkEnum.PrimitiveTopology

instance BaseTopology Lines where
  baseTopology _ = VkEnum.PRIMITIVE_TOPOLOGY_LINE_LIST

instance BaseTopology Points where
  baseTopology _ = VkEnum.PRIMITIVE_TOPOLOGY_POINT_LIST

instance BaseTopology Triangles where
  baseTopology _ = VkEnum.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST


toPrimitiveArray :: PrimitiveTopology t
  -> VertexArray a
  -> PrimitiveArray t a b
toPrimitiveArray t (VertexArray b l s) =
  PrimitiveArray [PrimitiveArrayDrawCall t (Unindexed l) Nothing s b]

toPrimitiveArrayIndexed :: PrimitiveTopology t
  -> IndexArray
  -> VertexArray a
  -> PrimitiveArray t a b
toPrimitiveArrayIndexed t ixs (VertexArray b _ s) =
  PrimitiveArray [PrimitiveArrayDrawCall t (Indexed ixs) Nothing s b]

toPrimitiveArrayInstanced :: PrimitiveTopology t
 -> VertexArray a
 -> VertexArray b
 -> PrimitiveArray t a b
toPrimitiveArrayInstanced t vertices instances =
  let VertexArray vB vL vS = vertices
      VertexArray iB iL iS = instances
  in PrimitiveArray [
       PrimitiveArrayDrawCall t (Unindexed vL) (Just (iB, iL, iS)) vS vB
     ]

toPrimitiveArrayIndexedInstanced :: PrimitiveTopology t
  -> IndexArray
  -> VertexArray a
  -> VertexArray b
  -> PrimitiveArray t a b
toPrimitiveArrayIndexedInstanced t ixs vertices instances =
  let VertexArray vB _  vS = vertices
      VertexArray iB iL iS = instances
  in PrimitiveArray [
       PrimitiveArrayDrawCall t (Indexed ixs) (Just (iB, iL, iS)) vS vB
     ]
