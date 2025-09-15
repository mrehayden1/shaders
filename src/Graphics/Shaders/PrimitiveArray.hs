module Graphics.Shaders.PrimitiveArray (
  VertexArray,

  dropVertices,
  takeVertices,
  zipVertices,

  toVertexArray,

  IndexArray,

  dropIndices,
  takeIndices,

  toIndexArray,

  PrimitiveArray,

  PrimitiveTopology(..),

  Lines,
  Points,
  Triangles,

  toPrimitiveArray,
  toPrimitiveArrayIndexed,
  toPrimitiveArrayInstanced,
  toPrimitiveArrayIndexedInstanced
) where

import Graphics.Shaders.Internal.PrimitiveArray
