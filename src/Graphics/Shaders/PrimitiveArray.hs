module Graphics.Shaders.PrimitiveArray (
  VertexArray,
  vertexArrayLength,

  dropVertices,
  takeVertices,

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
