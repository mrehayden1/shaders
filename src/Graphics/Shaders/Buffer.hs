module Graphics.Shaders.Buffer (
  Buffer,
  BufferAccess(..),

  B,
  B2,
  B3,
  B4,

  BufferFormat(..),
  ToBuffer(..),

  createBuffer,
  createBufferReadOnly,

  destroyBuffer,

  writeBuffer
) where

import Graphics.Shaders.Internal.Buffer
