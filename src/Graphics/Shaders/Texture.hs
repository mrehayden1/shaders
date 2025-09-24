module Graphics.Shaders.Texture (
  TextureSampler(..),
  createTextureSampler,
  destroyTextureSampler,
  TextureFilter(..),
  TextureWrap(..),
  TextureMipmapMode(..),

  Texture,
  createTexture,
  destroyTexture
) where

import Graphics.Shaders.Internal.Texture
