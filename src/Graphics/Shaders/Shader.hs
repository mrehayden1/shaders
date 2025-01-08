module Graphics.Shaders.Shader (
  Shader
) where

import Control.Monad.Reader

newtype Shader e a = Shader {
  unShader :: Reader e a
} deriving (Functor, Applicative, Monad)


