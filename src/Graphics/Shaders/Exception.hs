module Graphics.Shaders.Exception (
  ShadersException(..)
) where

import Control.Exception

data ShadersException = ShadersInitializationException String
  | ShadersMemoryException String
 deriving (Show)

instance Exception ShadersException
