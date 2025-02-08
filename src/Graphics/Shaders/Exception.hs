module Graphics.Shaders.Exception (
  ShadersException(..)
) where

import Control.Exception

data ShadersException = ShadersInitializationException String
  | ShadersMemoryException String
  | ShadersUnsupportedTransitionException
 deriving (Show)

instance Exception ShadersException
