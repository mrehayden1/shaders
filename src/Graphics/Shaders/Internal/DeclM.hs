module Graphics.Shaders.Internal.DeclM (
  DeclM,
  InOut(..),

  runDeclM,

  tellDecl
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Monad.State.Extra

-- A monad for creating GLSL shader input/output declarations
newtype DeclM a = DeclM {
  unDeclM :: ReaderT InOut (StateT Int (Writer ByteString)) a
} deriving (Functor, Applicative, Monad, MonadState Int, MonadReader InOut,
    MonadWriter ByteString)

data InOut = In | Out

runDeclM :: DeclM a -> InOut -> (a, ByteString)
runDeclM m i = runWriter . flip evalStateT 0 . flip runReaderT i . unDeclM $ m

tellDecl :: ByteString -> DeclM ByteString
tellDecl typ = do
  loc <- getNext
  io <- ask
  let inOut = case io of { In -> "in"; Out -> "out" }
      loc' = BS.pack . show $ loc
      name = inOut <> loc'
  tell $ "layout(location = " <> loc' <> ") " <> inOut <> " " <> typ <> " "
           <> name <> ";\n"
  return name
