module Graphics.Shaders.Internal.DeclM (
  DeclM,
  InOut(..),

  tellDecl
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Monad.State.Extra

type DeclM = ReaderT InOut (StateT Int (Writer ByteString))

data InOut = In | Out

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
