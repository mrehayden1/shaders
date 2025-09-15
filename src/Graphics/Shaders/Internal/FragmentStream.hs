module Graphics.Shaders.Internal.FragmentStream (
  GLPos,

  FragmentStream(..),
  Rasterization(..),

  FragmentInput(..),
  ToFragment(..)
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.State
import Data.ByteString (ByteString)
import Linear

import Graphics.Shaders.Internal.DeclM
import Graphics.Shaders.Internal.Expr

type GLPos = V4 (S V Float)

data FragmentStream a =
  FragmentStream
    a             -- Our fragments
    Rasterization -- Rasterization

instance Functor FragmentStream where
  fmap f (FragmentStream a r) = FragmentStream (f a) r

data Rasterization = Rasterization
  Int        -- Input name
  (S V ())   -- Vertex shader body
  GLPos
  ByteString -- Vertex shader output declarations
  ByteString -- Fragment input declarations

class FragmentInput a where
  type FragmentFormat a
  toFragment :: ToFragment a (FragmentFormat a)

newtype ToFragment a b = ToFragment
  -- Vertex shader output assignments (combined into a S V ()) and GLSL
  -- declarations.
  (Kleisli (StateT (S V ()) DeclM) a b)

instance Category ToFragment where
  id = ToFragment id
  (ToFragment a) . (ToFragment b) = ToFragment (a . b)

instance Arrow ToFragment where
  arr = ToFragment . arr
  first (ToFragment a) = ToFragment (first a)

toFragmentBasic :: ByteString -> ToFragment (S V a) (S F a)
toFragmentBasic typ = ToFragment
  (Kleisli $ \a -> do
    n <- lift $ tellDecl typ
    out <- get
    put . S $ do
      _ <- unS out
      a' <- unS a
      tellStatement $ n <> " = " <> a'
      return n
    return . S $ return n
  )

instance FragmentInput (S V Float) where
  type FragmentFormat (S V Float) = S F Float
  toFragment = toFragmentBasic "float"


instance FragmentInput (V0 a) where
  type FragmentFormat (V0 a) = V0 (FragmentFormat a)
  toFragment = proc ~V0 -> returnA -< V0

instance FragmentInput a => FragmentInput (V1 a) where
  type FragmentFormat (V1 a) = V1 (FragmentFormat a)
  toFragment = proc ~(V1 a) -> do
    a' <- toFragment -< a
    returnA -< V1 a'

instance FragmentInput a => FragmentInput (V2 a) where
  type FragmentFormat (V2 a) = V2 (FragmentFormat a)
  toFragment = proc ~(V2 a b) -> do
    (a', b') <- toFragment -< (a, b)
    returnA -< V2 a' b'

instance FragmentInput a => FragmentInput (V3 a) where
  type FragmentFormat (V3 a) = V3 (FragmentFormat a)
  toFragment = proc ~(V3 a b c) -> do
    (a', b', c') <- toFragment -< (a, b, c)
    returnA -< V3 a' b' c'

instance FragmentInput a => FragmentInput (V4 a) where
  type FragmentFormat (V4 a) = V4 (FragmentFormat a)
  toFragment = proc ~(V4 a b c d) -> do
    (a', b', c', d') <- toFragment -< (a, b, c, d)
    returnA -< V4 a' b' c' d'


instance (FragmentInput a, FragmentInput b)
    => FragmentInput (a, b) where
  type FragmentFormat (a, b) =
    (FragmentFormat a, FragmentFormat b)
  toFragment = proc ~(a, b) -> do
    a' <- toFragment -< a
    b' <- toFragment -< b
    returnA -< (a', b')

instance (FragmentInput a, FragmentInput b, FragmentInput c)
    => FragmentInput (a, b, c) where
  type FragmentFormat (a, b, c) =
    (FragmentFormat a, FragmentFormat b, FragmentFormat c)
  toFragment = proc ~(a, b, c) -> do
    (a', b') <- toFragment -< (a, b)
    c' <- toFragment -< c
    returnA -< (a', b', c')

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d)
    => FragmentInput (a, b, c, d) where
  type FragmentFormat (a, b, c, d) =
    (FragmentFormat a, FragmentFormat b, FragmentFormat c, FragmentFormat d)
  toFragment = proc ~(a, b, c, d) -> do
    (a', b', c') <- toFragment -< (a, b, c)
    d' <- toFragment -< d
    returnA -< (a', b', c', d')

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d, FragmentInput e)
    => FragmentInput (a, b, c, d, e) where
  type FragmentFormat (a, b, c, d, e) =
    (FragmentFormat a, FragmentFormat b, FragmentFormat c, FragmentFormat d, FragmentFormat e)
  toFragment = proc ~(a, b, c, d, e) -> do
    (a', b', c', d') <- toFragment -< (a, b, c, d)
    e' <- toFragment -< e
    returnA -< (a', b', c', d', e')
