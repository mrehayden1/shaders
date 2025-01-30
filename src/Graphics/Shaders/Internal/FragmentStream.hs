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

import Data.Linear
import Graphics.Shaders.Internal.DeclM
import Graphics.Shaders.Internal.Expr

type GLPos = S V (V4 Float)

data FragmentStream a =
  FragmentStream
    a             -- Usually an S F x, our fragments.
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

instance FragmentInput (S V Float) where
  type FragmentFormat (S V Float) = S F Float
  toFragment = ToFragment
    (Kleisli $ \a -> do
      n <- lift $ tellDecl "float"
      out <- get
      put . S $ do
        _ <- unS out
        a' <- unS a
        tellStatement $ n <> " = " <> a'
        return n
      return . S $ return n
    )

instance FragmentInput (S V (V3 Float)) where
  type FragmentFormat (S V (V3 Float)) = S F (V3 Float)
  toFragment = ToFragment
    (Kleisli $ \a -> do
      n <- lift $ tellDecl "vec3"
      out <- get
      put . S $ do
        _ <- unS out
        a' <- unS a
        tellStatement $ n <> " = " <> a'
        return n
      return . S $ return n
    )

instance FragmentInput (S V (V4 Float)) where
  type FragmentFormat (S V (V4 Float)) = S F (V4 Float)
  toFragment = ToFragment
    (Kleisli $ \a -> do
      n <- lift $ tellDecl "vec4"
      out <- get
      put . S $ do
        _ <- unS out
        a' <- unS a
        tellStatement $ n <> " = " <> a'
        return n
      return . S $ return n
    )

instance (FragmentInput (S V a), FragmentInput (S V b))
    => FragmentInput (S V a, S V b) where
  type FragmentFormat (S V a, S V b) =
    (FragmentFormat (S V a), FragmentFormat (S V b))
  toFragment = proc ~(a, b) -> do
    a' <- toFragment -< a
    b' <- toFragment -< b
    returnA -< (a', b')

instance (FragmentInput (S V a), FragmentInput (S V b), FragmentInput (S V c))
    => FragmentInput (S V a, S V b, S V c) where
  type FragmentFormat (S V a, S V b, S V c) =
    (FragmentFormat (S V a), FragmentFormat (S V b), FragmentFormat (S V c))
  toFragment = proc ~(a, b, c) -> do
    a' <- toFragment -< a
    b' <- toFragment -< b
    c' <- toFragment -< c
    returnA -< (a', b', c')

instance (FragmentInput (S V a), FragmentInput (S V b), FragmentInput (S V c),
  FragmentInput (S V d))
    => FragmentInput (S V a, S V b, S V c, S V d) where
  type FragmentFormat (S V a, S V b, S V c, S V d) = (FragmentFormat (S V a),
    FragmentFormat (S V b), FragmentFormat (S V c), FragmentFormat (S V d))
  toFragment = proc ~(a, b, c, d) -> do
    a' <- toFragment -< a
    b' <- toFragment -< b
    c' <- toFragment -< c
    d' <- toFragment -< d
    returnA -< (a', b', c', d')

instance (FragmentInput (S V a), FragmentInput (S V b), FragmentInput (S V c),
  FragmentInput (S V d), FragmentInput (S V e))
    => FragmentInput (S V a, S V b, S V c, S V d, S V e) where
  type FragmentFormat (S V a, S V b, S V c, S V d, S V e) =
    (FragmentFormat (S V a), FragmentFormat (S V b), FragmentFormat (S V c),
     FragmentFormat (S V d), FragmentFormat (S V e))
  toFragment = proc ~(a, b, c, d, e) -> do
    a' <- toFragment -< a
    b' <- toFragment -< b
    c' <- toFragment -< c
    d' <- toFragment -< d
    e' <- toFragment -< e
    returnA -< (a', b', c', d', e')
