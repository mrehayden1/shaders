module Graphics.Shaders.Internal.PrimitiveStream (
  PrimitiveStream(..),

  VertexInput(..),
  ToVertex(..)
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Vulkan.Core10.Enums as VkEnum
import qualified Vulkan.Core10.Pipeline as VkAttrs (
  VertexInputAttributeDescription(..))
import qualified Vulkan.Zero as Vk

import Data.Linear
import Control.Monad.State.Extra
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.DeclM
import Graphics.Shaders.Internal.Expr

data PrimitiveStream t a =
  PrimitiveStream
    Int -- Input name
    a   -- An S V x, representing our vertices and encoding the shader.

instance Functor (PrimitiveStream t) where
  fmap f (PrimitiveStream n a) = PrimitiveStream n (f a)

-- Represents 4 byte aligned data
class VertexInput a where
  type VertexFormat a
  toVertex :: ToVertex a (VertexFormat a)

data ToVertex a b = ToVertex
  -- Builds Vulcan attribute descriptions
  (Kleisli AttrDescrM a b)
  -- Builds GLSL vertex input declarations
  (Kleisli DeclM a b)

type AttrDescrM =
  (WriterT
    [VkAttrs.VertexInputAttributeDescription]
    -- Location
    (State Int)
  )

instance Category ToVertex where
  id = ToVertex id id
  (ToVertex a b) . (ToVertex a' b') = ToVertex (a . a') (b . b')

instance Arrow ToVertex where
  arr f = ToVertex (arr f) (arr f)
  first (ToVertex a b) = ToVertex (first a) (first b)

instance VertexInput (B ()) where
  type VertexFormat (B ()) = ()
  toVertex = proc _ -> returnA -< ()

bToVertex :: VkEnum.Format -> B a -> AttrDescrM b
bToVertex format B{..} = do
  l <- getNext
  tell [Vk.zero {
    VkAttrs.binding = fromIntegral bBinding,
    VkAttrs.format = format,
    VkAttrs.location = fromIntegral l,
    VkAttrs.offset = fromIntegral bOffset
  }]
  return undefined

instance VertexInput (B Float) where
  type VertexFormat (B Float) = S V Float
  toVertex = ToVertex
    (Kleisli $ bToVertex VkEnum.FORMAT_R32_SFLOAT)
    (Kleisli . const $ do
       S . return <$> tellDecl "float"
    )

instance VertexInput (B (V2 Float)) where
  type VertexFormat (B (V2 Float)) = S V (V2 Float)
  toVertex = ToVertex
    (Kleisli $ bToVertex VkEnum.FORMAT_R32G32_SFLOAT)
    (Kleisli . const $ do
       S . return <$> tellDecl "vec2"
    )

instance VertexInput (B (V3 Float)) where
  type VertexFormat (B (V3 Float)) = S V (V3 Float)
  toVertex = ToVertex
    (Kleisli $ bToVertex VkEnum.FORMAT_R32G32B32_SFLOAT)
    (Kleisli . const $ do
       S . return <$> tellDecl "vec3"
    )

instance VertexInput (B (V4 Float)) where
  type VertexFormat (B (V4 Float)) = S V (V4 Float)
  toVertex = ToVertex
    (Kleisli $ bToVertex VkEnum.FORMAT_R32G32B32A32_SFLOAT)
    (Kleisli . const $ do
       S . return <$> tellDecl "vec4"
    )

instance (VertexInput a, VertexInput b) => VertexInput (a, b) where
  type VertexFormat (a, b) = (VertexFormat a, VertexFormat b)
  toVertex =
    proc ~(a, b) -> do
      a' <- toVertex -< a
      b' <- toVertex -< b
      returnA -< (a', b')

instance (VertexInput a, VertexInput b, VertexInput c)
    => VertexInput (a, b, c) where
  type VertexFormat (a, b, c) =
    (VertexFormat a, VertexFormat b, VertexFormat c)
  toVertex =
    proc ~(a, b, c) -> do
      a' <- toVertex -< a
      b' <- toVertex -< b
      c' <- toVertex -< c
      returnA -< (a', b', c')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d)
    => VertexInput (a, b, c, d) where
  type VertexFormat (a, b, c, d) =
    (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d)
  toVertex =
    proc ~(a, b, c, d) -> do
      a' <- toVertex -< a
      b' <- toVertex -< b
      c' <- toVertex -< c
      d' <- toVertex -< d
      returnA -< (a', b', c', d')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d,
  VertexInput e)
    => VertexInput (a, b, c, d, e) where
  type VertexFormat (a, b, c, d, e) = (VertexFormat a, VertexFormat b,
    VertexFormat c, VertexFormat d, VertexFormat e)
  toVertex =
    proc ~(a, b, c, d, e) -> do
      a' <- toVertex -< a
      b' <- toVertex -< b
      c' <- toVertex -< c
      d' <- toVertex -< d
      e' <- toVertex -< e
      returnA -< (a', b', c', d', e')
