module Graphics.Shaders.Internal.PrimitiveStream (
  PrimitiveStream(..),

  VertexInput(..),
  ToVertex(..)
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString
import Data.Word
import Linear
import qualified Vulkan.Core10.Enums as VkEnum
import qualified Vulkan.Core10.Pipeline as VkAttrs (
  VertexInputAttributeDescription(..))
import qualified Vulkan.Zero as Vk

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

bToVertex :: VkEnum.Format -> ByteString -> ToVertex (B a) (ExprM ByteString)
bToVertex format typ = ToVertex
  (Kleisli makeInputAttrDescr)
  (Kleisli makeInputDecl)
 where
  makeInputAttrDescr B{..} = do
    l <- getNext
    tell [Vk.zero {
      VkAttrs.binding = fromIntegral bBinding,
      VkAttrs.format = format,
      VkAttrs.location = fromIntegral l,
      VkAttrs.offset = fromIntegral bOffset
    }]
    return undefined
  makeInputDecl _ = return <$> tellDecl typ


instance VertexInput (B Float) where
  type VertexFormat (B Float) = S V Float
  toVertex = bToVertex VkEnum.FORMAT_R32_SFLOAT "float" >>> arr S

instance VertexInput (B Word32) where
  type VertexFormat (B Word32) = S V Word32
  toVertex = bToVertex VkEnum.FORMAT_R32_UINT "uint" >>> arr S


instance VertexInput (B2 Float) where
  type VertexFormat (B2 Float) = V2 (S V Float)
  toVertex =
    arr unB2
      >>> bToVertex VkEnum.FORMAT_R32G32_SFLOAT "vec2"
      >>> arr toV2S'

instance VertexInput (B3 Float) where
  type VertexFormat (B3 Float) = V3 (S V Float)
  toVertex =
    arr unB3
      >>> bToVertex VkEnum.FORMAT_R32G32B32_SFLOAT "vec3"
      >>> arr toV3S'

instance VertexInput (B4 Float) where
  type VertexFormat (B4 Float) = V4 (S V Float)
  toVertex =
    arr unB4
      >>> bToVertex VkEnum.FORMAT_R32G32B32A32_SFLOAT "vec4"
      >>> arr toV4S'


instance VertexInput (V0 a) where
  type VertexFormat (V0 a) = V0 (VertexFormat a)
  toVertex = proc ~V0 -> returnA -< V0

instance VertexInput a => VertexInput (V1 a) where
  type VertexFormat (V1 a) = V1 (VertexFormat a)
  toVertex = proc ~(V1 a) -> do
    a' <- toVertex -< a
    returnA -< V1 a'

instance VertexInput a => VertexInput (V2 a) where
  type VertexFormat (V2 a) = V2 (VertexFormat a)
  toVertex = proc ~(V2 a b) -> do
    (a', b') <- toVertex -< (a, b)
    returnA -< V2 a' b'

instance VertexInput a => VertexInput (V3 a) where
  type VertexFormat (V3 a) = V3 (VertexFormat a)
  toVertex = proc ~(V3 a b c) -> do
    (a', b', c') <- toVertex -< (a, b, c)
    returnA -< V3 a' b' c'

instance VertexInput a => VertexInput (V4 a) where
  type VertexFormat (V4 a) = V4 (VertexFormat a)
  toVertex = proc ~(V4 a b c d) -> do
    (a', b', c', d') <- toVertex -< (a, b, c, d)
    returnA -< V4 a' b' c' d'


instance (VertexInput a, VertexInput b) => VertexInput (a, b) where
  type VertexFormat (a, b) = (VertexFormat a, VertexFormat b)
  toVertex = proc ~(a, b) -> do
    a' <- toVertex -< a
    b' <- toVertex -< b
    returnA -< (a', b')

instance (VertexInput a, VertexInput b, VertexInput c)
    => VertexInput (a, b, c) where
  type VertexFormat (a, b, c) =
    (VertexFormat a, VertexFormat b, VertexFormat c)
  toVertex = proc ~(a, b, c) -> do
    a' <- toVertex -< a
    b' <- toVertex -< b
    c' <- toVertex -< c
    returnA -< (a', b', c')

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d)
    => VertexInput (a, b, c, d) where
  type VertexFormat (a, b, c, d) =
    (VertexFormat a, VertexFormat b, VertexFormat c, VertexFormat d)
  toVertex = proc ~(a, b, c, d) -> do
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
  toVertex = proc ~(a, b, c, d, e) -> do
    a' <- toVertex -< a
    b' <- toVertex -< b
    c' <- toVertex -< c
    d' <- toVertex -< d
    e' <- toVertex -< e
    returnA -< (a', b', c', d', e')
