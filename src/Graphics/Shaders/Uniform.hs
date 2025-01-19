module Graphics.Shaders.Uniform (
  UniformInput(..),
  getUniform
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Data.ByteString.Char8 as BS
import Vulkan.Core10.DescriptorSet as VkDS
import Vulkan.Core10.DescriptorSet as VkDSLayout (DescriptorSetLayoutBinding(..))
import Vulkan.Core10.Enums as Vk
import Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Data.Linear
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Pipeline

class UniformInput a where
  type UniformFormat a
  toUniform :: ToUniform a (UniformFormat a)

newtype ToUniform a b = ToUniform (Kleisli DeclM a b)
 deriving (Category, Arrow)

-- Collects GLSL uniform declarations for a binding
type DeclM = ReaderT
  ByteString -- Uniform block name (used to prefix identifiers)
  (StateT
    Int -- Next identifier name
    (Writer
      [ByteString] -- GLSL declarations
    )
  )

-- Gets the uniform from the Pipeline environment.
--
-- Warning: Every call to getUniform will add a new uniform descriptor to the
-- descriptor layout. On many systems these can be extremely limited.
getUniform :: forall e a. UniformInput a
  => (e -> Buffer (Uniform a))
  -> Pipeline e (UniformFormat a)
getUniform getBuffer = do
  bind <- Pipeline getNext
  let n = BS.pack . show $ bind
      blockName = "un" <> n
      ToUniform (Kleisli buildDeclrs)
        = toUniform :: ToUniform a (UniformFormat a)
      (a, decls) = runWriter . flip evalStateT 0 . flip runReaderT blockName
                     . buildDeclrs $ (undefined :: a)
      decl = "layout (std140, binding = " <> n <> ") uniform UBO" <> n
                <> " {\n"
                <> "  " <> intercalate "\n  " decls
                <> "\n} " <> blockName <> ";"

      descriptorSetBinding = Vk.zero {
        VkDSLayout.binding = fromIntegral bind,
        VkDSLayout.descriptorCount = 1,
        VkDSLayout.descriptorType = VkDS.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        VkDSLayout.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
          .|. Vk.SHADER_STAGE_FRAGMENT_BIT
      }
  Pipeline . tell $ ([decl <> "\n"], [descriptorSetBinding])
  return a

toUniformField :: ByteString -> ToUniform a (S a)
toUniformField typ = ToUniform . Kleisli $ \_ -> do
  n <- BS.pack . show <$> getNext
  nm <- ask
  tell [typ <> " " <> nm <> "_" <> n <> ";"]
  return . S $ do
    return n

instance UniformInput Float where
  type UniformFormat Float = S Float
  toUniform = toUniformField "float"

instance UniformInput (V2 Float) where
  type UniformFormat (V2 Float) = S (V2 Float)
  toUniform = toUniformField "vec2"

instance UniformInput (V3 Float) where
  type UniformFormat (V3 Float) = S (V3 Float)
  toUniform = toUniformField "vec3"

instance UniformInput (V4 Float) where
  type UniformFormat (V4 Float) = S (V4 Float)
  toUniform = toUniformField "vec4"

instance UniformInput (M33 Float) where
  type UniformFormat (M33 Float) = S (M33 Float)
  toUniform = toUniformField "mat3"

instance UniformInput (M44 Float) where
  type UniformFormat (M44 Float) = S (M44 Float)
  toUniform = toUniformField "mat4"

instance (UniformInput a, UniformInput b) => UniformInput (a, b) where
  type UniformFormat (a, b) = (UniformFormat a, UniformFormat b)
  toUniform = proc ~(a, b) -> do
    a' <- toUniform -< a
    b' <- toUniform -< b
    returnA -< (a', b')

instance (UniformInput a, UniformInput b, UniformInput c)
    => UniformInput (a, b, c) where
  type UniformFormat (a, b, c)
         = (UniformFormat a, UniformFormat b, UniformFormat c)
  toUniform = proc ~(a, b, c) -> do
    (a', b') <- toUniform -< (a, b)
    c' <- toUniform -< c
    returnA -< (a', b', c')

instance (UniformInput a, UniformInput b, UniformInput c, UniformInput d)
    => UniformInput (a, b, c, d) where
  type UniformFormat (a, b, c, d)
         = (UniformFormat a, UniformFormat b, UniformFormat c, UniformFormat d)
  toUniform = proc ~(a, b, c, d) -> do
    (a', b', c') <- toUniform -< (a, b, c)
    d' <- toUniform -< d
    returnA -< (a', b', c', d')
