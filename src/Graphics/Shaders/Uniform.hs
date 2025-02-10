module Graphics.Shaders.Uniform (
  UniformInput(..),

  Uniform,
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
import qualified Data.Map as M
import System.Mem.StableName
import Vulkan.Core10.DescriptorSet as VkDS
import Vulkan.Core10.DescriptorSet as VkDSLayout (
  DescriptorSetLayoutBinding(..))
import Vulkan.Core10.Enums as Vk
import Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Data.Linear
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Internal.Pipeline

class UniformInput a where
  type UniformFormat a x
  toUniform :: ToUniform x a (UniformFormat a x)

newtype ToUniform x a b = ToUniform (Kleisli DeclM a b)
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
getUniform :: forall t e a x. UniformInput a
  => (e -> Buffer (Uniform a))
  -> Pipeline t e (UniformFormat a x)
getUniform getter = do
  -- Hash the getter StableName and look it up in the cache so we don't keep
  -- rebinding the uniform it's for.
  hash <- liftIO $ hashStableName <$> makeStableName getter
  bindings <- Pipeline . gets $ \(_, _, _, ubs, _) -> ubs
  let hasBinding = hash `M.member` bindings

  -- Get the uniform binding location.
  bind <- if hasBinding
            -- If it's already been bound return that,
            then return . uniformBindingNumber $ bindings M.! hash
            -- else use the next available one.
            else do
              (_, bind, _, _, _) <- Pipeline . update $
                \(n, un, ins, ubs, sbs) -> (n, un + 1, ins, ubs, sbs)
              return bind

  let bind' = BS.pack . show $ bind
      blockName = "un" <> bind'

      ToUniform (Kleisli buildDeclrs) =
        toUniform :: ToUniform x a (UniformFormat a x)
      (a, decls) = runWriter . flip evalStateT 0
        . flip runReaderT blockName . buildDeclrs $ (undefined :: a)

      decl = "layout(std140, binding = " <> bind' <> ") uniform UBO" <> bind'
        <> " {\n"
        <> "  " <> intercalate "\n  " decls <> "\n"
        <> "} " <> blockName <> ";"

      descrSetLayoutBinding = Vk.zero {
        VkDSLayout.binding = fromIntegral bind,
        VkDSLayout.descriptorCount = 1,
        VkDSLayout.descriptorType = VkDS.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        VkDSLayout.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
          .|. Vk.SHADER_STAGE_FRAGMENT_BIT
      }

  -- Create the uniform binding if it doesn't already have one.
  unless hasBinding . Pipeline $ do
    modify $ \(n, un, ins, ubs, sbs) ->
      let ub = UniformBinding {
        uniformBindingNumber = bind,
        uniformBufferGetter = BufferGetter getter,
        uniformDeclaration = decl <> "\n",
        uniformDescrSetLayoutBinding = descrSetLayoutBinding
      }
      in (n, un, ins, M.insert hash ub ubs, sbs)

  return a

toUniformField :: ByteString -> ToUniform x (B a) (S x a)
toUniformField typ = ToUniform . Kleisli $ \_ -> do
  n <- BS.pack . ("_" <>) . show <$> getNext
  blockname <- ask
  tell [typ <> " " <> n <> ";"]
  return . S $ do
    return $ blockname <> "." <> n

instance UniformInput (B Float) where
  type UniformFormat (B Float) x = S x Float
  toUniform = toUniformField "float"

instance UniformInput (B (V2 Float)) where
  type UniformFormat (B (V2 Float)) x = S x (V2 Float)
  toUniform = toUniformField "vec2"

instance UniformInput (B (V3 Float)) where
  type UniformFormat (B (V3 Float)) x = S x (V3 Float)
  toUniform = toUniformField "vec3"

instance UniformInput (B (V4 Float)) where
  type UniformFormat (B (V4 Float)) x = S x (V4 Float)
  toUniform = toUniformField "vec4"

instance UniformInput (B (M33 Float)) where
  type UniformFormat (B (M33 Float)) x = S x (M33 Float)
  toUniform = toUniformField "mat3"

instance UniformInput (B (M44 Float)) where
  type UniformFormat (B (M44 Float)) x = S x (M44 Float)
  toUniform = toUniformField "mat4"

instance (UniformInput a, UniformInput b) => UniformInput (a, b) where
  type UniformFormat (a, b) x = (UniformFormat a x, UniformFormat b x)
  toUniform = proc ~(a, b) -> do
    a' <- toUniform -< a
    b' <- toUniform -< b
    returnA -< (a', b')

instance (UniformInput a, UniformInput b, UniformInput c)
    => UniformInput (a, b, c) where
  type UniformFormat (a, b, c) x =
    (UniformFormat a x, UniformFormat b x, UniformFormat c x)
  toUniform = proc ~(a, b, c) -> do
    (a', b') <- toUniform -< (a, b)
    c' <- toUniform -< c
    returnA -< (a', b', c')

instance (UniformInput a, UniformInput b, UniformInput c, UniformInput d)
    => UniformInput (a, b, c, d) where
  type UniformFormat (a, b, c, d) x =
    (UniformFormat a x, UniformFormat b x, UniformFormat c x,
     UniformFormat d x)
  toUniform = proc ~(a, b, c, d) -> do
    (a', b', c') <- toUniform -< (a, b, c)
    d' <- toUniform -< d
    returnA -< (a', b', c', d')
