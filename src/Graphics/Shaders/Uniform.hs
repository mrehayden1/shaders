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
import Linear
import System.Mem.StableName
import Vulkan.Core10.DescriptorSet as VkDS
import Vulkan.Core10.DescriptorSet as VkDSLayout (
  DescriptorSetLayoutBinding(..))
import Vulkan.Core10.Enums as Vk
import Vulkan.Zero as Vk

import Control.Monad.State.Extra
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
getUniform :: forall t c e a x. UniformInput a
  => (e -> Buffer (Uniform a))
  -> PipelineBuilder t c e (UniformFormat a x)
getUniform getter = do
  -- Hash the getter StableName and look it up in the cache so we don't keep
  -- rebinding the uniform it's for.
  hash <- liftIO . fmap hashStableName . makeStableName $! getter
  bindings <- PipelineBuilder . gets $ \(_, _, _, ubs, _, _) -> ubs
  let hasBinding = hash `M.member` bindings

  -- Get the uniform binding location.
  bind <- if hasBinding
    -- If it's already been bound return that,
    then return . uniformBindingNumber $ bindings M.! hash
    -- else use the next available one.
    else do
      (_, bind, _, _, _, _) <- PipelineBuilder . update $
        \(n, un, ins, ubs, sbs, pc) -> (n, un + 1, ins, ubs, sbs, pc)
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
  unless hasBinding . PipelineBuilder $ do
    modify $ \(n, un, ins, ubs, sbs, pc) ->
      let ub = UniformBinding {
        uniformBindingNumber = bind,
        uniformBufferGetter = BufferGetter getter,
        uniformDeclaration = decl <> "\n",
        uniformDescrSetLayoutBinding = descrSetLayoutBinding
      }
      in (n, un, ins, M.insert hash ub ubs, sbs, pc)

  return a

toUniformField :: ByteString -> ToUniform x (B a) (ExprM ByteString)
toUniformField typ = ToUniform . Kleisli $ \_ -> do
  n <- BS.pack . ("_" <>) . show <$> getNext
  blockname <- ask
  tell [typ <> " " <> n <> ";"]
  return . return $ blockname <> "." <> n

instance UniformInput (B Float) where
  type UniformFormat (B Float) x = S x Float
  toUniform = toUniformField "float" >>> arr S

instance UniformInput (B2 Float) where
  type UniformFormat (B2 Float) x = V2 (S x Float)
  toUniform = arr unB2 >>> toUniformField "vec2" >>> arr toV2S'

instance UniformInput (B3 Float) where
  type UniformFormat (B3 Float) x = V3 (S x Float)
  toUniform = arr unB3 >>> toUniformField "vec3" >>> arr toV3S'

instance UniformInput (B4 Float) where
  type UniformFormat (B4 Float) x = V4 (S x Float)
  toUniform = arr unB4 >>> toUniformField "vec4" >>> arr toV4S'


instance UniformInput (V0 a) where
  type UniformFormat (V0 a) x = V0 (UniformFormat a x)
  toUniform = proc ~V0 -> returnA -< V0

instance UniformInput a => UniformInput (V1 a) where
  type UniformFormat (V1 a) x = V1 (UniformFormat a x)
  toUniform = proc ~(V1 a) -> do
    a' <- toUniform -< a
    returnA -< V1 a'

instance UniformInput a => UniformInput (V2 a) where
  type UniformFormat (V2 a) x = V2 (UniformFormat a x)
  toUniform = proc ~(V2 a b) -> do
    (a', b') <- toUniform -< (a, b)
    returnA -< V2 a' b'

instance UniformInput a => UniformInput (V3 a) where
  type UniformFormat (V3 a) x = V3 (UniformFormat a x)
  toUniform = proc ~(V3 a b c) -> do
    (a', b', c') <- toUniform -< (a, b, c)
    returnA -< V3 a' b' c'

instance UniformInput a => UniformInput (V4 a) where
  type UniformFormat (V4 a) x = V4 (UniformFormat a x)
  toUniform = proc ~(V4 a b c d) -> do
    (a', b', c', d') <- toUniform -< (a, b, c, d)
    returnA -< V4 a' b' c' d'


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
