module Graphics.Shaders.Pipeline (
  S(..),
  Pipeline(..),

  withPipeline
) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.Word
import qualified Data.Vector as V
import qualified Language.SpirV.Internal as SpirV
import qualified Language.SpirV.ShaderKind as SpirV
import qualified Language.SpirV.Shaderc as SpirV
import qualified Language.SpirV.Shaderc.CompileOptions as SpirV
import Text.Printf
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.Pipeline as VkAttrs (
  VertexInputAttributeDescription(..))
import qualified Vulkan.Core10.Pipeline as VkBinding (
  VertexInputBindingDescription(..))
import qualified Vulkan.Core10.Pipeline as VkPipeline hiding (
  ComputePipelineCreateInfo(..))
import Vulkan.CStruct.Extends (SomeStruct(..))
import qualified Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Data.Linear
import Graphics.Shaders.Base
import Graphics.Shaders.Buffer
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Logger.Class

newtype Pipeline vIn vOut = Pipeline {
  pipelineHandle :: Vk.Pipeline
}

withPipeline :: forall input output bIn bOut sIn sOut m.
 (MonadAsyncException m, MonadLogger m,
  Bufferable input, BufferFormat input ~ bIn,
  Bufferable output, BufferFormat output ~ bOut,
  VertexInput bIn, VertexFormat bIn ~ sIn,
  VertexInput bOut, VertexFormat bOut ~ sOut,
  VertexOutput sOut)
  => (sIn -> (S (V4 Float), sOut))
  -> ShadersT m (Pipeline input output)
withPipeline vertexShader = do
  deviceHandle <- getDeviceHandle
  renderPass <- getRenderPass
  vertexShaderModule <- createVertexShader @bIn @bOut deviceHandle vertexShader
  fragmentShaderModule <- createFragmentShader deviceHandle

  let ToBuffer (Kleisli calcStride) _ = toBuffer
      (b, stride) = flip runState 0 . calcStride $ (undefined :: input)

  let ToVertex (Kleisli buildVertexAttrDescrs) _
        = toVertex :: ToVertex (BufferFormat input)
                               (VertexFormat (BufferFormat input))

  let (vertexAttrDescrs, _) = flip runState (0, 0) . execWriterT
        . buildVertexAttrDescrs $ b

  debug "Creating pipeline layout."
  layout <- fromCps $ bracket
    (Vk.createPipelineLayout deviceHandle Vk.zero Nothing)
    (\l -> do
        debug "Destroying pipeline layout."
        Vk.destroyPipelineLayout deviceHandle l Nothing)

  let pipelineCreateInfos = V.singleton . SomeStruct $ Vk.zero {
          VkPipeline.colorBlendState = Just . SomeStruct $ Vk.zero {
            VkPipeline.attachments = V.singleton $ Vk.zero {
              VkPipeline.colorWriteMask = Vk.COLOR_COMPONENT_R_BIT
                .|. Vk.COLOR_COMPONENT_G_BIT
                .|. Vk.COLOR_COMPONENT_B_BIT
                .|. Vk.COLOR_COMPONENT_A_BIT
            },
            VkPipeline.attachmentCount = 1
          },
          VkPipeline.dynamicState = Just $ Vk.zero {
            VkPipeline.dynamicStates = V.fromList [
              VkPipeline.DYNAMIC_STATE_VIEWPORT,
              VkPipeline.DYNAMIC_STATE_SCISSOR
            ]
          },
          VkPipeline.inputAssemblyState = Just $ Vk.zero {
            VkPipeline.topology = VkPipeline.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
          },
          VkPipeline.layout = layout,
          VkPipeline.multisampleState = Just . SomeStruct $ Vk.zero {
            VkPipeline.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
          },
          VkPipeline.rasterizationState = Just . SomeStruct $ Vk.zero {
            VkPipeline.cullMode = VkPipeline.CULL_MODE_BACK_BIT,
            VkPipeline.lineWidth = 1,
            VkPipeline.polygonMode = VkPipeline.POLYGON_MODE_FILL
          },
          VkPipeline.renderPass = renderPass,
          VkPipeline.vertexInputState = Just . SomeStruct $ Vk.zero {
            VkPipeline.vertexAttributeDescriptions =
              V.fromList vertexAttrDescrs,
            VkPipeline.vertexBindingDescriptions = V.singleton $ Vk.zero {
              VkBinding.binding = 0,
              VkBinding.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX,
              VkBinding.stride = fromIntegral stride
            }
          },
          VkPipeline.viewportState = Just . SomeStruct $ Vk.zero {
            VkPipeline.scissorCount = 1,
            VkPipeline.viewportCount = 1
          },
          VkPipeline.stageCount = 2,
          VkPipeline.stages = V.fromList . fmap SomeStruct $ [
            Vk.zero {
              VkPipeline.stage = Vk.SHADER_STAGE_VERTEX_BIT,
              VkPipeline.module' = vertexShaderModule,
              VkPipeline.name = "main"
            },
            Vk.zero {
              VkPipeline.stage = Vk.SHADER_STAGE_FRAGMENT_BIT,
              VkPipeline.module' = fragmentShaderModule,
              VkPipeline.name = "main"
            }
          ]
        }

  debug "Creating pipeline."
  vkPipeline <- fromCps $ bracket
    -- We don't care about the result type since we're not preventing
    -- compilation.
    (do (result, pipelines) <-
          VkPipeline.createGraphicsPipelines deviceHandle Vk.zero
            pipelineCreateInfos Nothing
        when (result /= Vk.SUCCESS) $
          warn . printf "Non success result: %s" . show $ result
        return . V.head $ pipelines
    )
    (\p -> do
      debug "Destroying pipeline."
      VkPipeline.destroyPipeline deviceHandle p Nothing
    )

  return $ Pipeline vkPipeline

createVertexShader :: forall bIn bOut sIn sOut m. (
    MonadAsyncException m,
    MonadLogger m,
    VertexInput bIn,
    VertexFormat bIn ~ sIn,
    VertexInput bOut,
    VertexFormat bOut ~ sOut,
    VertexOutput sOut)
  => Vk.Device
  -> (sIn -> (S (V4 Float), sOut))
  -> ShadersT m Vk.ShaderModule
createVertexShader device shaderFn = do
  let ToVertex _ (Kleisli buildInDecls)
        = toVertex :: ToVertex bIn sIn
      ToGLSL (Kleisli buildOutDecls)
        = toGLSL :: ToGLSL sOut (S ())

  let (input, inDecls) = runWriter . flip evalStateT 0 . flip runReaderT In
        . buildInDecls $ (undefined :: bIn)
      (glPos, output) = shaderFn input
      (body, outDecls) = runWriter . flip evalStateT 0 . flip runReaderT Out
        . buildOutDecls $ output

  let code = "#version 460\n\n"
        <> inDecls <> "\n"
        <> outDecls <> "\n"
        <> "void main() {\n"
        <> execExprM 2 (do
               _ <- unS body
               n <- unS glPos
               tellExpr $ "gl_Position = " <> n
               return undefined
             )
        <> "}"

  debug "Compiling vertex shader source."
  trace . BS.unpack $ "Dumping vertex shader source: \n" <> code
  (SpirV.S compiledCode :: SpirV.S 'SpirV.VertexShader) <-
    liftIO $ SpirV.compile code "" "main" (def :: SpirV.C ())
  let createInfo = Vk.zero {
        Vk.code = compiledCode
      }
  debug "Creating vertex shader module."
  fromCps $ bracket
    (Vk.createShaderModule device createInfo Nothing)
    (\s -> do
      debug "Destroying vertex shader module."
      Vk.destroyShaderModule device s Nothing
    )

createFragmentShader :: (MonadAsyncException m, MonadLogger m)
  => Vk.Device
  -> ShadersT m Vk.ShaderModule
createFragmentShader device = do
  debug "Compiling fragment shader source."
  (SpirV.S compiledCode :: SpirV.S 'SpirV.FragmentShader) <-
    liftIO $ SpirV.compile code "" "main" (def :: SpirV.C ())
  let createInfo = Vk.zero {
        Vk.code = compiledCode
      }
  debug "Creating fragment shader module."
  fromCps $ bracket
    (Vk.createShaderModule device createInfo Nothing)
    (\s -> do
      debug "Destroying fragment shader module."
      Vk.destroyShaderModule device s Nothing
    )
 where
  code :: ByteString
  code  = "\
    \ #version 460\n\n\
    \ layout(location = 0) in vec3 fragColor;\n\n\
    \ layout(location = 0) out vec4 outColor;\n\n\
    \ void main() {\n\
    \   outColor = vec4(fragColor, 1.0);\n\
    \ }"

-- Constraint: vertices are 4 byte aligned
class VertexInput a where
  type VertexFormat a
  toVertex :: ToVertex a (VertexFormat a)

data ToVertex a b = ToVertex
  -- Builds Vulcan attribute descriptions
  (Kleisli AttrDescrM a b)
  -- Builds GLSL declarations
  (Kleisli DeclM a b)

instance Category ToVertex where
  id = ToVertex id id
  (ToVertex a b) . (ToVertex a' b') = ToVertex (a . a') (b . b')

instance Arrow ToVertex where
  arr f = ToVertex (arr f) (arr f)
  first (ToVertex a b) = ToVertex (first a) (first b)

type AttrDescrM =
  WriterT
    [VkAttrs.VertexInputAttributeDescription]
    -- Offset, location
    (State (Word32, Word32))

type DeclM = ReaderT InOut (StateT Int (Writer ByteString))

tellDecl :: ByteString -> DeclM ByteString
tellDecl typ = do
  loc <- getNext
  io <- ask
  let inOut = case io of { In -> "in"; Out -> "out" }
      loc' = BS.pack . show $ loc
      name = inOut <> loc'
  tell $ "layout(location=" <> loc' <> ") " <> inOut <> " " <> typ <> " "
           <> name <> ";\n"
  return name

data InOut = In | Out

instance VertexInput (B Float) where
  type VertexFormat (B Float) = S Float
  toVertex = ToVertex
    (Kleisli $ \B{..} -> do
       (off, i) <- get
       tell [Vk.zero {
         VkAttrs.binding = 0,
         VkAttrs.format = Vk.FORMAT_R32_SFLOAT,
         VkAttrs.location = i,
         VkAttrs.offset = off
       }]
       let off' = off + fromIntegral bStride
           i'   = i + 1
       put (off', i')
       return undefined
    )
    (Kleisli . const $ do
       S . return <$> tellDecl "float"
    )

instance VertexInput (B (V2 Float)) where
  type VertexFormat (B (V2 Float)) = S (V2 Float)
  toVertex = ToVertex
    (Kleisli $ \B{..} -> do
       (offset, loc) <- update $
         \(o, l) -> (o + fromIntegral bStride, l + 1)
       tell [Vk.zero {
         VkAttrs.binding = 0,
         VkAttrs.format = Vk.FORMAT_R32G32_SFLOAT,
         VkAttrs.location = loc,
         VkAttrs.offset = offset
       }]
       return undefined
    )
    (Kleisli . const $ do
       S . return <$> tellDecl "vec2"
    )

instance VertexInput (B (V3 Float)) where
  type VertexFormat (B (V3 Float)) = S (V3 Float)
  toVertex = ToVertex
    (Kleisli $ \B{..} -> do
       (offset, loc) <- update $
         \(o, l) -> (o + fromIntegral bStride, l + 1)
       tell [Vk.zero {
         VkAttrs.binding = 0,
         VkAttrs.format = Vk.FORMAT_R32G32B32_SFLOAT,
         VkAttrs.location = loc,
         VkAttrs.offset = offset
       }]
       return undefined
    )
    (Kleisli . const $ do
       S . return <$> tellDecl "vec3"
    )

instance (VertexInput a, VertexInput b) => VertexInput (a, b) where
  type VertexFormat (a, b) = (VertexFormat a, VertexFormat b)
  toVertex =
    proc ~(a, b) -> do
      a' <- toVertex -< a
      b' <- toVertex -< b
      returnA -< (a', b')

class VertexOutput a where
  toGLSL :: ToGLSL a (S ())

newtype ToGLSL a b = ToGLSL
  -- Builds GLSL declarations
  (Kleisli DeclM a b)

instance Category ToGLSL where
  id = ToGLSL id
  (ToGLSL a) . (ToGLSL b) = ToGLSL (a . b)

instance Arrow ToGLSL where
  arr = ToGLSL . arr
  first (ToGLSL a) = ToGLSL (first a)

instance VertexOutput (S Float) where
  toGLSL = ToGLSL
    (Kleisli $ \a -> do
      n <- tellDecl "float"
      return . S $ do
        a' <- unS a
        tellExpr $ n <> " = " <> a'
        return ""
    )

instance VertexOutput (S (V3 Float)) where
  toGLSL = ToGLSL
    (Kleisli $ \a -> do
      n <- tellDecl "vec3"
      return . S $ do
        a' <- unS a
        tellExpr $ n <> " = " <> a'
        return ""
    )

instance (VertexOutput (S a), VertexOutput (S b)) => VertexOutput (S a, S b) where
  toGLSL = proc ~(a, b) -> do
    aOut <- toGLSL -< a
    bOut <- toGLSL -< b
    returnA -< S $ do
      _ <- unS aOut
      _ <- unS bOut
      return ""
