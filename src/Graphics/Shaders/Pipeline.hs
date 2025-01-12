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
import Control.Monad.State.Extra
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

import Data.Linear
import Graphics.Shaders
import Graphics.Shaders.Buffer
import Graphics.Shaders.Logger.Class

newtype Pipeline v = Pipeline {
  pipelineHandle :: Vk.Pipeline
}

-- Shader expressions.
newtype S a = S a

withPipeline :: forall input vIn vOut m.
 (Bufferable input, BufferFormat input ~ vIn,
  MonadAsyncException m, MonadLogger m, VertexInput vIn, VertexInput vOut)
  => (vIn -> (V4 Float, vOut))
  -> ShadersT m (Pipeline input)
withPipeline vertexShader = do
  deviceHandle <- getDeviceHandle
  renderPass <- getRenderPass
  vertexShaderModule <- createVertexShader deviceHandle vertexShader
  fragmentShaderModule <- createFragmentShader deviceHandle

  let ToBuffer (Kleisli calcStride) _ = toBuffer
      (b, stride) = flip runState 0 . calcStride $ (undefined :: input)

  let ToVertex (Kleisli buildVertexAttrDescrs) _
        = toVertex :: ToVertex vIn (VertexFormat vIn)

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

createVertexShader :: forall m input output. (MonadAsyncException m,
    MonadLogger m, VertexInput input, VertexInput output)
  => Vk.Device
  -> (input -> (V4 Float, output))
  -> ShadersT m Vk.ShaderModule
createVertexShader device _ = do
  let ToVertex _ (Kleisli buildInDecls)
        = toVertex :: ToVertex input (VertexFormat input)
      ToVertex _ (Kleisli buildOutDecls)
        = toVertex :: ToVertex output (VertexFormat output)

  let inDecls = execWriter . flip runStateT 0 . flip runReaderT In
        . buildInDecls $ (undefined :: input)
      outDecls = execWriter . flip runStateT 0 . flip runReaderT Out
        . buildOutDecls $ (undefined :: output)

  let code = "#version 460\n\n"
        <> inDecls
        <> outDecls
        <> body

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
 where
  body =
   "\
    \ void main() {\n\
    \   gl_Position = vec4(in0, 0.0, 1.0);\n\
    \   out0 = in1;\n\
    \ }"

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
  -- Build Vulcan attribute descriptions
  (Kleisli AttrDescrM a b)
  -- Build GLSL declarations
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

data InOut = In | Out

inOut :: InOut -> ByteString
inOut io =
  case io of
    In  -> "in"
    Out -> "out"

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
    (Kleisli . const . ReaderT $ \io -> do
       n <- get
       put $ n + 1
       let n' = BS.pack . show $ n
       tell $ "layout(location=" <> n' <> ") " <> inOut io <> " float "
         <> inOut io <> n' <> ";"
       return undefined
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
    (Kleisli . const . ReaderT $ \io -> do
       n' <- BS.pack . show <$> getNext
       tell $ "layout(location=" <> n' <> ") " <> inOut io <> " vec2 "
         <> inOut io <> n' <> ";\n"
       return undefined
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
    (Kleisli . const . ReaderT $ \io -> do
       n' <- BS.pack . show <$> getNext
       tell $ "layout(location=" <> n' <> ") " <> inOut io <> " vec3 "
         <> inOut io <> n' <> ";\n"
       return undefined
    )

instance (VertexInput a, VertexInput b) => VertexInput (a, b) where
  type VertexFormat (a, b) = (VertexFormat a, VertexFormat b)
  toVertex =
    proc ~(a, b) -> do
      a' <- toVertex -< a
      b' <- toVertex -< b
      returnA -< (a', b')
