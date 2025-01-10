module Graphics.Shaders.Pipeline (
  Pipeline(..),

  withPipeline
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Exception
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.Vector (Vector)
import Data.Word
import qualified Data.Vector as V
import Foreign.Storable
import qualified Language.SpirV.Internal as SpirV
import qualified Language.SpirV.ShaderKind as SpirV
import qualified Language.SpirV.Shaderc as SpirV
import qualified Language.SpirV.Shaderc.CompileOptions as SpirV
import Linear hiding (trace)
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

import Graphics.Shaders
import Graphics.Shaders.Logger.Class

newtype Pipeline v = Pipeline {
  pipelineHandle :: Vk.Pipeline
}

withPipeline :: forall vIn vOut m. (MonadAsyncException m, MonadLogger m,
    VertexFormat vIn, VertexFormat vOut)
  => (vIn -> (V4 Float, vOut))
  -> ShadersT m (Pipeline vIn)
withPipeline vertexShader = do
  deviceHandle <- getDeviceHandle
  renderPass <- getRenderPass
  vertexShaderModule <- createVertexShader deviceHandle vertexShader
  fragmentShaderModule <- createFragmentShader deviceHandle

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
              vertexAttributeDescription (undefined :: vIn),
            VkPipeline.vertexBindingDescriptions = V.singleton
              . vertexBindingDescription $ (undefined :: vIn)
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
    MonadLogger m, VertexFormat input, VertexFormat output)
  => Vk.Device
  -> (input -> (V4 Float, output))
  -> ShadersT m Vk.ShaderModule
createVertexShader device _ = do
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
  inDecls = attributeDeclarations In (undefined :: input)

  outDecls = attributeDeclarations Out (undefined :: output)

  body =
   "\
    \ void main() {\n\
    \   gl_Position = vec4(in0, 0.0, 1.0);\n\
    \   out0 = in1;\n\
    \ }"

  code :: ByteString
  code = "#version 460\n\n"
    <> inDecls
    <> outDecls
    <> body

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
class VertexFormat a where
  vertexAttributes :: a -> [VertexAttribute]
  vertexStride :: a -> Word32 -- in bytes

data VertexAttribute = VertexAttribute {
  attributeFormat :: Vk.Format,
  attributeOffset :: Word32, -- in bytes
  attributeType :: GlslType
}

data GlslType = Float | Vec2 | Vec3 | Vec4

data InOut = In | Out

inOut :: InOut -> ByteString
inOut io =
  case io of
    In  -> "in"
    Out -> "out"

attributeDeclarations :: forall a. VertexFormat a => InOut -> a -> ByteString
attributeDeclarations io _ =
  let attrs = vertexAttributes (undefined :: a)
  in (<> "\n") . mconcat . flip fmap (zip attrs [0..])
       $ \(VertexAttribute{..}, i) ->
            let n = BS.pack . show $ (i :: Int)
            in "layout(location = " <> n <> ") " <> inOut io <> " "
                 <> glslType attributeType <> " " <> inOut io <> n <> ";\n"

glslType :: GlslType -> ByteString
glslType t =
  case t of
    Float -> "float"
    Vec2  -> "vec2"
    Vec3  -> "vec3"
    Vec4  -> "vec4"

vertexBindingDescription :: forall a. VertexFormat a
  => a
  -> VkBinding.VertexInputBindingDescription
vertexBindingDescription _ = Vk.zero {
    VkBinding.binding = 0,
    VkBinding.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX,
    VkBinding.stride = vertexStride (undefined :: a)
  }

vertexAttributeDescription :: forall a. VertexFormat a
  => a
  -> Vector VkAttrs.VertexInputAttributeDescription
vertexAttributeDescription _ =
  let attrs = vertexAttributes (undefined :: a)
  in V.fromList . flip fmap (zip attrs [0..]) $ \(VertexAttribute{..}, i) ->
       Vk.zero {
         VkAttrs.binding = 0,
         VkAttrs.format = attributeFormat,
         VkAttrs.location = i,
         VkAttrs.offset = attributeOffset
       }

instance VertexFormat Float where
  vertexAttributes _ = [
      VertexAttribute {
        attributeFormat = Vk.FORMAT_R32_SFLOAT,
        attributeOffset = 0,
        attributeType = Float
      }
    ]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: Float)

instance VertexFormat (V2 Float) where
  vertexAttributes _ = [
      VertexAttribute {
        attributeFormat = Vk.FORMAT_R32G32_SFLOAT,
        attributeOffset = 0,
        attributeType = Vec2
      }
    ]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V2 Float)

instance VertexFormat (V3 Float) where
  vertexAttributes _ = [
      VertexAttribute {
        attributeFormat = Vk.FORMAT_R32G32B32_SFLOAT,
        attributeOffset = 0,
        attributeType = Vec3
      }
    ]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V3 Float)

instance VertexFormat (V4 Float) where
  vertexAttributes _ = [
      VertexAttribute {
        attributeFormat = Vk.FORMAT_R32G32B32A32_SFLOAT,
        attributeOffset = 0,
        attributeType = Vec4
      }
    ]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V4 Float)

instance (VertexFormat a, VertexFormat b) => VertexFormat (a, b) where
  vertexAttributes _ =
    let attrs  = vertexAttributes (undefined :: a)
        stride = vertexStride (undefined :: a)
        attrs' = vertexAttributes (undefined :: b)
    in attrs <>
         fmap (\attr ->
                 attr { attributeOffset = attributeOffset attr + stride })
              attrs'

  vertexStride _ = vertexStride (undefined :: a)
                    + vertexStride (undefined :: b)

instance (VertexFormat a, VertexFormat b, VertexFormat c)
    => VertexFormat (a, b, c) where
  vertexAttributes _ =
    let attrs  = vertexAttributes (undefined :: (a, b))
        stride = vertexStride (undefined :: (a, b))
        attrs' = vertexAttributes (undefined :: c)
    in attrs <>
         fmap (\attr ->
                 attr { attributeOffset = attributeOffset attr + stride })
              attrs'

  vertexStride _ = vertexStride (undefined :: (a, b))
                     + vertexStride (undefined :: c)
