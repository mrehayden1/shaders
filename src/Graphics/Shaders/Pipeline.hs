module Graphics.Shaders.Pipeline (
  Pipeline(..),

  withPipeline
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Exception
import Data.Bits
import Data.ByteString (ByteString)
import Data.Default
import Data.Vector (Vector)
import Data.Word
import qualified Data.Vector as V
import Foreign.Storable
import Language.SpirV.Internal
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc
import Language.SpirV.Shaderc.CompileOptions
import Linear
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
import Graphics.Shaders.Buffer
import Graphics.Shaders.Logger.Class

newtype Pipeline v = Pipeline {
  pipelineHandle :: Vk.Pipeline
}

withPipeline :: forall inpt m. (MonadAsyncException m, MonadLogger m,
    VertexFormat inpt)
  => ShadersT m (Pipeline inpt)
withPipeline = do
  deviceHandle <- getDeviceHandle
  renderPass <- getRenderPass
  vertexShaderModule <- createVertexShader deviceHandle
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
              vertexAttributeDescription (undefined :: inpt),
            VkPipeline.vertexBindingDescriptions = V.singleton
              . vertexBindingDescription $ (undefined :: inpt)
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

createVertexShader :: (MonadAsyncException m, MonadLogger m)
  => Vk.Device
  -> ShadersT m Vk.ShaderModule
createVertexShader device = do
  debug "Compiling vertex shader source."
  (S compiledCode :: S 'VertexShader) <-
    liftIO $ compile code "" "main" (def :: C ())
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
  code :: ByteString
  code = "\
    \ #version 460\n\n\
    \ layout(location = 0) in vec2 inPosition;\n\
    \ layout(location = 1) in vec3 inColor;\n\n\
    \ layout(location = 0) out vec3 fragColor;\n\n\
    \ void main() {\n\
    \   gl_Position = vec4(inPosition, 0.0, 1.0);\n\
    \   fragColor = inColor;\n\
    \ }"

createFragmentShader :: (MonadAsyncException m, MonadLogger m)
  => Vk.Device
  -> ShadersT m Vk.ShaderModule
createFragmentShader device = do
  debug "Compiling fragment shader source."
  (S compiledCode :: S 'FragmentShader) <-
    liftIO $ compile code "" "main" (def :: C ())
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

class Bufferable a => VertexFormat a where
  vertexAttributes :: a -> [VertexAttribute]
  vertexStride :: a -> Word32 -- in bytes

data VertexAttribute = VertexAttribute {
  attributeFormat :: Vk.Format,
  attributeOffset :: Word32 -- in bytes
}

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
        attributeOffset = 0
      }
    ]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: Float)

instance VertexFormat (V2 Float) where
  vertexAttributes _ = [
      VertexAttribute {
        attributeFormat = Vk.FORMAT_R32G32_SFLOAT,
        attributeOffset = 0
      }
    ]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V2 Float)

instance VertexFormat (V3 Float) where
  vertexAttributes _ = [
      VertexAttribute {
        attributeFormat = Vk.FORMAT_R32G32B32_SFLOAT,
        attributeOffset = 0
      }
    ]
  vertexStride _ = fromIntegral $ sizeOf (undefined :: V3 Float)

instance VertexFormat (V4 Float) where
  vertexAttributes _ = [
      VertexAttribute {
        attributeFormat = Vk.FORMAT_R32G32B32A32_SFLOAT,
        attributeOffset = 0
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
