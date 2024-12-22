module Graphics.Shaders.Pipeline (
  createPipeline
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString
import Data.Default
import qualified Data.Vector as V
import Language.SpirV.Internal
import Language.SpirV.ShaderKind
import Language.SpirV.Shaderc
import Language.SpirV.Shaderc.CompileOptions
import Text.Printf
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.Pipeline as VkPipeline hiding (
  ComputePipelineCreateInfo(..))
import Vulkan.CStruct.Extends (SomeStruct(..))
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Device

createPipeline :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Vk.RenderPass
  -> Codensity m VkPipeline.Pipeline
createPipeline Device{..} renderPass = do

  vertexShaderModule <- createVertexShader deviceHandle
  fragmentShaderModule <- createFragmentShader deviceHandle

  debug "Creating pipeline layout."
  layout <- Codensity $ bracket
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
          VkPipeline.vertexInputState = Just . SomeStruct $ Vk.zero,
          VkPipeline.viewportState = Just . SomeStruct $ Vk.zero {
            VkPipeline.viewportCount = 1,
            VkPipeline.scissorCount = 1
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
  Codensity $ bracket
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

createVertexShader :: (MonadAsyncException m, MonadLogger m)
  => Vk.Device
  -> Codensity m Vk.ShaderModule
createVertexShader device = do
  debug "Compiling vertex shader source."
  (S compiledCode :: S 'VertexShader) <-
    liftIO $ compile code "" "main" (def :: C ())
  let createInfo = Vk.zero {
          Vk.code = compiledCode
        }
  debug "Creating vertex shader module."
  Codensity $ bracket
    (Vk.createShaderModule device createInfo Nothing)
    (\s -> do
      debug "Destroying vertex shader module."
      Vk.destroyShaderModule device s Nothing
    )
 where
  code :: ByteString
  code = "\
    \ #version 450\n\
    \ #extension GL_ARB_separate_shader_objects : enable\n\n\
    \ layout(location = 0) out vec3 fragColor;\n\n\
    \ vec2 positions[3] = vec2[](\n\
    \   vec2(0.0, -0.5),\n\
    \   vec2(-0.5, 0.5),\n\
    \   vec2(0.5, 0.5)\n\
    \ );\n\n\
    \ vec3 colors[3] = vec3[](\n\
    \   vec3(1.0, 0.0, 0.0),\n\
    \   vec3(0.0, 1.0, 0.0),\n\
    \   vec3(0.0, 0.0, 1.0)\n\
    \ );\n\n\
    \ void main() {\n\
    \   gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);\n\
    \   fragColor = colors[gl_VertexIndex];\n\
    \ }"

createFragmentShader :: (MonadAsyncException m, MonadLogger m)
  => Vk.Device
  -> Codensity m Vk.ShaderModule
createFragmentShader device = do
  debug "Compiling fragment shader source."
  (S compiledCode :: S 'FragmentShader) <-
    liftIO $ compile code "" "main" (def :: C ())
  let createInfo = Vk.zero {
          Vk.code = compiledCode
        }
  debug "Creating fragment shader module."
  Codensity $ bracket
    (Vk.createShaderModule device createInfo Nothing)
    (\s -> do
      debug "Destroying fragment shader module."
      Vk.destroyShaderModule device s Nothing
    )
 where
  code :: ByteString
  code  = "\
    \ #version 450\n\
    \ #extension GL_ARB_separate_shader_objects : enable\n\n\
    \ layout(location = 0) in vec3 fragColor;\n\n\
    \ layout(location = 0) out vec4 outColor;\n\n\
    \ void main() {\n\
    \   outColor = vec4(fragColor, 1.0);\n\
    \ }"
