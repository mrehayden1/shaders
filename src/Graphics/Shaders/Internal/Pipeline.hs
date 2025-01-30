module Graphics.Shaders.Internal.Pipeline (
  Pipeline(..),
  CompiledPipeline,

  UniformBinding(..),
  BufferGetter(..),

  PrimitiveStream,
  FragmentStream,

  compilePipeline,
  runPipeline,

  toPrimitiveStream,
  rasterize
) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.Function hiding ((.), id)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Language.SpirV.Internal as SpirV
import qualified Language.SpirV.ShaderKind as SpirV
import qualified Language.SpirV.Shaderc as SpirV
import qualified Language.SpirV.Shaderc.CompileOptions as SpirV
import Text.Printf
import qualified Vulkan.Core10.APIConstants as Vk
import qualified Vulkan.Core10.CommandBuffer as VkCmd hiding (
  CommandBufferInheritanceInfo(..))
import qualified Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.DescriptorSet as VkDescr
import qualified Vulkan.Core10.DescriptorSet as VkDSBuffer (
  DescriptorBufferInfo(..))
import qualified Vulkan.Core10.DescriptorSet as VkDSWrite (
  WriteDescriptorSet(..))
import qualified Vulkan.Core10.Device as VkDevice
import qualified Vulkan.Core10.Enums as VkEnum
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Pipeline as VkBinding (
  VertexInputBindingDescription(..))
import qualified Vulkan.Core10.Pipeline as VkPipeline hiding (
  ComputePipelineCreateInfo(..))
import qualified Vulkan.Core10.PipelineLayout as VkPipeline
import qualified Vulkan.Core10.Queue as VkQueue
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.FundamentalTypes as VkRect2D (Rect2D(..))
import qualified Vulkan.Core10.Shader as VkShader
import qualified Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state as VkCmd
import Vulkan.CStruct.Extends (SomeStruct(..))
import qualified Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Data.Linear
import Graphics.Shaders.Base
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.DeclM
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Internal.FragmentStream
import Graphics.Shaders.Internal.PrimitiveArray
import Graphics.Shaders.Internal.PrimitiveStream
import Graphics.Shaders.Logger.Class

data CompiledPipeline e = CompiledPipeline {
  compiledPipeline :: VkPipeline.Pipeline,
  compiledPipelineDescriptorSets :: Vector VkDescr.DescriptorSet,
  compiledPipelineLayout :: Vk.PipelineLayout,
  compiledPipelineUniformInput :: [(Int, BufferGetter e)],
  compiledPipelinePrimitiveArray :: PrimitiveArrayGetter e
}

-- Pipeline Monad
newtype Pipeline t e a = Pipeline {
  unPipeline ::
    StateT
    (PipelineS e)
    IO
    a
} deriving (Functor, Applicative, Monad, MonadIO)

type PipelineS e = (
    Int, -- Next unique name
    Int, -- Next uniform binding number
    -- Vertex binding mapped by unique name
    Map Int (VertexBinding e),
    -- Uniform binding mapped by hashed StableName of the uniform's getter.
    Map Int (UniformBinding e)
  )

data VertexBinding e = VertexBinding {
  vertexAttributeDescriptions :: [VkPipeline.VertexInputAttributeDescription],
  vertexBindingInputDescription :: [VkPipeline.VertexInputBindingDescription],
  vertexInputDeclarations :: ByteString,
  vertexPrimitiveArrayGetter :: PrimitiveArrayGetter e
}

data UniformBinding e = UniformBinding {
  uniformBindingNumber :: Int,
  uniformBufferGetter :: BufferGetter e,
  uniformDeclaration :: ByteString,
  uniformDescrSetLayoutBinding :: VkDescr.DescriptorSetLayoutBinding
}

tellVertexInput :: VertexBinding e -> Pipeline t e Int
tellVertexInput i = do
  (n', _, _, _) <- Pipeline . update $
    \(n, ub, ins, ug) -> (n + 1, ub, M.insert n i ins, ug)
  return n'

data BufferGetter e = forall a. BufferGetter (e -> Buffer a)

withBufferGetter :: BufferGetter e -> e -> (forall a. Buffer a -> r) -> r
withBufferGetter (BufferGetter getter) e f = f $ getter e

data PrimitiveArrayGetter e =
  forall a b t. PrimitiveArrayGetter (e -> PrimitiveArray t a b)

withPrimitiveArray :: PrimitiveArrayGetter e
  -> e
  -> (forall t a b. PrimitiveArray t a b -> r)
  -> r
withPrimitiveArray (PrimitiveArrayGetter getter) e f = f $ getter e

vertexBufferBindingNumber :: Num a => a
vertexBufferBindingNumber = 0

instanceBufferBindingNumber :: Num a => a
instanceBufferBindingNumber = 1

toPrimitiveStream :: forall e t a b c. (BufferFormat a, BufferFormat b,
    VertexInput c)
  => (e -> PrimitiveArray t a b)
  -> (a -> b -> c)
  -> Pipeline t e (PrimitiveStream t (VertexFormat c))
toPrimitiveStream getPrimitiveArray f = do
  let (vB, vBindDescr) =
        makeVertexBinding @a vertexBufferBindingNumber
          VkEnum.VERTEX_INPUT_RATE_VERTEX

      (iB, iBindDescr) =
        makeVertexBinding @b instanceBufferBindingNumber
          VkEnum.VERTEX_INPUT_RATE_INSTANCE

      ToVertex (Kleisli buildDescrs) (Kleisli buildInDecls) =
        toVertex :: ToVertex c (VertexFormat c)
      (aIn, inDecls) = runWriter . flip evalStateT 0 . flip runReaderT In
                         . buildInDecls $ f vB iB
      (_, inDescrs) = flip evalState 0 . runWriterT . buildDescrs . f vB $ iB

      binding = VertexBinding inDescrs [vBindDescr, iBindDescr] inDecls
        (PrimitiveArrayGetter getPrimitiveArray)

  inName <- tellVertexInput binding
  return $ PrimitiveStream inName aIn
 where
  makeVertexBinding :: forall d. (BufferFormat d)
    => Int
    -> VkEnum.VertexInputRate
    -> (d,
        VkPipeline.VertexInputBindingDescription
       )
  makeVertexBinding binding inputRate =
    let ToBuffer (Kleisli calcAlign) _ (Kleisli valueProd) vAlignMode =
          toBuffer :: ToBuffer (HostFormat d) d
        ((_, pads), stride) = flip runState 0 . runWriterT
          . flip runReaderT vAlignMode . calcAlign $ undefined
        b = flip evalState (pads, 0) . flip runReaderT binding . valueProd
              $ undefined

        bindDescrs = Vk.zero {
          VkBinding.binding = fromIntegral binding,
          VkBinding.inputRate = inputRate,
          VkBinding.stride = fromIntegral stride
        }
    in (b, bindDescrs)

rasterize :: forall a e t. FragmentInput a
  => PrimitiveStream t (GLPos, a)
  -> Pipeline t e (FragmentStream (FragmentFormat a))
rasterize (PrimitiveStream inName (glPos, vOut)) = do
  let ToFragment (Kleisli buildOutput) =
        toFragment :: ToFragment a (FragmentFormat a)

      ((_, vBody), vOutDecls) = runWriter . flip evalStateT 0
        . flip runReaderT Out . flip runStateT (S (return "")) . buildOutput
        $ vOut

      ((fIn, _), fInDecls) = runWriter . flip evalStateT 0
        . flip runReaderT In . flip runStateT (S (return "")) . buildOutput
        $ vOut

  let raster = Rasterization inName vBody glPos vOutDecls fInDecls

  return $ FragmentStream fIn raster

compilePipeline :: forall e m t. (MonadAsyncException m, MonadLogger m,
  BaseTopology t)
  => Pipeline t e (FragmentStream (S F (V4 Float)))
  -> ShadersT m (CompiledPipeline e)
compilePipeline pipeline = do
  framesInFlight <- ShadersT . asks $ V.length . graphicsFrames
  deviceHandle <- getDeviceHandle
  renderPass <- getRenderPass

  -- TODO replace all this pattern matching with a `runPipeline` function.
  (fs, (_, _, inputs, uniforms)) <- liftIO
    . flip runStateT (0, 0, mempty, mempty) . unPipeline
    $ pipeline
  let FragmentStream fOut raster = fs
      Rasterization inName vBody glPos vOutDecls fInDecls = raster
      VertexBinding{..} = inputs M.! inName
      vInDecls = vertexInputDeclarations

  let uniformLayoutBindings = M.elems
        . fmap uniformDescrSetLayoutBinding $ uniforms

  debug "Creating descriptor set layout."
  let descriptorSetLayoutInfo = Vk.zero {
    VkDescr.bindings = V.fromList uniformLayoutBindings
  }
  descriptorSetLayout <- fromCps $ bracket
    (VkDescr.createDescriptorSetLayout deviceHandle descriptorSetLayoutInfo
      Nothing)
    (\l -> do
        debug "Destroying descriptor set layout."
        VkDescr.destroyDescriptorSetLayout deviceHandle l Nothing)

  debug "Creating descriptor pool."
  let descriptorPoolInfo = Vk.zero {
        VkDescr.maxSets = fromIntegral framesInFlight,
        VkDescr.poolSizes = V.fromList [
          VkDescr.DescriptorPoolSize VkEnum.DESCRIPTOR_TYPE_UNIFORM_BUFFER
            (fromIntegral . length $ uniforms)
        ]
      }
  descriptorPool <- fromCps $ bracket
    (VkDescr.createDescriptorPool deviceHandle descriptorPoolInfo Nothing)
    (\p -> do
      debug "Destroying descriptor pool."
      VkDescr.destroyDescriptorPool deviceHandle p Nothing
    )

  debug "Allocating descriptor sets."
  let descriptorSetInfo = Vk.zero {
    VkDescr.descriptorPool = descriptorPool,
    VkDescr.setLayouts = V.replicate framesInFlight descriptorSetLayout
  }
  -- Descriptor sets are freed when the pool is destroyed.
  descriptorSets <-
    VkDescr.allocateDescriptorSets deviceHandle descriptorSetInfo

  -- Compile the shaders
  let uniformDecls = M.elems
        . fmap uniformDeclaration $ uniforms

  let vShaderSource = "#version 460\n\n"
        <> vInDecls <> "\n"
        <> vOutDecls <> "\n"
        <> BS.intercalate "\n" uniformDecls <> "\n"
        <> "void main() {\n"
        <> execExprM 2 (do
             _ <- unS vBody
             n <- unS glPos
             tellStatement $ "gl_Position = " <> n
           )
        <> "}"
  vertexShaderModule <- createVertexShader deviceHandle vShaderSource

  let fShaderSource = "#version 460\n\n"
        <> fInDecls <> "\n"
        <> "layout(location = 0) out vec4 outColor;\n\n"
        <> BS.intercalate "\n" uniformDecls <> "\n"
        <> "void main() {\n"
        <> execExprM 2 (do
             n <- unS fOut
             tellStatement $ "outColor = " <> n
           )
        <> "}"
  fragmentShaderModule <- createFragmentShader deviceHandle fShaderSource

  debug "Creating pipeline layout."
  let layoutInfo = Vk.zero {
    VkPipeline.setLayouts = V.singleton descriptorSetLayout
  }
  layout <- fromCps $ bracket
    (VkPipeline.createPipelineLayout deviceHandle layoutInfo Nothing)
    (\l -> do
        debug "Destroying pipeline layout."
        VkPipeline.destroyPipelineLayout deviceHandle l Nothing)

  let pipelineCreateInfos = V.singleton . SomeStruct $ Vk.zero {
    VkPipeline.colorBlendState = Just . SomeStruct $ Vk.zero {
      VkPipeline.attachments = V.singleton $ Vk.zero {
        VkPipeline.colorWriteMask = VkEnum.COLOR_COMPONENT_R_BIT
          .|. VkEnum.COLOR_COMPONENT_G_BIT
          .|. VkEnum.COLOR_COMPONENT_B_BIT
          .|. VkEnum.COLOR_COMPONENT_A_BIT
      },
      VkPipeline.attachmentCount = 1
    },
    VkPipeline.dynamicState = Just $ Vk.zero {
      VkPipeline.dynamicStates = V.fromList [
        VkPipeline.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY,
        VkPipeline.DYNAMIC_STATE_SCISSOR,
        VkPipeline.DYNAMIC_STATE_VIEWPORT
      ]
    },
    VkPipeline.inputAssemblyState = Just $ Vk.zero {
      VkPipeline.topology = baseTopology (undefined :: t)
    },
    VkPipeline.layout = layout,
    VkPipeline.multisampleState = Just . SomeStruct $ Vk.zero {
      VkPipeline.rasterizationSamples = VkEnum.SAMPLE_COUNT_1_BIT
    },
    VkPipeline.rasterizationState = Just . SomeStruct $ Vk.zero {
      VkPipeline.cullMode = VkPipeline.CULL_MODE_BACK_BIT,
      VkPipeline.lineWidth = 1,
      VkPipeline.polygonMode = VkPipeline.POLYGON_MODE_FILL
    },
    VkPipeline.renderPass = renderPass,
    VkPipeline.vertexInputState = Just . SomeStruct $ Vk.zero {
      VkPipeline.vertexAttributeDescriptions = V.fromList
        vertexAttributeDescriptions,
      VkPipeline.vertexBindingDescriptions =
        V.fromList vertexBindingInputDescription
    },
    VkPipeline.viewportState = Just . SomeStruct $ Vk.zero {
      VkPipeline.scissorCount = 1,
      VkPipeline.viewportCount = 1
    },
    VkPipeline.stageCount = 2,
    VkPipeline.stages = V.fromList . fmap SomeStruct $ [
      Vk.zero {
        VkPipeline.stage = VkEnum.SHADER_STAGE_VERTEX_BIT,
        VkPipeline.module' = vertexShaderModule,
        VkPipeline.name = "main"
      },
      Vk.zero {
        VkPipeline.stage = VkEnum.SHADER_STAGE_FRAGMENT_BIT,
        VkPipeline.module' = fragmentShaderModule,
        VkPipeline.name = "main"
      }
    ]
  }

  debug "Creating pipeline."
  vkPipeline <- fromCps $ bracket
    (do (result, ps) <-
          VkPipeline.createGraphicsPipelines deviceHandle Vk.zero
            pipelineCreateInfos Nothing
        when (result /= VkEnum.SUCCESS) $
          warn . printf "Non success result: %s" . show $ result
        return . V.head $ ps
    )
    (\p -> do
      debug "Destroying pipeline."
      VkPipeline.destroyPipeline deviceHandle p Nothing
    )

  return $ CompiledPipeline {
    compiledPipeline = vkPipeline,
    compiledPipelineDescriptorSets = descriptorSets,
    compiledPipelineLayout = layout,
    compiledPipelineUniformInput = M.elems . flip fmap uniforms $
      (,) <$> uniformBindingNumber <*> uniformBufferGetter,
    compiledPipelinePrimitiveArray = vertexPrimitiveArrayGetter
  }
 where
  createVertexShader :: VkDevice.Device
    -> ByteString
    -> ShadersT m VkShader.ShaderModule
  createVertexShader device code = do
    debug "Compiling vertex shader source."
    logTrace . BS.unpack $ "Dumping vertex shader source to log: \n" <> code
    (SpirV.S compiledCode :: SpirV.S 'SpirV.VertexShader) <-
      liftIO $ SpirV.compile code "<filename>" "main" (def :: SpirV.C ())
    let createInfo = Vk.zero {
      VkShader.code = compiledCode
    }
    debug "Creating vertex shader module."
    fromCps $ bracket
      (VkShader.createShaderModule device createInfo Nothing)
      (\s -> do
        debug "Destroying vertex shader module."
        VkShader.destroyShaderModule device s Nothing
      )

  createFragmentShader :: VkDevice.Device
    -> ByteString
    -> ShadersT m VkShader.ShaderModule
  createFragmentShader device code = do
    debug "Compiling fragment shader source."
    logTrace . BS.unpack $ "Dumping fragment shader source to log: \n" <> code
    (SpirV.S compiledCode :: SpirV.S 'SpirV.FragmentShader) <-
      liftIO $ SpirV.compile code "<filename>" "main" (def :: SpirV.C ())
    let createInfo = Vk.zero {
      VkShader.code = compiledCode
    }
    debug "Creating fragment shader module."
    fromCps $ bracket
      (VkShader.createShaderModule device createInfo Nothing)
      (\s -> do
        debug "Destroying fragment shader module."
        VkShader.destroyShaderModule device s Nothing
      )

runPipeline :: forall m e. (MonadIO m, MonadLogger m)
  => e
  -> CompiledPipeline e
  -> ShadersT m ()
runPipeline e CompiledPipeline{..} = do
  deviceHandle <- getDeviceHandle
  frameNumber <- ShadersT $ gets drawStateFrameIndex

  -- Update the current frame's descriptor set to point to the correct buffers.
  let descriptorSet = compiledPipelineDescriptorSets V.! frameNumber
      descriptorWrites = V.fromList
        . flip fmap compiledPipelineUniformInput $ \(bn, getter) ->
            let b = withBufferGetter getter e bufferHandle
            in SomeStruct $ Vk.zero {
              VkDSWrite.dstSet = descriptorSet,
              VkDSWrite.dstBinding = fromIntegral bn,
              VkDSWrite.descriptorCount = 1,
              VkDSWrite.descriptorType = VkEnum.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
              VkDSWrite.bufferInfo = V.singleton $ Vk.zero {
                VkDSBuffer.buffer = b,
                VkDSBuffer.range = Vk.WHOLE_SIZE
              }
            }
  VkDescr.updateDescriptorSets deviceHandle descriptorWrites V.empty

  -- Get the framebuffer for the next image in the swapchain by getting the
  -- swapchain image index.
  -- Ignore TIMEOUT and NOT_READY since we're not using a fence or semaphore
  -- and ignore SUBOPTIMAL_KHR.
  logTrace "Acquiring next image from swapchain"
  (_, framebuffer) <- getCurrentSwapChainImage

  -- Record and submit the command buffer.
  logTrace "Submitting command buffer"
  Frame{..} <- getCurrentFrame
  let SyncObjects{..} = frameSyncObjects
      commandBuffer = frameCommandBuffer
  recordCommandBuffer commandBuffer framebuffer compiledPipeline descriptorSet
    compiledPipelinePrimitiveArray

  let submitInfos = fmap SomeStruct . V.singleton $ Vk.zero {
    VkQueue.commandBuffers =
      fmap VkCmd.commandBufferHandle . V.fromList $ [ commandBuffer ],
    VkQueue.signalSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  queueHandle <- getQueueHandle
  VkQueue.queueSubmit queueHandle submitInfos syncInFlightFence

  return ()
 where
  recordCommandBuffer :: VkCmd.CommandBuffer
    -> Vk.Framebuffer
    -> VkPipeline.Pipeline
    -> Vk.DescriptorSet
    -> PrimitiveArrayGetter e
    -> ShadersT m ()
  recordCommandBuffer commandBuffer framebuffer pipelineHandle descriptorSet
    primitiveArrayGetter = do
    -- Use Codensity to bracket command buffer recording and render pass.
    flip runCodensity return $ do
      extent <- lift getExtent
      renderPass <- lift getRenderPass
      Codensity $ VkCmd.useCommandBuffer commandBuffer Vk.zero . (&) ()
      let renderPassBeginInfo = Vk.zero {
        VkCmd.clearValues = V.fromList [
          VkCmd.Color (VkCmd.Float32 0 0 0 1)
        ],
        VkCmd.framebuffer = framebuffer,
        VkCmd.renderArea = Vk.zero {
          VkRect2D.extent = extent
        },
        VkCmd.renderPass = renderPass
      }
      Codensity $
        VkCmd.cmdUseRenderPass commandBuffer renderPassBeginInfo
          VkCmd.SUBPASS_CONTENTS_INLINE . (&) ()
      lift $ VkCmd.cmdBindPipeline commandBuffer
               VkEnum.PIPELINE_BIND_POINT_GRAPHICS
               pipelineHandle

      let viewport = Vk.zero {
        VkPipeline.height = fromIntegral . VkExtent2D.height $ extent,
        VkPipeline.width = fromIntegral . VkExtent2D.width $ extent,
        VkPipeline.maxDepth = 1
      }
      VkCmd.cmdSetViewport commandBuffer 0 . V.singleton $ viewport

      let scissor = Vk.zero {
        VkRect2D.extent = extent
      }
      VkCmd.cmdSetScissor commandBuffer 0 . V.singleton $ scissor

      VkCmd.cmdBindDescriptorSets commandBuffer
        VkEnum.PIPELINE_BIND_POINT_GRAPHICS
        compiledPipelineLayout
        0
        (V.singleton descriptorSet)
        V.empty

      -- Begin draw calls
      withPrimitiveArray primitiveArrayGetter e $ \pa ->
        forM_ (unPrimitiveArray pa) $
          \PrimitiveArrayDrawCall{..} -> do

        let vertexBuffer = primitiveArrayVertices
            vertexStart = primitiveArrayStart
            indexed = primitiveArrayIndexed
            instances = primitiveArrayInstances
            topology = primitiveTopology primitiveArrayTopology

        let (instanceBuffer, numInstances, instanceStart) = case instances of
              Nothing              -> (Vk.zero, 1, 0)
              Just (b, len, start) -> (b, fromIntegral len, fromIntegral start)

        VkCmd.cmdBindVertexBuffers commandBuffer 0
          (V.fromList [ vertexBuffer, instanceBuffer ])
          (V.fromList [ 0, 0 ])

        VkCmd.cmdSetPrimitiveTopology commandBuffer topology

        case indexed of
          Indexed IndexArray{..} -> do
            VkCmd.cmdBindIndexBuffer commandBuffer indexArrayBufferHandle
              0 indexArrayType
            VkCmd.cmdDrawIndexed commandBuffer (fromIntegral indexArrayLength)
              numInstances (fromIntegral indexArrayStart)
              (fromIntegral vertexStart) instanceStart
          Unindexed numVertices ->
            VkCmd.cmdDraw commandBuffer (fromIntegral numVertices) numInstances
              (fromIntegral vertexStart) instanceStart
