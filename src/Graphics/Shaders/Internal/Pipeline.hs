module Graphics.Shaders.Internal.Pipeline (
  PipelineBuilder(..),
  CompiledPipeline,

  UniformBinding(..),
  SamplerBinding(..),
  BufferGetter(..),

  PrimitiveStream,
  FragmentStream,

  compilePipeline,
  runPipeline,

  toPrimitiveStream,
  rasterize
) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char
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
import Linear
import Text.Printf
import qualified Vulkan.Core10.APIConstants as Vk
import qualified Vulkan.Core10.CommandBuffer as VkCmd hiding (
  CommandBufferInheritanceInfo(..))
import qualified Vulkan.Core10.CommandBufferBuilding as VkCmd
import qualified Vulkan.Core10.DescriptorSet as VkDescr
import qualified Vulkan.Core10.DescriptorSet as VkDSBuffer (
  DescriptorBufferInfo(..))
import qualified Vulkan.Core10.DescriptorSet as VkDSImage (
  DescriptorImageInfo(..))
import qualified Vulkan.Core10.DescriptorSet as VkDSWrite (
  WriteDescriptorSet(..))
import qualified Vulkan.Core10.Device as VkDevice
import qualified Vulkan.Core10.Enums as Vk
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
import Graphics.Shaders.Class
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.DeclM
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Internal.FragmentStream
import Graphics.Shaders.Internal.PrimitiveArray
import Graphics.Shaders.Internal.PrimitiveStream
import Graphics.Shaders.Internal.Texture
import Graphics.Shaders.Logger.Class

data CompiledPipeline e = CompiledPipeline {
  compiledPipeline :: VkPipeline.Pipeline,
  compiledPipelineDescriptorSets :: Vector VkDescr.DescriptorSet,
  compiledPipelineLayout :: Vk.PipelineLayout,
  compiledPipelinePrimitiveArray :: PrimitiveArrayGetter e,
  compiledPipelineSamplerInput :: [(Int, e -> Texture)],
  compiledPipelineUniformInput :: [(Int, BufferGetter e)]
}

-- |A monad for defining graphics pipelines.
newtype PipelineBuilder t e a = PipelineBuilder {
  unPipeline ::
    StateT
    (PipelineBuilderState e)
    IO
    a
} deriving (Functor, Applicative, Monad, MonadIO)

type PipelineBuilderState e = (
    Int, -- Next unique name
    Int, -- Next uniform binding number
    -- Vertex binding mapped by unique name
    Map Int (VertexBinding e),
    -- Uniform binding mapped by hashed StableName of the uniform's getter.
    Map Int (UniformBinding e),
    -- Sampler binding mapped by hashed StableName of the samplers's getter.
    Map Int (SamplerBinding e)
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

data SamplerBinding e = SamplerBinding {
  samplerBindingNumber :: Int,
  samplerDeclaration :: ByteString,
  samplerDescrSetLayoutBinding :: VkDescr.DescriptorSetLayoutBinding,
  samplerTextureGetter :: e -> Texture
}

tellVertexInput :: VertexBinding e -> PipelineBuilder t e Int
tellVertexInput i = do
  (n', _, _, _, _) <- PipelineBuilder . update $
    \(n, ub, ins, ubs, sbs) -> (n + 1, ub, M.insert n i ins, ubs, sbs)
  return n'

data BufferGetter e = forall a r. BufferGetter (e -> Buffer r a)

withBufferGetter :: BufferGetter e -> e -> (forall a r. Buffer r a -> r') -> r'
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
  -> PipelineBuilder t e (PrimitiveStream t (VertexFormat c))
toPrimitiveStream getPrimitiveArray f = do
  let (vB, vBindDescr) =
        makeVertexBinding @a vertexBufferBindingNumber
          Vk.VERTEX_INPUT_RATE_VERTEX

      (iB, iBindDescr) =
        makeVertexBinding @b instanceBufferBindingNumber
          Vk.VERTEX_INPUT_RATE_INSTANCE

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
    -> Vk.VertexInputRate
    -> (d,
        VkPipeline.VertexInputBindingDescription
       )
  makeVertexBinding binding inputRate =
    let ToBuffer (Kleisli calcAlign) _ (Kleisli valueProd) vAlignMode _ =
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
  -> PipelineBuilder t e (FragmentStream (FragmentFormat a))
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
    MonadResource m, HasVulkan m, HasVulkanDevice m, HasSwapchain m,
    BaseTopology t)
  => PipelineBuilder t e (FragmentStream (V4 (S F Float)))
  -> m (CompiledPipeline e)
compilePipeline pipeline = do
  allocator <- getVulkanAllocator
  device <- getDevice
  numFrames <- getNumFrames
  renderPass <- getRenderPass

  -- TODO replace all this pattern matching with a `runPipeline`
  (fs, (_, _, inputs, uniforms, samplers)) <- liftIO
    . flip runStateT (0, 0, mempty, mempty, mempty) . unPipeline
    $ pipeline
  let FragmentStream fOut raster = fs
      Rasterization inName vBody glPos vOutDecls fInDecls = raster
      VertexBinding{..} = inputs M.! inName
      vInDecls = vertexInputDeclarations

  let uniformDescrSetLayoutBindings = fmap uniformDescrSetLayoutBinding uniforms
      samplerDescrSetLayoutBindings = fmap samplerDescrSetLayoutBinding samplers
      descrSetLayoutBindings = M.elems $
        uniformDescrSetLayoutBindings <> samplerDescrSetLayoutBindings

  debug "Creating descriptor set layout."
  let descriptorSetLayoutInfo = Vk.zero {
    VkDescr.bindings = V.fromList descrSetLayoutBindings
  }
  (_, descriptorSetLayout) <- allocate
    (VkDescr.createDescriptorSetLayout device descriptorSetLayoutInfo
      allocator)
    (\l -> VkDescr.destroyDescriptorSetLayout device l allocator)

  debug "Creating descriptor pool."
  let descriptorPoolInfo = Vk.zero {
        VkDescr.maxSets = fromIntegral numFrames,
        VkDescr.poolSizes = V.fromList [
          VkDescr.DescriptorPoolSize
            Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
            (fromIntegral . length $ uniforms),
          VkDescr.DescriptorPoolSize
            Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            (fromIntegral . length $ samplers)
        ]
      }
  (_, descriptorPool) <- allocate
    (VkDescr.createDescriptorPool device descriptorPoolInfo allocator)
    (\p -> VkDescr.destroyDescriptorPool device p allocator)

  debug "Allocating descriptor sets."
  let descriptorSetInfo = Vk.zero {
    VkDescr.descriptorPool = descriptorPool,
    VkDescr.setLayouts = V.replicate numFrames descriptorSetLayout
  }
  -- Descriptor sets are freed when the pool is destroyed.
  descriptorSets <-
    VkDescr.allocateDescriptorSets device descriptorSetInfo

  -- Compile the shaders
  let uniformDecls = fmap uniformDeclaration uniforms
      samplerDecls = fmap samplerDeclaration samplers
      uniformDecls' = M.elems $ uniformDecls <> samplerDecls

  vShaderBody <- liftIO . execExprM 2 $ do
    _ <- unS vBody
    let V4 x y z w = glPos
    x' <- unS x
    y' <- unS y
    z' <- unS z
    w' <- unS w
    tellStatement $ "gl_Position = vec4("
      <> BS.intercalate ", " [x', y', z', w'] <> ")"

  let vShaderSource = "#version 460\n\n"
        <> vInDecls <> "\n"
        <> vOutDecls <> "\n"
        <> BS.intercalate "\n" uniformDecls' <> "\n"
        <> "void main() {\n"
        <> vShaderBody
        <> "}"
  vertexShaderModule <- createShader device Vertex vShaderSource

  fShaderBody <- liftIO . execExprM 2 $ do
    let V4 r g b a = fOut
    r' <- unS r
    g' <- unS g
    b' <- unS b
    a' <- unS a
    tellStatement $ "outColor = vec4("
      <> BS.intercalate ", " [r', g', b', a'] <> ")"

  let fShaderSource = "#version 460\n\n"
        <> fInDecls <> "\n"
        <> "layout(location = 0) out vec4 outColor;\n\n"
        <> BS.intercalate "\n" uniformDecls' <> "\n"
        <> "void main() {\n"
        <> fShaderBody
        <> "}"
  fragmentShaderModule <- createShader device Fragment fShaderSource

  debug "Creating pipeline layout."
  let layoutInfo = Vk.zero {
    VkPipeline.setLayouts = V.singleton descriptorSetLayout
  }
  (_, layout) <- allocate
    (VkPipeline.createPipelineLayout device layoutInfo allocator)
    (\l -> VkPipeline.destroyPipelineLayout device l allocator)

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
      VkPipeline.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
    },
    VkPipeline.rasterizationState = Just . SomeStruct $ Vk.zero {
      VkPipeline.cullMode = VkPipeline.CULL_MODE_BACK_BIT,
      VkPipeline.frontFace = VkPipeline.FRONT_FACE_COUNTER_CLOCKWISE,
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
  (_, (result, vkPipeline)) <- allocate
    (do (result, ps) <-
          VkPipeline.createGraphicsPipelines device Vk.zero
            pipelineCreateInfos allocator
        return (result, V.head ps)
    )
    (\(_, p) -> VkPipeline.destroyPipeline device p allocator)

  when (result /= Vk.SUCCESS) $
    warn . printf "Non success result: %s" . show $ result

  return $ CompiledPipeline {
    compiledPipeline = vkPipeline,
    compiledPipelineDescriptorSets = descriptorSets,
    compiledPipelineLayout = layout,
    compiledPipelinePrimitiveArray = vertexPrimitiveArrayGetter,
    compiledPipelineSamplerInput = M.elems . flip fmap samplers $
      liftA2 (,) samplerBindingNumber samplerTextureGetter,
    compiledPipelineUniformInput = M.elems . flip fmap uniforms $
      liftA2 (,) uniformBindingNumber uniformBufferGetter
  }


data ShaderStage = Vertex | Fragment
 deriving Show

createShader :: (MonadAsyncException m, MonadLogger m, MonadResource m,
    HasVulkan m)
  => VkDevice.Device
  -> ShaderStage
  -> ByteString
  -> m VkShader.ShaderModule
createShader device stage code = do
  allocator <- getVulkanAllocator
  let stageName = fmap toLower . show $ stage
  debug . printf "Creating %s shader module." $ stageName
  debug "Compiling shader source."
  logTrace . BS.unpack $ "Dumping shader source to log: \n" <> code
  compiledCode <-
    liftIO $ case stage of
      Fragment -> compile @'SpirV.FragmentShader
      Vertex -> compile @'SpirV.VertexShader
  let createInfo = Vk.zero {
    VkShader.code = compiledCode
  }
  snd <$> allocate
    (VkShader.createShaderModule device createInfo allocator)
    (\s -> VkShader.destroyShaderModule device s allocator)
 where
  compile :: forall (s :: SpirV.ShaderKind). SpirV.IsShaderKind s
    => IO ByteString
  compile = do
    let shaderFileExt = case stage of
          Fragment -> ".frag"
          Vertex   -> ".vert"

    SpirV.S compiledCode :: SpirV.S s <-
      SpirV.compile code ("<no name>" <> shaderFileExt) "main"
        (def :: SpirV.C ())

    return compiledCode

runPipeline :: forall m e. (MonadIO m, MonadLogger m, HasVulkanDevice m,
    HasSwapchain m)
  => e
  -> CompiledPipeline e
  -> m ()
runPipeline e CompiledPipeline{..} = do
  device <- getDevice
  (frameNumber, Frame{..}, framebuffer) <- getCurrentFrame

  -- Update the current frame's descriptor set to point to the correct uniform
  -- buffers and textures/samplers.
  let descriptorSet = compiledPipelineDescriptorSets V.! frameNumber
      uniformDescriptorWrites = flip fmap compiledPipelineUniformInput $
        \(bind, getter) ->
          let b = withBufferGetter getter e bufferHandle
          in SomeStruct $ Vk.zero {
            VkDSWrite.dstSet = descriptorSet,
            VkDSWrite.dstBinding = fromIntegral bind,
            VkDSWrite.descriptorCount = 1,
            VkDSWrite.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            VkDSWrite.bufferInfo = V.singleton $ Vk.zero {
              VkDSBuffer.buffer = b,
              VkDSBuffer.range = Vk.WHOLE_SIZE
            }
          }
      samplerDescriptorWrites = flip fmap compiledPipelineSamplerInput $
        \(bind, getter) ->
          let Texture{..} = getter e
          in SomeStruct $ Vk.zero {
            VkDSWrite.dstSet = descriptorSet,
            VkDSWrite.dstBinding = fromIntegral bind,
            VkDSWrite.descriptorCount = 1,
            VkDSWrite.descriptorType =
              Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            VkDSWrite.imageInfo = V.singleton $ Vk.zero {
              VkDSImage.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
              VkDSImage.imageView = textureImageView,
              VkDSImage.sampler = textureSampler
            }
          }
      descriptorWrites = V.fromList $
        uniformDescriptorWrites <> samplerDescriptorWrites
  VkDescr.updateDescriptorSets device descriptorWrites V.empty

  -- Record and submit the command buffers.
  logTrace "Submitting command buffers"

  let SyncObjects{..} = frameSyncObjects
      commandBuffer = frameCommandBuffer

  recordCommands commandBuffer framebuffer compiledPipeline descriptorSet
    compiledPipelinePrimitiveArray

  let submitInfos = fmap SomeStruct . V.singleton $ Vk.zero {
    VkQueue.commandBuffers =
      fmap VkCmd.commandBufferHandle . V.fromList $ [ commandBuffer ],
    VkQueue.signalSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  queue <- getDeviceQueue
  VkQueue.queueSubmit queue submitInfos syncInFlightFence

  return ()
 where
  recordCommands :: VkCmd.CommandBuffer
    -> Vk.Framebuffer
    -> VkPipeline.Pipeline
    -> Vk.DescriptorSet
    -> PrimitiveArrayGetter e
    -> m ()
  recordCommands commandBuffer framebuffer pipelineHandle descriptorSet
    primitiveArrayGetter = flip runCodensity return $ do
    -- Use Codensity to bracket command buffer recording and render pass.
    extent <- lift getSwapChainExtent
    renderPass <- lift getRenderPass
    Codensity $ VkCmd.useCommandBuffer commandBuffer Vk.zero . (&) ()

    -- Transfer data in staging buffers to their writable buffer.
    forM_ compiledPipelineUniformInput $
      \(_, getter) ->
        withBufferGetter getter e (
          (\case
             BufferReadOnly{}   -> return ()
             BufferWritable{..} -> do
               VkCmd.cmdCopyBuffer
                 commandBuffer
                 wBufferStagingBufferHandle
                 wBufferHandle
                 (V.singleton $ VkCmd.BufferCopy
                    Vk.zero
                    Vk.zero
                    (fromIntegral $ wBufferStride * wBufferNumElems)
                 )
          ) :: Buffer r a -> Codensity m ()
        )

    -- TODO Barrier
    --VkCmd.cmdPipelineBarrier

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
      Vk.PIPELINE_BIND_POINT_GRAPHICS pipelineHandle

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
      Vk.PIPELINE_BIND_POINT_GRAPHICS
      compiledPipelineLayout
      0
      (V.singleton descriptorSet)
      V.empty

    -- Begin draw calls
    withPrimitiveArray primitiveArrayGetter e $ \pa ->
      forM_ (unPrimitiveArray pa) $ \PrimitiveArrayDrawCall{..} -> do

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
