module Graphics.Shaders.Internal.Pipeline (
  Pipeline(..),
  CompiledPipeline,

  UniformBinding(..),
  BufferGetter(..),

  VertexStream,
  FragmentStream,

  compilePipeline,
  runPipeline,

  toVertexStream,
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
import Data.Word
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
import qualified Vulkan.Core10.Pipeline as VkAttrs (
  VertexInputAttributeDescription(..))
import qualified Vulkan.Core10.Pipeline as VkBinding (
  VertexInputBindingDescription(..))
import qualified Vulkan.Core10.Pipeline as VkPipeline hiding (
  ComputePipelineCreateInfo(..))
import qualified Vulkan.Core10.PipelineLayout as VkPipeline
import qualified Vulkan.Core10.Queue as VkQueue
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.FundamentalTypes as VkRect2D (Rect2D(..))
import qualified Vulkan.Core10.Shader as VkShader
import Vulkan.CStruct.Extends (SomeStruct(..))
import qualified Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Data.Linear
import Graphics.Shaders.Base
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Logger.Class

type GLPos = S V (V4 Float)

data CompiledPipeline e = CompiledPipeline {
  compiledPipeline :: VkPipeline.Pipeline,
  compiledPipelineDescriptorSets :: Vector VkDescr.DescriptorSet,
  compiledPipelineLayout :: Vk.PipelineLayout,
  compiledPipelineUniformInput :: [(Int, BufferGetter e)],
  compiledPipelineVertexInput :: BufferGetter e
}

-- Pipeline Monad
newtype Pipeline e a = Pipeline {
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
  vertexBufferGetter :: BufferGetter e,
  vertexInputDeclarations :: ByteString,
  vertexStride :: Int
}

data UniformBinding e = UniformBinding {
  uniformBindingNumber :: Int,
  uniformBufferGetter :: BufferGetter e,
  uniformDeclaration :: ByteString,
  uniformDescrSetLayoutBinding :: VkDescr.DescriptorSetLayoutBinding
}

tellVertexInput :: VertexBinding e -> Pipeline e Int
tellVertexInput i = do
  (n', _, _, _) <- Pipeline . update $
    \(n, ub, ins, ug) -> (n + 1, ub, M.insert n i ins, ug)
  return n'

data BufferGetter e = forall b. BufferGetter (e -> Buffer b)

withBufferGetter :: (forall b. Buffer b -> r) -> BufferGetter e -> e -> r
withBufferGetter f (BufferGetter getBuffer) e = f $ getBuffer e


data VertexStream a =
  VertexStream
    Int -- Input name
    a   -- An S V x, representing our vertices and encoding the shader.

instance Functor VertexStream where
  fmap f (VertexStream n a) = VertexStream n (f a)

toVertexStream :: forall e a. (BufferFormat a, VertexInput a)
  => (e -> Buffer a)
  -> Pipeline e (VertexStream (VertexFormat a))
toVertexStream getBuffer = do
  let ToBuffer (Kleisli calcStride) _ alignMode
        = toBuffer :: ToBuffer (HostFormat a) a
      ((bIn, _), stride) = flip runState 0 . runWriterT
        . flip runReaderT alignMode . calcStride $ (undefined :: HostFormat a)

  let ToVertex (Kleisli buildDescrs) (Kleisli buildInDecls) =
        toVertex :: ToVertex a (VertexFormat a)
      (vIn, vInputDecls) = runWriter . flip evalStateT 0 . flip runReaderT In
                             . buildInDecls $ bIn
      (_, vInDescrs) = flip evalState (0, 0) . runWriterT . buildDescrs
                         $ bIn

  inName <- tellVertexInput $
    VertexBinding vInDescrs (BufferGetter getBuffer) vInputDecls stride
  return $ VertexStream inName vIn


data FragmentStream a =
  FragmentStream
    a             -- Usually an S F x, our fragments.
    Rasterization -- Rasterization

instance Functor FragmentStream where
  fmap f (FragmentStream a r) = FragmentStream (f a) r

data Rasterization = Rasterization
  Int        -- Input name
  (S V ())   -- Vertex shader body
  GLPos
  ByteString -- Vertex shader output declarations
  ByteString -- Fragment input declarations

rasterize :: forall e a. FragmentInput a
  => VertexStream (GLPos, a)
  -> Pipeline e (FragmentStream (FragmentFormat a))
rasterize (VertexStream inName (glPos, vOut)) = do
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

compilePipeline :: (MonadAsyncException m, MonadLogger m)
  => Pipeline e (FragmentStream (S F (V4 Float)))
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
        VkPipeline.DYNAMIC_STATE_VIEWPORT,
        VkPipeline.DYNAMIC_STATE_SCISSOR
      ]
    },
    VkPipeline.inputAssemblyState = Just $ Vk.zero {
      VkPipeline.topology = VkPipeline.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
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
      VkPipeline.vertexBindingDescriptions = V.singleton $ Vk.zero {
        VkBinding.binding = 0,
        VkBinding.inputRate = VkEnum.VERTEX_INPUT_RATE_VERTEX,
        VkBinding.stride = fromIntegral vertexStride
      }
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
    compiledPipelineVertexInput = vertexBufferGetter
  }
 where
  createVertexShader :: (MonadAsyncException m, MonadLogger m)
    => VkDevice.Device
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

  createFragmentShader :: (MonadAsyncException m, MonadLogger m)
    => VkDevice.Device
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
            let b = withBufferGetter bufferHandle getter e
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
    compiledPipelineVertexInput

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
    -> BufferGetter e
    -> ShadersT m ()
  recordCommandBuffer commandBuffer framebuffer pipelineHandle descriptorSet
    bufferGetter = do
    let bufferHandle' = withBufferGetter bufferHandle bufferGetter e
        bufferNumVertices' = withBufferGetter bufferNumVertices bufferGetter e
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

      VkCmd.cmdBindVertexBuffers commandBuffer 0
        (V.singleton bufferHandle') (V.singleton 0)

      VkCmd.cmdDraw commandBuffer bufferNumVertices' 1 0 0



-- Represents 4 byte aligned data
class VertexInput a where
  type VertexFormat a
  toVertex :: ToVertex a (VertexFormat a)

data ToVertex a b = ToVertex
  -- Builds Vulcan attribute descriptions
  (Kleisli AttrDescrM a b)
  -- Builds GLSL vertex input declarations
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
  tell $ "layout(location = " <> loc' <> ") " <> inOut <> " " <> typ <> " "
           <> name <> ";\n"
  return name

data InOut = In | Out



instance VertexInput (B Float) where
  type VertexFormat (B Float) = S V Float
  toVertex = ToVertex
    (Kleisli $ \B{..} -> do
       (offset, loc) <- update $
         \(o, l) -> (o + fromIntegral bOffset, l + 1)
       tell [Vk.zero {
         VkAttrs.binding = 0,
         VkAttrs.format = VkEnum.FORMAT_R32_SFLOAT,
         VkAttrs.location = loc,
         VkAttrs.offset = offset
       }]
       return undefined
    )
    (Kleisli . const $ do
       S . return <$> tellDecl "float"
    )

instance VertexInput (B (V2 Float)) where
  type VertexFormat (B (V2 Float)) = S V (V2 Float)
  toVertex = ToVertex
    (Kleisli $ \B{..} -> do
       (offset, loc) <- update $
         \(o, l) -> (o + fromIntegral bOffset, l + 1)
       tell [Vk.zero {
         VkAttrs.binding = 0,
         VkAttrs.format = VkEnum.FORMAT_R32G32_SFLOAT,
         VkAttrs.location = loc,
         VkAttrs.offset = offset
       }]
       return undefined
    )
    (Kleisli . const $ do
       S . return <$> tellDecl "vec2"
    )

instance VertexInput (B (V3 Float)) where
  type VertexFormat (B (V3 Float)) = S V (V3 Float)
  toVertex = ToVertex
    (Kleisli $ \B{..} -> do
       (offset, loc) <- update $
         \(o, l) -> (o + fromIntegral bOffset, l + 1)
       tell [Vk.zero {
         VkAttrs.binding = 0,
         VkAttrs.format = VkEnum.FORMAT_R32G32B32_SFLOAT,
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


class FragmentInput a where
  type FragmentFormat a
  toFragment :: ToFragment a (FragmentFormat a)

newtype ToFragment a b = ToFragment
  -- Vertex shader output assignments (combined into a S V ()) and GLSL
  -- declarations.
  (Kleisli (StateT (S V ()) DeclM) a b)

instance Category ToFragment where
  id = ToFragment id
  (ToFragment a) . (ToFragment b) = ToFragment (a . b)

instance Arrow ToFragment where
  arr = ToFragment . arr
  first (ToFragment a) = ToFragment (first a)

instance FragmentInput (S V Float) where
  type FragmentFormat (S V Float) = S F Float
  toFragment = ToFragment
    (Kleisli $ \a -> do
      n <- lift $ tellDecl "float"
      out <- get
      put . S $ do
        _ <- unS out
        a' <- unS a
        tellStatement $ n <> " = " <> a'
        return n
      return . S $ return n
    )

instance FragmentInput (S V (V3 Float)) where
  type FragmentFormat (S V (V3 Float)) = S F (V3 Float)
  toFragment = ToFragment
    (Kleisli $ \a -> do
      n <- lift $ tellDecl "vec3"
      out <- get
      put . S $ do
        _ <- unS out
        a' <- unS a
        tellStatement $ n <> " = " <> a'
        return n
      return . S $ return n
    )

instance FragmentInput (S V (V4 Float)) where
  type FragmentFormat (S V (V4 Float)) = S F (V4 Float)
  toFragment = ToFragment
    (Kleisli $ \a -> do
      n <- lift $ tellDecl "vec4"
      out <- get
      put . S $ do
        _ <- unS out
        a' <- unS a
        tellStatement $ n <> " = " <> a'
        return n
      return . S $ return n
    )

instance (FragmentInput (S V a), FragmentInput (S V b))
    => FragmentInput (S V a, S V b) where
  type FragmentFormat (S V a, S V b) = (FragmentFormat (S V a), FragmentFormat (S V b))
  toFragment = proc ~(a, b) -> do
    a' <- toFragment -< a
    b' <- toFragment -< b
    returnA -< (a', b')
