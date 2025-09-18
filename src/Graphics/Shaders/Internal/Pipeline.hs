module Graphics.Shaders.Internal.Pipeline (
  PipelineBuilder(..),
  CompiledPipeline(..),

  UniformBinding(..),
  SamplerBinding(..),
  BufferGetter(..),
  withBufferGetter,

  PrimitiveArrayGetter(..),
  withPrimitiveArrayGetter,

  PrimitiveStream(..),
  FragmentStream(..),

  compilePipeline,

  toPrimitiveStream,
  rasterize
) where

import Prelude
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Bits
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.ByteString.Char8 as BS
import Data.Default
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
import qualified Vulkan.Core10.DescriptorSet as VkDescr
import qualified Vulkan.Core10.Device as VkDevice
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Pipeline as VkPipeline hiding (
  ComputePipelineCreateInfo(..))
import qualified Vulkan.Core10.PipelineLayout as VkPipeline
import qualified Vulkan.Core10.Shader as VkShader
import Vulkan.CStruct.Extends (SomeStruct(..))
import qualified Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.DeclM
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Internal.FragmentStream
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.PrimitiveArray
import Graphics.Shaders.Internal.Swapchain
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
    -- Vertex bindings mapped by primitive stream name.
    Map Int (VertexBinding e),
    -- Uniform binding mapped by hashed StableName of the uniform's getter.
    Map Int (UniformBinding e),
    -- Sampler binding mapped by hashed StableName of the samplers's getter.
    Map Int (SamplerBinding e)
  )

data VertexBinding e = VertexBinding {
  vertexInputDeclarations :: ByteString,
  vertexPrimitiveArrayGetter :: PrimitiveArrayGetter e
}

tellVertexBinding :: VertexBinding e -> PipelineBuilder t e Int
tellVertexBinding i = do
  (n', _, _, _, _) <- PipelineBuilder . update $
    \(n, ub, ins, ubs, sbs) -> (n + 1, ub, M.insert n i ins, ubs, sbs)
  return n'

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

data BufferGetter e = forall a. BufferGetter (e -> Buffer a)

withBufferGetter :: BufferGetter e -> e -> (forall a. Buffer a -> r) -> r
withBufferGetter (BufferGetter getter) e f = f $ getter e

data PrimitiveArrayGetter e =
  forall a t. PrimitiveArrayGetter (e -> PrimitiveArray t a)

withPrimitiveArrayGetter :: PrimitiveArrayGetter e
  -> e
  -> (forall t a. PrimitiveArray t a -> r)
  -> r
withPrimitiveArrayGetter (PrimitiveArrayGetter getter) e f = f $ getter e


-- A stream of primitives indexed by primitive topology and its vertices, to be
-- rasterized.
data PrimitiveStream t a = PrimitiveStream {
  primitiveStreamName :: Int,
  primitiveStreamVertices :: a
}

-- Transformations of vertices in a primitive stream are executed as shaders
-- on the GPU.
instance Functor (PrimitiveStream t) where
  fmap f (PrimitiveStream n a) = PrimitiveStream n (f a)

toPrimitiveStream
  :: forall e t a. (VertexInput a)
  => (e -> PrimitiveArray t a)
  -> PipelineBuilder t e (PrimitiveStream t (VertexFormat a))
toPrimitiveStream getPrimitiveArray = do
  let ToVertex _ (Kleisli buildDeclM) = toVertex
        :: ToVertex a (VertexFormat a)
      (a, inputDecls) = flip runDeclM In . buildDeclM $ undefined
      getter = PrimitiveArrayGetter getPrimitiveArray

  inName <- tellVertexBinding $ VertexBinding inputDecls getter
  return $ PrimitiveStream inName a

rasterize :: forall a e t. FragmentInput a
  => PrimitiveStream t (GLPos, a)
  -> PipelineBuilder t e (FragmentStream (FragmentFormat a))
rasterize (PrimitiveStream inName (glPos, vOut)) = do
  let ToFragment (Kleisli buildOutput) =
        toFragment :: ToFragment a (FragmentFormat a)

      ((_, vBody), vOutDecls) = flip runDeclM Out
        . flip runStateT (S (return "")) . buildOutput
        $ vOut

      ((fIn, _), fInDecls) = flip runDeclM In
        . flip runStateT (S (return "")) . buildOutput
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

  let descrSetLayoutBindings = M.elems $
        fmap uniformDescrSetLayoutBinding uniforms
          <> fmap samplerDescrSetLayoutBinding samplers

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
    VkPipeline.depthStencilState = Just $ Vk.zero {
      VkPipeline.depthCompareOp = Vk.COMPARE_OP_LESS,
      VkPipeline.depthTestEnable = True,
      VkPipeline.depthWriteEnable = True,
      VkPipeline.minDepthBounds = 0,
      VkPipeline.maxDepthBounds = 1
    },
    VkPipeline.dynamicState = Just $ Vk.zero {
      VkPipeline.dynamicStates = V.fromList [
        VkPipeline.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY,
        VkPipeline.DYNAMIC_STATE_SCISSOR,
        VkPipeline.DYNAMIC_STATE_VERTEX_INPUT_EXT,
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
    VkPipeline.vertexInputState = Nothing, -- Dynamic input state
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
