module Graphics.Shaders.Render (
  Render,
  renderFrame,

  writeBuffer,

  clearWindow,
  drawWindow
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Data.Function
import qualified Data.Vector as V
import Foreign
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
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.FundamentalTypes as VkRect2D (Rect2D(..))
import qualified Vulkan.Core10.ImageView as VkImageView
import Vulkan.Core10.OtherTypes as VkBarrier (ImageMemoryBarrier(..))
import qualified Vulkan.Core10.Pipeline as VkPipeline hiding (
  ComputePipelineCreateInfo(..))
import qualified Vulkan.Core10.Queue as VkQueue
import qualified Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state
  as VkCmd
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state as VkVertexInput
import Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state as VkBind (
  VertexInputBindingDescription2EXT(..))
import Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state as VkAttr (
  VertexInputAttributeDescription2EXT(..))
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Internal.Buffer
import Graphics.Shaders.Internal.Device
import Graphics.Shaders.Internal.Instance
import Graphics.Shaders.Internal.Pipeline
import Graphics.Shaders.Internal.PrimitiveArray
import Graphics.Shaders.Internal.Swapchain
import Graphics.Shaders.Internal.Texture
import Graphics.Shaders.Logger.Class

-- A monad that brackets graphics queue commands using the current frame's
-- command buffer and finally displaying any rendered output to the window.
newtype Render m a = Render { unRender :: ContT () m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance HasVulkan m => HasVulkan (Render m)
instance HasVulkanDevice m => HasVulkanDevice (Render m)
instance HasSwapchain m => HasSwapchain (Render m)
instance MonadLogger m => MonadLogger (Render m)


renderFrame :: (MonadIO m, HasVulkanDevice m, HasSwapchain m, MonadLogger m)
  => Render m ()
  -> m ()
renderFrame (Render m) = do
  (_, Frame{..}) <- getCurrentFrame
  let SyncObjects{..} = frameSyncObjects
      commandBuffer = frameCommandBuffer

  flip runContT return $ do
    ContT $ VkCmd.useCommandBuffer commandBuffer Vk.zero . (&) ()
    m

  logTrace "Submitting graphics queue."
  let submitInfos = fmap SomeStruct . V.singleton $ Vk.zero {
    VkQueue.commandBuffers =
      fmap VkCmd.commandBufferHandle . V.fromList $ [ commandBuffer ],
    VkQueue.signalSemaphores = V.singleton syncRenderFinishedSemaphore
  }
  queue <- getQueue
  VkQueue.queueSubmit queue submitInfos syncInFlightFence
  swap

-- Queue up a buffer write in the currently rendering frame.
writeBuffer
  :: (HasSwapchain m, MonadIO m)
  => Buffer a
  -> [HostFormat a]
  -> Render m ()
writeBuffer Buffer{..} as = do
  (_, Frame{..}) <- getCurrentFrame
  let commandBuffer = frameCommandBuffer
  -- Put data in the staging buffer to be copied to the front buffer before
  -- draw commands are issued in the next frame.
  liftIO . bufferWriter $ as
  -- Transfer data in staging buffers to their writable buffer.
  let copy = V.singleton $ VkCmd.BufferCopy Vk.zero Vk.zero
               . fromIntegral $ bufferStride * bufferNumElems
  VkCmd.cmdCopyBuffer commandBuffer bufferStagingBufferHandle
    bufferHandle copy

clearWindow :: (MonadIO m, HasSwapchain m) => Render m ()
clearWindow = do
  (_, Frame{..}) <- getCurrentFrame
  ((colorImage, depthImage), _) <- getCurrentSwapImage
  let commandBuffer = frameCommandBuffer

  let imagesToClearMemoryBarriers = V.fromList [
          SomeStruct $ Vk.zero {
            VkBarrier.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
            VkBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.image = colorImage,
            VkBarrier.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VkBarrier.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
            VkBarrier.srcAccessMask = Vk.ACCESS_MEMORY_READ_BIT,
            VkBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.subresourceRange = Vk.zero {
              VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
              VkImageView.layerCount = 1,
              VkImageView.levelCount = 1
            }
          },
          SomeStruct $ Vk.zero {
            VkBarrier.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
            VkBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.image = depthImage,
            VkBarrier.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VkBarrier.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
            VkBarrier.srcAccessMask = Vk.ACCESS_MEMORY_READ_BIT,
            VkBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.subresourceRange = Vk.zero {
              VkImageView.aspectMask = Vk.IMAGE_ASPECT_DEPTH_BIT,
              VkImageView.layerCount = 1,
              VkImageView.levelCount = 1
            }
          }
        ]

  let imagesFromClearMemoryBarriers = V.fromList [
          SomeStruct $ Vk.zero {
            VkBarrier.dstAccessMask = Vk.ACCESS_MEMORY_READ_BIT,
            VkBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.image = colorImage,
            VkBarrier.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR,
            VkBarrier.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VkBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
            VkBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.subresourceRange = Vk.zero {
              VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
              VkImageView.layerCount = 1,
              VkImageView.levelCount = 1
            }
          },
          SomeStruct $ Vk.zero {
            VkBarrier.dstAccessMask = Vk.ACCESS_MEMORY_READ_BIT,
            VkBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.image = depthImage,
            VkBarrier.newLayout =
              Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
            VkBarrier.oldLayout = Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
            VkBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT,
            VkBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED,
            VkBarrier.subresourceRange = Vk.zero {
              VkImageView.aspectMask = Vk.IMAGE_ASPECT_DEPTH_BIT,
              VkImageView.layerCount = 1,
              VkImageView.levelCount = 1
            }
          }
        ]

  -- Transition images to optimal layout for clearing.
  VkCmd.cmdPipelineBarrier commandBuffer Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.zero mempty mempty
    imagesToClearMemoryBarriers

  -- Clear surface colour image.
  let colorSubresourceRange = VkImageView.ImageSubresourceRange {
          aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
          baseMipLevel = 0,
          levelCount = 1,
          baseArrayLayer = 0,
          layerCount = 1
        }
      clearColor = VkCmd.Float32 0 0 0 0

  VkCmd.cmdClearColorImage commandBuffer colorImage
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL clearColor
    . V.singleton $ colorSubresourceRange

  -- Clear depth image.
  let depthSubresourceRange = VkImageView.ImageSubresourceRange {
          aspectMask = Vk.IMAGE_ASPECT_DEPTH_BIT,
          baseMipLevel = 0,
          levelCount = 1,
          baseArrayLayer = 0,
          layerCount = 1
        }
      clearDepth = VkCmd.ClearDepthStencilValue {
          depth = 1,
          stencil = 0
        }

  VkCmd.cmdClearDepthStencilImage commandBuffer depthImage
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL clearDepth
    . V.singleton $ depthSubresourceRange

  -- Transition images back.
  VkCmd.cmdPipelineBarrier commandBuffer Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_ALL_GRAPHICS_BIT Vk.zero mempty mempty
    imagesFromClearMemoryBarriers

drawWindow :: forall m c e. (MonadIO m, MonadLogger m, HasVulkanDevice m,
    HasSwapchain m)
  => c
  -> e
  -> CompiledPipeline c e
  -> Render m ()
drawWindow push env CompiledPipeline{..} = do
  device <- getDevice
  (frameNumber, Frame{..}) <- getCurrentFrame
  let commandBuffer = frameCommandBuffer
  (_, framebuffer) <- getCurrentSwapImage

  -- Update the current frame's descriptor set to point to the correct uniform
  -- buffers and textures/samplers.
  logTrace "Updating descriptors."

  let descriptorSet = compiledPipelineDescriptorSets V.! frameNumber
      uniformDescriptorWrites = flip fmap compiledPipelineUniformInput $
        \(bind, getter) ->
          let b = withBufferGetter getter env bufferHandle
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
          let (Texture{..}, TextureSampler{..}) = getter env
          in SomeStruct $ Vk.zero {
            VkDSWrite.dstSet = descriptorSet,
            VkDSWrite.dstBinding = fromIntegral bind,
            VkDSWrite.descriptorCount = 1,
            VkDSWrite.descriptorType =
              Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            VkDSWrite.imageInfo = V.singleton $ Vk.zero {
              VkDSImage.imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
              VkDSImage.imageView = textureImageView,
              VkDSImage.sampler = textureSamplerHandle
            }
          }
      descriptorWrites = V.fromList $
        uniformDescriptorWrites <> samplerDescriptorWrites
  VkDescr.updateDescriptorSets device descriptorWrites V.empty

  -- Record draw commands.
  --
  -- Use Codensity to bracket command buffer recording and render pass.
  -- TODO We don't need to use codensity to clean up after an exception.
  flip runContT return $ do
    extent <- getSwapchainExtent
    renderPass <- getRenderPass

    -- Synchronise buffer copies with reads
    VkCmd.cmdPipelineBarrier commandBuffer Vk.PIPELINE_STAGE_TRANSFER_BIT
      Vk.PIPELINE_STAGE_ALL_GRAPHICS_BIT Vk.zero mempty mempty mempty

    let renderPassBeginInfo = Vk.zero {
      VkCmd.framebuffer = framebuffer,
      VkCmd.renderArea = Vk.zero {
        VkRect2D.extent = extent
      },
      VkCmd.renderPass = renderPass
    }

    -- Push the push constants
    case compiledPipelinePushConstant of
      Nothing -> return ()
      Just (ptr, sz, pcWriter) -> do
        liftIO $ pcWriter push
        VkCmd.cmdPushConstants
          commandBuffer
          compiledPipelineLayout
          (Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT)
          0
          (fromIntegral sz)
          ptr

    ContT $
      VkCmd.cmdUseRenderPass commandBuffer renderPassBeginInfo
        VkCmd.SUBPASS_CONTENTS_INLINE . (&) ()

    VkCmd.cmdBindPipeline commandBuffer
      Vk.PIPELINE_BIND_POINT_GRAPHICS compiledPipeline

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

    logTrace "Binding descriptors."
    VkCmd.cmdBindDescriptorSets commandBuffer
      Vk.PIPELINE_BIND_POINT_GRAPHICS
      compiledPipelineLayout
      0
      (V.singleton descriptorSet)
      V.empty

    -- Begin draw calls
    logTrace "Recording draw commands."
    withPrimitiveArrayGetter compiledPipelinePrimitiveArray env $ \pa ->
      forM_ (unPrimitiveArray pa) $ \PrimitiveArrayDrawCall{..} -> do

        let indexed = primitiveArrayIndexed
            instances = primitiveArrayInstances
            topology = primitiveTopology primitiveArrayTopology

        -- Set the vertex input state
        let bindingDescrs =
              flip fmap (zip [0..] primitiveArrayVertexBindingDescrs) $
                \(binding, (_, inputRate, _, stride)) ->
                  Vk.zero {
                    VkBind.binding = binding,
                    VkBind.inputRate = case inputRate of
                      InputPerInstance -> Vk.VERTEX_INPUT_RATE_INSTANCE
                      InputPerVertex   -> Vk.VERTEX_INPUT_RATE_VERTEX,
                    VkBind.stride = fromIntegral stride,
                    VkBind.divisor = 1
                  }

            attrDescrs =
              flip fmap primitiveArrayVertexAttrDescrs $
                \(binding, format, location, offset) ->
                  Vk.zero {
                    VkAttr.binding = fromIntegral binding,
                    VkAttr.format = format,
                    VkAttr.location = fromIntegral location,
                    VkAttr.offset = fromIntegral offset
                  }

        VkVertexInput.cmdSetVertexInputEXT
          commandBuffer
          (V.fromList bindingDescrs)
          (V.fromList attrDescrs)

        -- Bind the buffers
        let (numInstances, instanceStart) = case instances of
              Nothing           -> (1, 0)
              Just (len, start) -> (fromIntegral len, fromIntegral start)

        let (buffers, ptrOffsets) =
              unzip . flip fmap primitiveArrayVertexBindingDescrs $
                \(buffer, _, ptrOffset, _) -> (buffer, fromIntegral ptrOffset)

        VkCmd.cmdBindVertexBuffers
          commandBuffer
          0 -- First binding number
          (V.fromList buffers)
          (V.fromList ptrOffsets)

        -- Set toplogy
        VkCmd.cmdSetPrimitiveTopology commandBuffer topology

        -- Draw
        case indexed of
          Indexed IndexArray{..} -> do
            VkCmd.cmdBindIndexBuffer commandBuffer indexArrayBufferHandle
              0 indexArrayType
            VkCmd.cmdDrawIndexed commandBuffer (fromIntegral indexArrayLength)
              numInstances (fromIntegral indexArrayStart)
              (fromIntegral primitiveArrayVertexStart) instanceStart
          Unindexed numVertices ->
            VkCmd.cmdDraw commandBuffer (fromIntegral numVertices) numInstances
              (fromIntegral primitiveArrayVertexStart) instanceStart

  return ()
