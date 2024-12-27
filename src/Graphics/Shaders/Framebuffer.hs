module Graphics.Shaders.Framebuffer (
  createRenderPass,
  createFramebuffers
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Exception
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.Pass as VkPass hiding (
  FramebufferCreateInfo(..))
import qualified Vulkan.Core10.Pass as VkFramebuffer hiding (
  RenderPassCreateInfo(..))
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import qualified Vulkan.Zero as Vk

import Graphics.Shaders.Class
import Graphics.Shaders.Device

createRenderPass :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Codensity m Vk.RenderPass
createRenderPass Device{..} = do
  debug "Creating render pass."
  let passCreateInfo = Vk.zero {
          VkPass.attachments = V.fromList [
              Vk.zero {
                VkPass.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR,
                VkPass.format = VkSurface.format . swapChainFormat
                  $ deviceSwapChain,
                VkPass.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED,
                VkPass.loadOp = VkPass.ATTACHMENT_LOAD_OP_CLEAR,
                VkPass.samples = Vk.SAMPLE_COUNT_1_BIT,
                VkPass.stencilLoadOp = VkPass.ATTACHMENT_LOAD_OP_DONT_CARE,
                VkPass.stencilStoreOp = VkPass.ATTACHMENT_STORE_OP_DONT_CARE,
                VkPass.storeOp = VkPass.ATTACHMENT_STORE_OP_STORE
              }
            ],
          VkPass.subpasses = V.fromList [
              Vk.zero {
                  VkPass.colorAttachments = V.singleton $ Vk.zero {
                      VkPass.attachment = 0,
                      VkPass.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                    },
                  VkPass.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
                }
            ]
        }

  Codensity $ bracket
    (VkPass.createRenderPass deviceHandle passCreateInfo Nothing)
    (\p -> do
       debug "Destroying render pass."
       VkPass.destroyRenderPass deviceHandle p Nothing
    )

createFramebuffers :: (MonadAsyncException m, MonadLogger m)
  => Device
  -> Vk.RenderPass
  -> Codensity m (Vector Vk.Framebuffer)
createFramebuffers Device{..} renderPass = do
  debug "Creating framebuffers."
  let SwapChain{..} = deviceSwapChain
  Codensity $ bracket
    (forM swapChainImageViews $ \imageView -> do
        let framebufferCreateInfo = Vk.zero {
          VkFramebuffer.attachments = V.singleton imageView,
          VkFramebuffer.height = VkExtent2D.height swapChainExtent,
          VkFramebuffer.layers = 1,
          VkFramebuffer.renderPass = renderPass,
          VkFramebuffer.width = VkExtent2D.width swapChainExtent
        }
        VkFramebuffer.createFramebuffer deviceHandle framebufferCreateInfo Nothing
    )
    (\framebuffers -> do
       debug "Destroying framebuffers."
       forM_ framebuffers $ \framebuffer ->
         VkFramebuffer.destroyFramebuffer deviceHandle framebuffer Nothing
    )
