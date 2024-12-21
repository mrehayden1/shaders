module Graphics.FrameBuffer (
  createRenderPass
) where

import Control.Monad.Codensity
import Control.Monad.Exception
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.Pass as VkPass hiding (FramebufferCreateInfo(..))
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import qualified Vulkan.Zero as Vk

import Graphics.Class
import Graphics.Device

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
    (VkPass.createRenderPass deviceVkDevice passCreateInfo Nothing)
    (\p-> do
       debug "Destroying render pass."
       VkPass.destroyRenderPass deviceVkDevice p Nothing
    )
