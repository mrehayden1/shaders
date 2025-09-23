module Graphics.Shaders.Internal.Sampler (
  getSampler,
  Sampler(..),

  sample
) where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Bits
import Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Linear
import System.Mem.StableName
import Vulkan.Core10.DescriptorSet as VkDS
import Vulkan.Core10.DescriptorSet as VkDSLayout (
  DescriptorSetLayoutBinding(..))
import Vulkan.Core10.Enums as Vk
import Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Internal.Pipeline
import Graphics.Shaders.Internal.Texture

data Sampler = Sampler

getSampler :: (e -> Texture) -> PipelineBuilder t c e (S x Sampler)
getSampler getter = do
  -- Hash the getter StableName and look it up in the cache so we don't keep
  -- rebinding the uniform it's for.
  hash <- liftIO . fmap hashStableName . makeStableName $! getter
  bindings <- PipelineBuilder . gets $ \(_, _, _, _, sbs, _) -> sbs
  let hasBinding = hash `M.member` bindings

  -- Get the uniform binding location.
  bind <- if hasBinding
    -- If it's already been bound return that,
    then return . samplerBindingNumber $ bindings M.! hash
    -- else use the next available one.
    else do
      (_, bind, _, _, _, _) <- PipelineBuilder . update $
        \(n, un, ins, ubs, sbs, pc) -> (n, un + 1, ins, ubs, sbs, pc)
      return bind

  let bind' = BS.pack . show $ bind

      decl = "layout(binding = " <> bind' <> ") uniform sampler2D un" <> bind'
        <> ";"

      descrSetLayoutBinding = Vk.zero {
        VkDSLayout.binding = fromIntegral bind,
        VkDSLayout.descriptorCount = 1,
        VkDSLayout.descriptorType =
          VkDS.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        VkDSLayout.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
          .|. Vk.SHADER_STAGE_FRAGMENT_BIT
      }

  unless hasBinding . PipelineBuilder $ do
    modify $ \(n, un, ins, ubs, sbs, pc) ->
      let sb = SamplerBinding {
        samplerBindingNumber = bind,
        samplerDeclaration = decl <> "\n",
        samplerDescrSetLayoutBinding = descrSetLayoutBinding,
        samplerTextureGetter = getter
      }
      in (n, un, ins, ubs, M.insert hash sb sbs, pc)

  return . S . return $ "un" <> bind'


-- Texture sampler shader expressions, i.e. the GLSL `texture` function.
sample :: S x Sampler -> V2 (S x Float) -> V4 (S x Float)
sample s (V2 u v) =
  toV4S' . tellAssignment GlVec4 $ do
    s' <- unS s
    u' <- unS u
    v' <- unS v
    return $ "texture(" <> s' <> ", vec2(" <> u' <> ", " <> v' <> "))"
