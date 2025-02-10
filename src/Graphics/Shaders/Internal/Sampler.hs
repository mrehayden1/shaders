module Graphics.Shaders.Internal.Sampler (
  getSampler,
  Sampler(..),

  texture
) where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Bits
import Data.ByteString.Char8 as BS
import qualified Data.Map as M
import System.Mem.StableName
import Vulkan.Core10.DescriptorSet as VkDS
import Vulkan.Core10.DescriptorSet as VkDSLayout (
  DescriptorSetLayoutBinding(..))
import Vulkan.Core10.Enums as Vk
import Vulkan.Zero as Vk

import Control.Monad.State.Extra
import Data.Linear
import Graphics.Shaders.Internal.Expr
import Graphics.Shaders.Internal.Pipeline
import Graphics.Shaders.Internal.Texture

data Sampler = Sampler

getSampler :: (e -> Texture) -> Pipeline t e (S x Sampler)
getSampler getter = do
  -- Hash the getter StableName and look it up in the cache so we don't keep
  -- rebinding the uniform it's for.
  hash <- liftIO $ hashStableName <$> makeStableName getter
  bindings <- Pipeline . gets $ \(_, _, _, _, sbs) -> sbs
  let hasBinding = hash `M.member` bindings

  -- Get the uniform binding location.
  bind <- if hasBinding
            -- If it's already been bound return that,
            then return . samplerBindingNumber $ bindings M.! hash
            -- else use the next available one.
            else do
              (_, bind, _, _, _) <- Pipeline . update $
                \(n, un, ins, ubs, sbs) -> (n, un + 1, ins, ubs, sbs)
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

  unless hasBinding . Pipeline $ do
    modify $ \(n, un, ins, ubs, sbs) ->
      let sb = SamplerBinding {
        samplerBindingNumber = bind,
        samplerDeclaration = decl <> "\n",
        samplerDescrSetLayoutBinding = descrSetLayoutBinding,
        samplerTextureGetter = getter
      }
      in (n, un, ins, ubs, M.insert hash sb sbs)

  return . S . return $ "un" <> bind'


-- Texture sampler shader expressions

texture :: S x Sampler -> S x (V2 Float) -> S x (V4 Float)
texture s p = S $ do
  s' <- unS s
  p' <- unS p
  tellAssignment "vec4" $ "texture(" <> s' <> ", " <> p' <> ")"
