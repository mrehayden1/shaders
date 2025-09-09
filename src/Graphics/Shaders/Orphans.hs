{-# OPTIONS_GHC -Wno-orphans #-}
module Graphics.Shaders.Orphans () where

import Control.Monad.Codensity
import Control.Monad.Trans
import Control.Monad.Exception
import Control.Monad.Trans.Resource.Internal

import Graphics.Shaders.Class
import Graphics.Shaders.Logger.Class
import Graphics.Shaders.Window


instance HasVulkan m => HasVulkan (Codensity m)
instance HasVulkanDevice m => HasVulkanDevice (Codensity m)
instance HasSwapchain m => HasSwapchain (Codensity m)
instance MonadLogger m => MonadLogger (Codensity m)


instance MonadException m => MonadException (ResourceT m) where
  catch (ResourceT m) c =
    ResourceT $ \r -> m r `catch` \e -> unResourceT (c e) r
  throw = lift . throw

instance MonadAsyncException m => MonadAsyncException (ResourceT m) where
  mask a = ResourceT $ \e -> mask $ \u -> unResourceT (a $ q u) e
    where q u (ResourceT b) = ResourceT (u . b)

instance (MonadIO m, HasWindow m) => HasWindow (ResourceT m)
