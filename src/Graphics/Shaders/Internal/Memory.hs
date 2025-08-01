module Graphics.Shaders.Internal.Memory (
  allocateMemory
) where

import Control.Monad
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.Vector as V
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Core10.Memory as VkMemory
import qualified Vulkan.Core10.MemoryManagement as VkMemRequirements
import qualified Vulkan.Zero as Vk

import Data.Bits.Extra
import Graphics.Shaders.Base
import Graphics.Shaders.Exception
import Graphics.Shaders.Logger.Class

allocateMemory :: (MonadAsyncException m, MonadLogger m,
  MonadReader GraphicsEnv m, MonadResource m)
  => VkDevice.PhysicalDeviceMemoryProperties
  -> VkMemRequirements.MemoryRequirements
  -> Vk.MemoryPropertyFlags
  -> m (ReleaseKey, Vk.DeviceMemory)
allocateMemory memoryProperties memoryRequirements memoryPropertyFlags = do
  deviceHandle <- getDeviceHandle
  -- Find compatible memory
  let memoryTypeIndices = V.imapMaybe getMemoryTypeIndexIfCompatible
        . VkDevice.memoryTypes
        $ memoryProperties

  when (V.null memoryTypeIndices) $ do
    let msg = "No suitable memory type found."
    err msg
    throw . ShadersMemoryException $ msg

  let memoryTypeIndex = V.head memoryTypeIndices

  let allocInfo = Vk.zero {
        VkMemory.allocationSize = VkMemRequirements.size memoryRequirements,
        VkMemory.memoryTypeIndex = fromIntegral memoryTypeIndex
      }
  allocate (VkMemory.allocateMemory deviceHandle allocInfo Nothing)
    (\m -> VkMemory.freeMemory deviceHandle m Nothing)
 where
  getMemoryTypeIndexIfCompatible i t =
    if VkDevice.propertyFlags t .&&. memoryPropertyFlags
         && testBit (VkMemRequirements.memoryTypeBits memoryRequirements) i
      then Just i
      else Nothing
