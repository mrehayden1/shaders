module Graphics.Device (
  Device(..),
  createDevice,
  destroyDevice
) where

import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Function
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.Vector (Vector)
import Data.Word
import qualified Data.Vector as V
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import Vulkan.CStruct.Extends

-- Abstract representation of a graphics enabled device.
data Device = Device {
  deviceVkDevice :: Vk.Device,
  deviceQueueHandle :: Vk.Queue
} deriving (Show)

createDevice :: Vk.Instance -> IO (Maybe Device)
createDevice vkInstance = runMaybeT $ do
  physicalDevice <- MaybeT $ getMostSuitablePhysicalDevice vkInstance
  queueFamilyIndex <- MaybeT . pure
                        $ findSuitableQueueFamilyIndex physicalDevice
  let queueCreateInfo = pure . SomeStruct
        . Vk.DeviceQueueCreateInfo () Vk.zero queueFamilyIndex
        . pure $ 1
      deviceCreateInfo = Vk.DeviceCreateInfo () Vk.zero queueCreateInfo V.empty
                           V.empty Nothing
  vkDevice <- Vk.createDevice (physicalDeviceHandle physicalDevice)
                deviceCreateInfo Nothing

  queue <- Vk.getDeviceQueue vkDevice queueFamilyIndex 0

  return . Device vkDevice $ queue

destroyDevice :: Device -> IO ()
destroyDevice Device{..} = do
  Vk.destroyDevice deviceVkDevice Nothing

data PhysicalDevice = PhysicalDevice {
    physicalDeviceHandle :: Vk.PhysicalDevice,
    physicalDeviceFeatures :: Vk.PhysicalDeviceFeatures,
    physicalDeviceProperties :: Vk.PhysicalDeviceProperties,
    physicalDeviceQueueFamilies :: Vector Vk.QueueFamilyProperties
  } deriving (Show)

getAllDevices :: Vk.Instance -> IO (Vector PhysicalDevice)
getAllDevices vkInstance = do
  (_, devices) <- Vk.enumeratePhysicalDevices vkInstance
  --  Get the devices and their properties and features.
  properties <- mapM Vk.getPhysicalDeviceProperties devices
  features <- mapM Vk.getPhysicalDeviceFeatures devices
  queueFamilyProperties <- mapM Vk.getPhysicalDeviceQueueFamilyProperties
                             devices
  return $ PhysicalDevice <$> devices <*> features <*> properties
    <*> queueFamilyProperties

getMostSuitablePhysicalDevice :: Vk.Instance -> IO (Maybe PhysicalDevice)
getMostSuitablePhysicalDevice inst = do
  devices <- getAllDevices inst
  -- Remove devices that don't support the features we need and sort by
  -- suitability score.
  return . listToMaybe . sortBy (compare `on` Down . scoreSuitability)
    . filter isSuitable . V.toList $ devices
 where
  isSuitable :: PhysicalDevice -> Bool
  isSuitable = isJust . findSuitableQueueFamilyIndex

  scoreSuitability :: PhysicalDevice -> Int
  scoreSuitability device =
    case Vk.deviceType . physicalDeviceProperties $ device of
      Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
      Vk.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 9
      Vk.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 8
      Vk.PHYSICAL_DEVICE_TYPE_CPU -> 7
      Vk.PHYSICAL_DEVICE_TYPE_OTHER -> 0

findSuitableQueueFamilyIndex :: PhysicalDevice -> Maybe Word32
findSuitableQueueFamilyIndex PhysicalDevice{..} =
  -- Check the device has at least one queue family that can do everything we
  -- want. This is almost always going to be the case.
  fmap fromIntegral
    . V.findIndex isSuitable
    $ physicalDeviceQueueFamilies
 where
  isSuitable = (/= Vk.zero) . (.&. Vk.QUEUE_GRAPHICS_BIT) . Vk.queueFlags
