module Graphics.Device (
  Device(..),
  createDevice,
  destroyDevice
) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString (ByteString)
import Data.Function
import Data.List ((\\), sortBy)
import Data.Maybe
import Data.Ord
import Data.Word
import qualified Data.Vector as V
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import Vulkan.CStruct.Extends
import Witherable

-- Abstract representation of a graphics enabled device.
data Device = Device {
  deviceVkDevice :: Vk.Device,
  deviceQueueHandle :: Vk.Queue
} deriving (Show)

requiredDeviceExtensions :: [ByteString]
requiredDeviceExtensions = [Vk.KHR_SWAPCHAIN_EXTENSION_NAME]

createDevice :: Vk.Instance -> Vk.SurfaceKHR -> IO (Maybe Device)
createDevice vkInstance surface = runMaybeT $ do
  physicalDevice <- MaybeT $ getMostSuitablePhysicalDevice vkInstance surface
  queueFamilyIndex <- MaybeT . findSuitableQueueFamilyIndex surface
    $ physicalDevice

  let queueCreateInfo = pure . SomeStruct $
        Vk.DeviceQueueCreateInfo
          ()
          Vk.zero
          queueFamilyIndex
          (pure 1)

      deviceCreateInfo =
        Vk.DeviceCreateInfo
          ()
          Vk.zero
          queueCreateInfo
          V.empty
          (V.fromList requiredDeviceExtensions)
          Nothing

  vkDevice <- Vk.createDevice (physicalDeviceHandle physicalDevice)
                deviceCreateInfo Nothing

  queue <- Vk.getDeviceQueue vkDevice queueFamilyIndex 0

  return . Device vkDevice $ queue

destroyDevice :: Device -> IO ()
destroyDevice Device{..} = do
  Vk.destroyDevice deviceVkDevice Nothing

data PhysicalDevice = PhysicalDevice {
    physicalDeviceHandle :: Vk.PhysicalDevice,
    physicalDeviceExtensions :: [Vk.ExtensionProperties],
    physicalDeviceFeatures :: Vk.PhysicalDeviceFeatures,
    physicalDeviceProperties :: Vk.PhysicalDeviceProperties,
    physicalDeviceQueueFamilies :: [Vk.QueueFamilyProperties]
  } deriving (Show)

getAllDevices :: Vk.Instance -> IO [PhysicalDevice]
getAllDevices vkInstance = do
  (_, devices) <- Vk.enumeratePhysicalDevices vkInstance
  let devices' = V.toList devices
  --  Get the devices and their properties and features.
  properties <- mapM Vk.getPhysicalDeviceProperties devices'
  features <- mapM Vk.getPhysicalDeviceFeatures devices'
  queueFamilyProperties <-
    mapM (fmap V.toList . Vk.getPhysicalDeviceQueueFamilyProperties) devices'
  extensions <-
    mapM (fmap (V.toList . snd)
            . (`Vk.enumerateDeviceExtensionProperties` Nothing))
         devices'

  return . getZipList $ PhysicalDevice
    <$> ZipList devices'
    <*> ZipList extensions
    <*> ZipList features
    <*> ZipList properties
    <*> ZipList queueFamilyProperties

getMostSuitablePhysicalDevice :: Vk.Instance
  -> Vk.SurfaceKHR
  -> IO (Maybe PhysicalDevice)
getMostSuitablePhysicalDevice vkInstance surface = do
  devices <- getAllDevices vkInstance
  -- Remove devices that don't support the features we need and sort by
  -- suitability.
  fmap (listToMaybe . sortBy (compare `on` Down . scoreSuitability))
    . filterA isSuitable $ devices
 where
  isSuitable :: PhysicalDevice -> IO Bool
  isSuitable device = do
    -- Check the device for required extension support.
    let hasExtensions = null . (requiredDeviceExtensions \\)
          . fmap Vk.extensionName . physicalDeviceExtensions $ device
    -- Make sure there is a suitable queue family.
    hasQueueFamily <-
      fmap isJust . findSuitableQueueFamilyIndex surface $ device
    return $ hasQueueFamily && hasExtensions

  scoreSuitability :: PhysicalDevice -> Int
  scoreSuitability device =
    case Vk.deviceType . physicalDeviceProperties $ device of
      Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
      Vk.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 9
      Vk.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 8
      Vk.PHYSICAL_DEVICE_TYPE_CPU -> 7
      Vk.PHYSICAL_DEVICE_TYPE_OTHER -> 0

findSuitableQueueFamilyIndex :: Vk.SurfaceKHR
  -> PhysicalDevice
  -> IO (Maybe Word32)
findSuitableQueueFamilyIndex surface PhysicalDevice{..} =
  -- Check the device has at least one queue family that can do everything we
  -- want and pick it. This is pretty much always going to be the case.
  fmap (listToMaybe . fmap fst)
    . filterA (uncurry isSuitable) . zip [0..]
    $ physicalDeviceQueueFamilies
 where
  -- Some queue family properties we can get from the properties object, some
  -- we have to request via extension APIs, hence why we're doing this in IO.
  isSuitable :: Word32 -> Vk.QueueFamilyProperties -> IO Bool
  isSuitable i queue = do
    canSurface <- supportsSurface i
    return . (&& canSurface) . supportsGraphics $ queue

  supportsGraphics = (/= Vk.zero) . (.&. Vk.QUEUE_GRAPHICS_BIT)
    . Vk.queueFlags

  supportsSurface i = Vk.getPhysicalDeviceSurfaceSupportKHR
                        physicalDeviceHandle i surface
