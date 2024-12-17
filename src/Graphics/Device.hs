module Graphics.Device (
  Device(..),
  createDevice,
  destroyDevice
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString.UTF8 as UTF8
import Data.Function
import Data.List ((\\), intercalate, sortBy)
import Data.Maybe
import Data.Ord
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import Vulkan.CStruct.Extends
import Witherable

import Graphics.Class

-- A graphics enabled logical device.
data Device = Device {
  deviceVkDevice :: Vk.Device,
  deviceQueueHandle :: Vk.Queue
} deriving (Show)

requiredDeviceExtensions :: [ByteString]
requiredDeviceExtensions = [Vk.KHR_SWAPCHAIN_EXTENSION_NAME]

createDevice :: (MonadIO m, MonadLogger m)
  => Vk.Instance
  -> Vk.SurfaceKHR
  -> m (Maybe Device)
createDevice vkInstance surface = runMaybeT $ do
  devices <- lift $ getSuitableDevices vkInstance surface

  PhysicalDevice{..} <- hoistMaybe . listToMaybe $ devices
  let deviceHandle = physicalDeviceHandle
      deviceName = Vk.deviceName physicalDeviceProperties
  lift . info $ "Chosen physical device: " <> UTF8.toString deviceName

  let queueCreateInfo = pure . SomeStruct $
        Vk.DeviceQueueCreateInfo
          ()
          Vk.zero
          physicalDeviceQueueFamilyIndex
          (V.singleton 1)

      deviceCreateInfo =
        Vk.DeviceCreateInfo
          ()
          Vk.zero
          queueCreateInfo
          V.empty
          (V.fromList requiredDeviceExtensions)
          Nothing

  -- Create the logical device.
  vkDevice <- Vk.createDevice deviceHandle deviceCreateInfo Nothing
  -- Get the queue that was just created too.
  queue <- Vk.getDeviceQueue vkDevice physicalDeviceQueueFamilyIndex 0

  return . Device vkDevice $ queue

destroyDevice :: MonadIO m => Device -> m ()
destroyDevice Device{..} = do
  liftIO $ Vk.destroyDevice deviceVkDevice Nothing

-- A graphics enabled physical device, such as a GPU or CPU.
data PhysicalDevice = PhysicalDevice {
    physicalDeviceHandle :: Vk.PhysicalDevice,
    physicalDeviceProperties :: Vk.PhysicalDeviceProperties,
    physicalDeviceQueueFamilyIndex :: Word32,
    physicalDeviceSwapChainSupport :: SwapChainSupport
  } deriving (Show)

data SwapChainSupport = SwapChainSupport {
  } deriving (Show)

-- Returns a list of physical devices that have support everything we need
-- sorted by a suitability score.
getSuitableDevices :: (MonadIO m, MonadLogger m)
  => Vk.Instance
  -> Vk.SurfaceKHR
  -> m [PhysicalDevice]
getSuitableDevices vkInstance surface = do
  (_, devices) <- Vk.enumeratePhysicalDevices vkInstance
  debug . printf "%d devices found." . V.length $ devices

  devices' <- flip iwither devices $ \i device -> runMaybeT $ do
    properties <- Vk.getPhysicalDeviceProperties device

    let deviceName = UTF8.toString . Vk.deviceName $ properties
    lift . debug . printf "Checking device %d: %s." i $ deviceName

    -- Check this device for required extension support and skip it if there
    -- are any that are unsupported.
    lift . debug . printf $ "Checking device extensions."
    (result, extensions) <-
      Vk.enumerateDeviceExtensionProperties device Nothing
    when (result == Vk.INCOMPLETE) $
      lift $ warn "Vulkan API returned incomplete device extension list."
    let unsupportedExtensions = (requiredDeviceExtensions \\) . V.toList
          . fmap Vk.extensionName $ extensions
    unless (null unsupportedExtensions) $ do
      let errStr = printf "Missing required extensions: %s." . intercalate ", "
            . fmap UTF8.toString $ unsupportedExtensions
      lift . debug $ errStr
      fail errStr
    lift . debug . printf $ "Found required extensions."

    -- Find the first queue family that supports everything we need, skipping
    -- the device if we can't find one.
    lift . debug . printf $ "Finding compatible device queue family."
    queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties device
    queueFamilyIndex <-
      findSuitableQueueFamilyIndex surface device queueFamilyProperties
    lift . debug . printf "Using queue family %d." $ queueFamilyIndex

    -- Get the surface capabilities, formats and present modes.
    swapChainSupport <- getDeviceSwapChainSupport surface device

    let physicalDevice = PhysicalDevice device properties queueFamilyIndex
                           swapChainSupport

    return physicalDevice

  -- Finally sort all the device by a suitability score and pick the best.
  return . sortBy (compare `on` Down . scoreSuitability) . V.toList
    $ devices'

scoreSuitability :: PhysicalDevice -> Int
scoreSuitability device =
  case Vk.deviceType . physicalDeviceProperties $ device of
    Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
    Vk.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 9
    Vk.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 8
    Vk.PHYSICAL_DEVICE_TYPE_CPU -> 7
    Vk.PHYSICAL_DEVICE_TYPE_OTHER -> 0

-- Try to get swap chain info, failing when the surface support doesn't meet
-- requirements.
--
--   Surface format we require just one true colour, SRGB format:
--   {format=FORMAT_B8G8R8A8_SRGB, colorSpace=COLOR_SPACE_SRGB_NONLINEAR_KHR}
--   which is 99.9% supported on Windows.
--
--   Present mode: PRESENT_MODE_IMMEDIATE_KHR is our choice for development
--   but we could fall back to PRESENT_MODE_FIFO_KHR (100% Windows support) if
--   its not available.
--
getDeviceSwapChainSupport :: (MonadIO m, MonadLogger m)
  => Vk.SurfaceKHR
  -> Vk.PhysicalDevice
  -> MaybeT m SwapChainSupport
getDeviceSwapChainSupport surface device = do
  -- Note that we've already checked if there is a queue family that supports
  -- the VK_KHR_surface extension as required by
  -- `vkGetPhysicalDeviceSurfaceCapabilitiesKHR`.
  lift . debug . printf $ "Getting device surface capabilities."
  surfaceCapabilities <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR device
                           surface
  lift . trace . show $ surfaceCapabilities

  -- Make sure we have the surface format we're looking for, abort if not.
  lift . debug . printf $ "Checking device surface formats."
  (res0, surfaceFormats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR device
                                surface
  when (res0 == Vk.INCOMPLETE) $
    lift $ warn "Vulkan API returned incomplete surface formats list."
  lift . trace . show $ surfaceFormats
  let hasSurfaceFormat = any isCompatibleSurfaceFormat . V.toList
        $ surfaceFormats
  unless hasSurfaceFormat $ do
    let errStr = "Missing required surface format {format=FORMAT_B8G8R8A8_SRGB"
                   <> ", colorSpace=COLOR_SPACE_SRGB_NONLINEAR_KHR}."
    lift . debug $ errStr
    fail errStr
  lift . debug . printf $ "Found compatible surface format."

  -- Make sure we have the present mode we want.
  lift . debug . printf $ "Checking deivce present modes."
  (res1, presentModes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR device
                              surface
  when (res1 == Vk.INCOMPLETE) $
    lift $ warn "Vulkan API returned incomplete present modes list."
  lift . trace . show $ presentModes
  let hasPresentMode = Vk.PRESENT_MODE_IMMEDIATE_KHR `elem` presentModes
  unless hasPresentMode $ do
    let errStr = "Missing required present mode PRESENT_MODE_IMMEDIATE_KHR."
    lift . debug $ errStr
    fail errStr
  lift . debug . printf $ "Found compatible present mode."

  return SwapChainSupport

 where
  isCompatibleSurfaceFormat Vk.SurfaceFormatKHR{..} =
    format == Vk.FORMAT_B8G8R8A8_SRGB
      && colorSpace == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR

-- Checks a device has at least one queue family that can do everything we want
-- and pick it. This is pretty much always going to be the case.
findSuitableQueueFamilyIndex :: forall m. (MonadIO m, MonadLogger m)
  => Vk.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vector Vk.QueueFamilyProperties
  -> MaybeT m Word32
findSuitableQueueFamilyIndex surface device queueFamilyProperties = do
  queueFamilies <- filterA (uncurry isSuitable) . zip [0..] . V.toList
                     $ queueFamilyProperties
  when (null queueFamilies) $ do
    let errStr = "No compatible queue family."
    lift $ debug errStr
    fail errStr
  hoistMaybe . listToMaybe . fmap fst $ queueFamilies
 where
  -- Some queue family properties we can get from the properties object, some
  -- we have to request via extension API calls, hence why we're doing this in
  -- IO.
  isSuitable :: Word32 -> Vk.QueueFamilyProperties -> MaybeT m Bool
  isSuitable i queue = do
    canSurface <- liftIO $ supportsSurface i
    return . (&& canSurface) . supportsGraphics $ queue

  supportsGraphics = (/= Vk.zero) . (.&. Vk.QUEUE_GRAPHICS_BIT) . Vk.queueFlags

  supportsSurface i = Vk.getPhysicalDeviceSurfaceSupportKHR device i surface

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
