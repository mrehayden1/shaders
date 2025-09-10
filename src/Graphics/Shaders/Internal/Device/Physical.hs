module Graphics.Shaders.Internal.Device.Physical (
  PhysicalDevice(..),
  SwapchainSettings(..),

  deviceColorImageFormat,
  deviceDepthImageFormat,

  getSuitableDevices
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
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Text.Printf
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.ExtensionDiscovery as VkExtension
import Vulkan.Core10.FundamentalTypes (Extent2D(Extent2D))
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.Handles as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import Witherable

import Control.Monad.Trans.Maybe.Extra
import Data.Bits.Extra
import Graphics.Shaders.Internal.Window
import Graphics.Shaders.Logger.Class

deviceColorImageFormat :: Vk.Format
deviceColorImageFormat = Vk.FORMAT_B8G8R8A8_SRGB

deviceDepthImageFormat :: Vk.Format
deviceDepthImageFormat = Vk.FORMAT_D32_SFLOAT

-- A graphics enabled physical device, such as a GPU or CPU.
data PhysicalDevice = PhysicalDevice {
    physicalDeviceHandle :: Vk.PhysicalDevice,
    physicalDeviceMemoryProperties :: VkDevice.PhysicalDeviceMemoryProperties,
    physicalDeviceProperties :: VkDevice.PhysicalDeviceProperties,
    physicalDeviceQueueFamilyIndex :: Word32,
    physicalDeviceSwapchainSettings :: SwapchainSettings
  } deriving (Show)

-- A physical device's swap chain settings
data SwapchainSettings = SwapchainSettings {
    swapSettingsExtent :: Extent2D,
    swapSettingsImageCount :: Word32,
    swapSettingsPresentMode :: VkSurface.PresentModeKHR,
    swapSettingsSurfaceFormat :: VkSurface.SurfaceFormatKHR,
    swapSettingsTransform :: VkSurface.SurfaceTransformFlagBitsKHR
  } deriving (Show)

-- Returns a list of physical devices that have support for everything we need
-- sorted by a suitability score.
--
-- Support statistics quote below are per vulkan.gpuinfo.org
--
-- Requirements:
--
--   Surface format:
--
--   We rely on the following surface format, which is 99% supported on
--   Windows.
--
--     SurfaceFormatKHR {
--       format=FORMAT_B8G8R8A8_SRGB,
--       colorSpace=COLOR_SPACE_SRGB_NONLINEAR_KHR
--     }
--
--
--   Present mode:
--
--   Require PRESENT_MODE_IMMEDIATE_KHR (at least for now but we could fall
--   back to PRESENT_MODE_FIFO_KHR, 100% Windows support if it's not
--   available).
--
--
--   Depth format:
--
--   Require D32_SFLOAT, which as 100% Windows Support for depth stencil
--   attachments and optimal tiling.
--
getSuitableDevices :: (MonadIO m, MonadLogger m, HasWindow m)
  => Vk.Instance
  -> VkSurface.SurfaceKHR
  -> [ByteString]
  -> m [PhysicalDevice]
getSuitableDevices vkInstance surface requiredExtensions = do
  (_, devices) <- VkDevice.enumeratePhysicalDevices vkInstance
  debug . printf "%d devices found." . V.length $ devices

  devices' <- flip iwither devices $ \i device -> runMaybeT $ do
    properties <- VkDevice.getPhysicalDeviceProperties device

    let deviceName = UTF8.toString . VkDevice.deviceName $ properties
    debug . printf "Checking device %d: %s." i $ deviceName

    -- Check this device for required extension support and skip it if there
    -- are any that are unsupported.
    debug "Checking device extensions."
    (result, extensions) <-
      VkExtension.enumerateDeviceExtensionProperties device Nothing
    when (result == Vk.INCOMPLETE) $
      warn "Vulkan API returned incomplete device extension list."
    let unsupportedExtensions = (requiredExtensions \\) . V.toList
          . fmap VkExtension.extensionName $ extensions
    unless (null unsupportedExtensions) $ do
      let errStr = printf "Missing required extensions: %s." . intercalate ", "
            . fmap UTF8.toString $ unsupportedExtensions
      err errStr
      fail errStr
    debug "Found required extensions."

    -- Find the first queue family that supports everything we need, skipping
    -- the device if we can't find one.
    debug "Finding compatible device queue family."
    queueFamilyProperties <-
      VkDevice.getPhysicalDeviceQueueFamilyProperties device
    queueFamilyIndex <-
      tryFindSuitableQueueFamilyIndex surface device queueFamilyProperties
    debug . printf "Chosen queue family %d." $ queueFamilyIndex

    -- Try to get the required surface capabilities, formats, present modes.
    swapchainSupport <- tryGetSwapchainSupport device surface
    logTrace "Dumping device swap chain support."
    logTrace . show $ swapchainSupport

    -- Try to get the required depth format.
    depthFormat <- tryGetDepthFormat device
    debug . printf "Chosen depth format %s." . show $ depthFormat

    -- Get the device memory properties
    memoryProperties <- VkDevice.getPhysicalDeviceMemoryProperties device
    logTrace "Dumping device memory properties."
    logTrace . show $ memoryProperties

    let physicalDevice = PhysicalDevice {
          physicalDeviceHandle = device,
          physicalDeviceMemoryProperties = memoryProperties,
          physicalDeviceProperties = properties,
          physicalDeviceQueueFamilyIndex = queueFamilyIndex,
          physicalDeviceSwapchainSettings = swapchainSupport
        }

    return physicalDevice

  -- Finally sort all the device by a suitability score and pick the best.
  return . sortBy (compare `on` Down . scoreSuitability) . V.toList
    $ devices'

scoreSuitability :: PhysicalDevice -> Int
scoreSuitability device =
  case VkDevice.deviceType . physicalDeviceProperties $ device of
    Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> 10
    Vk.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> 9
    Vk.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> 8
    Vk.PHYSICAL_DEVICE_TYPE_CPU -> 7
    Vk.PHYSICAL_DEVICE_TYPE_OTHER -> 0

-- Try to get swap chain info, failing when the surface support doesn't meet
-- requirements.
tryGetSwapchainSupport :: (MonadIO m, MonadLogger m, HasWindow m)
  => Vk.PhysicalDevice
  -> VkSurface.SurfaceKHR
  -> MaybeT m SwapchainSettings
tryGetSwapchainSupport device surface = do
  -- Note that we've already checked if there is a queue family that supports
  -- the VK_KHR_surface extension on this device as required by
  -- `vkGetPhysicalDeviceSurfaceCapabilitiesKHR`.
  debug "Getting device surface capabilities."
  surfaceCapabilities <- VkSurface.getPhysicalDeviceSurfaceCapabilitiesKHR
                           device
                           surface
  logTrace . show $ surfaceCapabilities

  -- Calculate the swap image extent.
  debug "Calculating swap image extent."
  let surfaceCurrentExtent = VkSurface.currentExtent surfaceCapabilities
  (extentWidth, extentHeight) <-
    if VkExtent2D.width surfaceCurrentExtent /= maxBound
      then return (
               VkExtent2D.width surfaceCurrentExtent,
               VkExtent2D.height surfaceCurrentExtent
             )
      else do
        -- We get the frame buffer size rather than the original window size
        -- so we get device pixels.
        (frameBufferWidth, frameBufferHeight) <- lift getWindowBufferSize
        let minImageExtent = VkSurface.minImageExtent surfaceCapabilities
            maxImageExtent = VkSurface.maxImageExtent surfaceCapabilities
            widthBounds = (VkExtent2D.width minImageExtent,
                           VkExtent2D.width maxImageExtent)
            heightBounds = (VkExtent2D.height minImageExtent,
                            VkExtent2D.height maxImageExtent)
        return (
            clamp widthBounds . fromIntegral $ frameBufferWidth,
            clamp heightBounds . fromIntegral $ frameBufferHeight
          )
  debug . printf "Got extent %dpx × %dpx." extentWidth $ extentHeight

  -- Calculate image count
  debug "Calculating swap image count."
  let minImageCount = VkSurface.minImageCount surfaceCapabilities
      maxImageCount = if VkSurface.maxImageCount surfaceCapabilities == 0
                        then maxBound
                        else VkSurface.maxImageCount surfaceCapabilities
      imageCount = min maxImageCount $ minImageCount + 1
  debug . printf "Choosing %d swap images." $ imageCount

  let transform = VkSurface.currentTransform surfaceCapabilities
  debug . printf "Using transform %s." . show $ transform

  -- Make sure we have the surface format we're looking for, abort if not.
  debug "Checking device surface formats."
  (res0, surfaceFormats) <- VkSurface.getPhysicalDeviceSurfaceFormatsKHR
                              device
                              surface
  when (res0 == Vk.INCOMPLETE) $
    warn "Vulkan API returned incomplete surface formats list."
  logTrace . show $ surfaceFormats
  let requiredSurfaceFormat = VkSurface.SurfaceFormatKHR {
                          VkSurface.colorSpace =
                            VkSurface.COLOR_SPACE_SRGB_NONLINEAR_KHR,
                          VkSurface.format = deviceColorImageFormat
                        }
      hasSurfaceFormat = elem requiredSurfaceFormat . V.toList $ surfaceFormats
  unless hasSurfaceFormat $ do
    let errStr = printf "Missing required surface format %s" . show
                   $ requiredSurfaceFormat
    err errStr
    fail errStr
  debug . printf "Chosen surface format %s." . show $ requiredSurfaceFormat

  -- Make sure we have the present mode we want.
  debug "Checking deivce present modes."
  (res1, presentModes) <- VkSurface.getPhysicalDeviceSurfacePresentModesKHR
                            device
                            surface
  when (res1 == Vk.INCOMPLETE) $
    warn "Vulkan API returned incomplete present modes list."
  logTrace . show $ presentModes
  let presentMode = VkSurface.PRESENT_MODE_IMMEDIATE_KHR
      hasPresentMode = presentMode `elem` presentModes
  unless hasPresentMode $ do
    let errStr = "Missing required present mode PRESENT_MODE_IMMEDIATE_KHR."
    err errStr
    fail errStr
  debug . printf "Chosen present mode %s." . show $ presentMode

  return $ SwapchainSettings {
      swapSettingsExtent = Extent2D extentWidth extentHeight,
      swapSettingsImageCount = imageCount,
      swapSettingsPresentMode = presentMode,
      swapSettingsSurfaceFormat = requiredSurfaceFormat,
      swapSettingsTransform = transform
    }

tryGetDepthFormat :: (MonadIO m, MonadLogger m)
  => Vk.PhysicalDevice
  -> MaybeT m Vk.Format
tryGetDepthFormat device = do
  debug "Getting required depth format."
  formatProperties <- VkDevice.getPhysicalDeviceFormatProperties device
    deviceDepthImageFormat
  let hasDepthFormat =
        (.?. VkDevice.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)
          . VkDevice.optimalTilingFeatures
          $ formatProperties
  unless hasDepthFormat $ do
    let errStr = printf "Missing required depth format %s" . show
          $ deviceDepthImageFormat
    err errStr
    fail errStr
  return deviceDepthImageFormat

-- Checks that a device has at least one queue family that can do everything
-- we want and try to pick it. This is pretty much always going to be possible.
tryFindSuitableQueueFamilyIndex :: forall m. (MonadIO m, MonadLogger m)
  => VkSurface.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vector VkDevice.QueueFamilyProperties
  -> MaybeT m Word32
tryFindSuitableQueueFamilyIndex surface device queueFamilyProperties = do
  queueFamilies <- filterA (uncurry isSuitable) . zip [0..] . V.toList
                     $ queueFamilyProperties
  when (null queueFamilies) $ do
    let errStr = "No compatible queue family."
    err errStr
    fail errStr
  hoistMaybe . listToMaybe . fmap fst $ queueFamilies
 where
  -- Some queue family properties we can get from the properties object, some
  -- we have to request via extension API calls, hence why we're doing this in
  -- IO.
  isSuitable :: Word32 -> VkDevice.QueueFamilyProperties -> MaybeT m Bool
  isSuitable i queue = do
    logTrace . printf "Checking queue family %d..." $ i
    let supportsFlags = hasFlags queue
    logTrace . ("Supports flags: " ++) $
      if supportsFlags then "True" else "False"
    canSurface <- checkSurfaceSupport i
    return $ canSurface && supportsFlags

  hasFlags = (== flags) . (.&. flags) . VkDevice.queueFlags

  flags = Vk.QUEUE_GRAPHICS_BIT .|. Vk.QUEUE_TRANSFER_BIT

  checkSurfaceSupport i = do
    supportsSurface <- VkSurface.getPhysicalDeviceSurfaceSupportKHR device i
      surface
    logTrace . ("Has surface support: " ++) $
      if supportsSurface then "True" else "False"
    return supportsSurface
