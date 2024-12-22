module Graphics.Shaders.Device.Physical (
  PhysicalDevice(..),
  SwapChainSettings(..),

  getSuitableDevices
) where

import Control.Monad
import Control.Monad.IO.Class
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
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import qualified Vulkan.Core10 as Vk
import Vulkan.Core10.FundamentalTypes (Extent2D(Extent2D))
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import qualified Vulkan.Zero as Vk
import Witherable

import Graphics.Shaders.Class

-- A graphics enabled physical device, such as a GPU or CPU.
data PhysicalDevice = PhysicalDevice {
    physicalDeviceHandle :: Vk.PhysicalDevice,
    physicalDeviceProperties :: Vk.PhysicalDeviceProperties,
    physicalDeviceQueueFamilyIndex :: Word32,
    physicalDeviceSwapChainSettings :: SwapChainSettings
  } deriving (Show)

-- A physical device's swap chain settings
data SwapChainSettings = SwapChainSettings {
    swapSettingsExtent :: Extent2D,
    swapSettingsImageCount :: Word32,
    swapSettingsPresentMode :: VkSurface.PresentModeKHR,
    swapSettingsSurfaceFormat :: VkSurface.SurfaceFormatKHR,
    swapSettingsTransform :: VkSurface.SurfaceTransformFlagBitsKHR
  } deriving (Show)

-- Returns a list of physical devices that have support everything we need
-- sorted by a suitability score.
getSuitableDevices :: (MonadIO m, MonadLogger m)
  => Vk.Instance
  -> GLFW.Window
  -> VkSurface.SurfaceKHR
  -> [ByteString]
  -> m [PhysicalDevice]
getSuitableDevices vkInstance window surface requiredExtensions = do
  (_, devices) <- Vk.enumeratePhysicalDevices vkInstance
  debug . printf "%d devices found." . V.length $ devices

  devices' <- flip iwither devices $ \i device -> runMaybeT $ do
    properties <- Vk.getPhysicalDeviceProperties device

    let deviceName = UTF8.toString . Vk.deviceName $ properties
    debug . printf "Checking device %d: %s." i $ deviceName

    -- Check this device for required extension support and skip it if there
    -- are any that are unsupported.
    debug "Checking device extensions."
    (result, extensions) <-
      Vk.enumerateDeviceExtensionProperties device Nothing
    when (result == Vk.INCOMPLETE) $
      warn "Vulkan API returned incomplete device extension list."
    let unsupportedExtensions = (requiredExtensions \\) . V.toList
          . fmap Vk.extensionName $ extensions
    unless (null unsupportedExtensions) $ do
      let errStr = printf "Missing required extensions: %s." . intercalate ", "
            . fmap UTF8.toString $ unsupportedExtensions
      debug errStr
      fail errStr
    debug "Found required extensions."

    -- Find the first queue family that supports everything we need, skipping
    -- the device if we can't find one.
    debug "Finding compatible device queue family."
    queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties device
    queueFamilyIndex <-
      findSuitableQueueFamilyIndex surface device queueFamilyProperties
    debug . printf "Chosen queue family %d." $ queueFamilyIndex

    -- Get the surface capabilities, formats and present modes.
    swapChainSupport <- getSwapChainSupport device window surface

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
-- Notes:
--
--   Surface format we require just one true colour, SRGB format:
--   {format=FORMAT_B8G8R8A8_SRGB, colorSpace=COLOR_SPACE_SRGB_NONLINEAR_KHR}
--   which is 99.9% supported on Windows.
--
--   Present mode: PRESENT_MODE_IMMEDIATE_KHR is our choice for development
--   but we could fall back to PRESENT_MODE_FIFO_KHR (100% Windows support) if
--   its not available.
--
getSwapChainSupport :: (MonadIO m, MonadLogger m)
  => Vk.PhysicalDevice
  -> GLFW.Window
  -> VkSurface.SurfaceKHR
  -> MaybeT m SwapChainSettings
getSwapChainSupport device window surface = do
  -- Note that we've already checked if there is a queue family that supports
  -- the VK_KHR_surface extension on this device as required by
  -- `vkGetPhysicalDeviceSurfaceCapabilitiesKHR`.
  debug "Getting device surface capabilities."
  surfaceCapabilities <- VkSurface.getPhysicalDeviceSurfaceCapabilitiesKHR
                           device
                           surface
  trace . show $ surfaceCapabilities

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
        (frameBufferWidth, frameBufferHeight)
          <- liftIO $ GLFW.getFramebufferSize window
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
  trace . show $ surfaceFormats
  let surfaceFormat = VkSurface.SurfaceFormatKHR {
                          VkSurface.colorSpace =
                            VkSurface.COLOR_SPACE_SRGB_NONLINEAR_KHR,
                          VkSurface.format = Vk.FORMAT_B8G8R8A8_SRGB
                        }
      hasSurfaceFormat = elem surfaceFormat . V.toList $ surfaceFormats
  unless hasSurfaceFormat $ do
    let errStr = printf "Missing required surface format %s" . show
                   $ surfaceFormat
    debug errStr
    fail errStr
  debug . printf "Chosen surface format %s." . show $ surfaceFormat

  -- Make sure we have the present mode we want.
  debug "Checking deivce present modes."
  (res1, presentModes) <- VkSurface.getPhysicalDeviceSurfacePresentModesKHR
                            device
                            surface
  when (res1 == Vk.INCOMPLETE) $
    warn "Vulkan API returned incomplete present modes list."
  trace . show $ presentModes
  let presentMode = VkSurface.PRESENT_MODE_IMMEDIATE_KHR
      hasPresentMode = presentMode `elem` presentModes
  unless hasPresentMode $ do
    let errStr = "Missing required present mode PRESENT_MODE_IMMEDIATE_KHR."
    debug errStr
    fail errStr
  debug . printf "Chosen present mode %s." . show $ presentMode

  return $ SwapChainSettings {
      swapSettingsExtent = Extent2D extentWidth extentHeight,
      swapSettingsImageCount = imageCount,
      swapSettingsPresentMode = presentMode,
      swapSettingsSurfaceFormat = surfaceFormat,
      swapSettingsTransform = transform
    }

-- Checks a device has at least one queue family that can do everything we want
-- and pick it. This is pretty much always going to be the case.
findSuitableQueueFamilyIndex :: forall m. (MonadIO m, MonadLogger m)
  => VkSurface.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vector Vk.QueueFamilyProperties
  -> MaybeT m Word32
findSuitableQueueFamilyIndex surface device queueFamilyProperties = do
  queueFamilies <- filterA (uncurry isSuitable) . zip [0..] . V.toList
                     $ queueFamilyProperties
  when (null queueFamilies) $ do
    let errStr = "No compatible queue family."
    debug errStr
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

  supportsSurface i = VkSurface.getPhysicalDeviceSurfaceSupportKHR device i
                        surface
