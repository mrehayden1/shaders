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
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core10.ImageView as VkImageView
import qualified Vulkan.Zero as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as VkSwapChain
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import Vulkan.Core10.FundamentalTypes (Extent2D(Extent2D))
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import Vulkan.CStruct.Extends
import Witherable

import Graphics.Class

-- A graphics enabled logical device.
data Device = Device {
  deviceVkDevice :: Vk.Device,
  deviceQueueHandle :: Vk.Queue,
  deviceSwapChain :: SwapChain
} deriving (Show)

-- Information about the swap chain.
data SwapChain = SwapChain {
  swapChainVkHandle :: VkSwapChain.SwapchainKHR,
  swapChainExtent :: VkExtent2D.Extent2D,
  swapChainFormat :: VkSurface.SurfaceFormatKHR,
  swapChainImages :: Vector Vk.Image,
  swapChainImageViews :: Vector VkImageView.ImageView
} deriving (Show)

requiredDeviceExtensions :: [ByteString]
requiredDeviceExtensions = [VkSwapChain.KHR_SWAPCHAIN_EXTENSION_NAME]

createDevice :: (MonadIO m, MonadLogger m)
  => Vk.Instance
  -> GLFW.Window
  -> VkSurface.SurfaceKHR
  -> m (Maybe Device)
createDevice vkInstance window surface = runMaybeT $ do
  devices <- lift $ getSuitableDevices vkInstance window surface

  PhysicalDevice{..} <- hoistMaybe . listToMaybe $ devices
  let deviceHandle = physicalDeviceHandle
      deviceName = Vk.deviceName physicalDeviceProperties
  lift . info $ "Chosen physical device: " <> UTF8.toString deviceName

  -- Create the logical device and queue.
  lift . debug $ "Creating logical device."
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
  vkDevice <- Vk.createDevice deviceHandle deviceCreateInfo Nothing
  queue <- Vk.getDeviceQueue vkDevice physicalDeviceQueueFamilyIndex 0

  -- Create swap chain
  lift . debug $ "Creating swap chain."
  let swapChainImageFormat = VkSurface.format
        . swapSettingsSurfaceFormat $ physicalDeviceSwapChainSettings

  let swapChainCreateInfo = Vk.zero {
          VkSwapChain.flags = Vk.zero,
          VkSwapChain.surface = surface,
          VkSwapChain.minImageCount =
            swapSettingsImageCount physicalDeviceSwapChainSettings,
          VkSwapChain.imageFormat = swapChainImageFormat,
          VkSwapChain.imageColorSpace = VkSurface.colorSpace
            . swapSettingsSurfaceFormat $ physicalDeviceSwapChainSettings,
          VkSwapChain.imageExtent = swapSettingsExtent
            physicalDeviceSwapChainSettings,
          VkSwapChain.imageArrayLayers = 1,
          VkSwapChain.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
          VkSwapChain.imageSharingMode = Vk.SHARING_MODE_EXCLUSIVE,
          VkSwapChain.preTransform =
            swapSettingsTransform physicalDeviceSwapChainSettings,
          VkSwapChain.compositeAlpha =
            VkSwapChain.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
          VkSwapChain.presentMode =
            swapSettingsPresentMode physicalDeviceSwapChainSettings,
          VkSwapChain.clipped = True
        }

  vkSwapChain <- liftIO $ VkSwapChain.createSwapchainKHR
                            vkDevice
                            swapChainCreateInfo
                            Nothing

  -- Create swap chain images and image views.
  lift . debug $ "Retrieving swap chain images."
  (result, swapImages) <- VkSwapChain.getSwapchainImagesKHR
                            vkDevice
                            vkSwapChain
  when (result == Vk.INCOMPLETE) $
    lift $ warn "Vulkan API returned incomplete swap chain images list."

  lift . debug $ "Creating swap chain image views."
  swapImageViews <- forM swapImages $ \image -> do
    let imageViewCreateInfo = Vk.zero {
            VkImageView.format = swapChainImageFormat,
            VkImageView.image = image,
            VkImageView.subresourceRange = Vk.zero {
                VkImageView.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
                VkImageView.layerCount = 1,
                VkImageView.levelCount = 1
              },
            VkImageView.viewType = VkImageView.IMAGE_VIEW_TYPE_2D
          }
    Vk.createImageView vkDevice imageViewCreateInfo Nothing

  let swapChain = SwapChain {
          swapChainVkHandle = vkSwapChain,
          swapChainExtent = swapSettingsExtent physicalDeviceSwapChainSettings,
          swapChainFormat =
            swapSettingsSurfaceFormat physicalDeviceSwapChainSettings,
          swapChainImages = swapImages,
          swapChainImageViews = swapImageViews
        }

  return . Device vkDevice queue $ swapChain

destroySwapChain :: (MonadIO m, MonadLogger m)
  => Vk.Device
  -> SwapChain
  -> m ()
destroySwapChain vkDevice SwapChain{..} = do
  debug "Destroying swap chain image views."
  forM_ swapChainImageViews $ \i ->
    VkImageView.destroyImageView vkDevice i Nothing
  debug "Destroying swap chain."
  VkSwapChain.destroySwapchainKHR
    vkDevice
    swapChainVkHandle
    Nothing

destroyDevice :: (MonadIO m, MonadLogger m) => Device -> m ()
destroyDevice Device{..} = do
  destroySwapChain deviceVkDevice deviceSwapChain
  debug "Destroying logical device."
  Vk.destroyDevice deviceVkDevice Nothing

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
  -> m [PhysicalDevice]
getSuitableDevices vkInstance window surface = do
  (_, devices) <- Vk.enumeratePhysicalDevices vkInstance
  debug . printf "%d devices found." . V.length $ devices

  devices' <- flip iwither devices $ \i device -> runMaybeT $ do
    properties <- Vk.getPhysicalDeviceProperties device

    let deviceName = UTF8.toString . Vk.deviceName $ properties
    lift . debug . printf "Checking device %d: %s." i $ deviceName

    -- Check this device for required extension support and skip it if there
    -- are any that are unsupported.
    lift . debug $ "Checking device extensions."
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
    lift . debug $ "Found required extensions."

    -- Find the first queue family that supports everything we need, skipping
    -- the device if we can't find one.
    lift . debug $ "Finding compatible device queue family."
    queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties device
    queueFamilyIndex <-
      findSuitableQueueFamilyIndex surface device queueFamilyProperties
    lift . debug . printf "Chosen queue family %d." $ queueFamilyIndex

    -- Get the surface capabilities, formats and present modes.
    swapChainSupport <- getDeviceSwapChainSupport device window surface

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
getDeviceSwapChainSupport :: (MonadIO m, MonadLogger m)
  => Vk.PhysicalDevice
  -> GLFW.Window
  -> VkSurface.SurfaceKHR
  -> MaybeT m SwapChainSettings
getDeviceSwapChainSupport device window surface = do
  -- Note that we've already checked if there is a queue family that supports
  -- the VK_KHR_surface extension on this device as required by
  -- `vkGetPhysicalDeviceSurfaceCapabilitiesKHR`.
  lift . debug $ "Getting device surface capabilities."
  surfaceCapabilities <- VkSurface.getPhysicalDeviceSurfaceCapabilitiesKHR
                           device
                           surface
  lift . trace . show $ surfaceCapabilities

  -- Calculate the swap image extent.
  lift . debug $ "Calculating swap image extent."
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
  lift . debug . printf "Got extent %dpx × %dpx." extentWidth $ extentHeight

  -- Calculate image count
  lift . debug $ "Calculating swap image count."
  let minImageCount = VkSurface.minImageCount surfaceCapabilities
      maxImageCount = if VkSurface.maxImageCount surfaceCapabilities == 0
                        then maxBound
                        else VkSurface.maxImageCount surfaceCapabilities
      imageCount = min maxImageCount $ minImageCount + 1
  lift . debug . printf "Using %d swap images." $ imageCount

  let transform = VkSurface.currentTransform surfaceCapabilities
  lift . debug . printf "Using transform %s." . show $ transform

  -- Make sure we have the surface format we're looking for, abort if not.
  lift . debug $ "Checking device surface formats."
  (res0, surfaceFormats) <- VkSurface.getPhysicalDeviceSurfaceFormatsKHR
                              device
                              surface
  when (res0 == Vk.INCOMPLETE) $
    lift $ warn "Vulkan API returned incomplete surface formats list."
  lift . trace . show $ surfaceFormats
  let surfaceFormat = VkSurface.SurfaceFormatKHR {
                          VkSurface.colorSpace =
                            VkSurface.COLOR_SPACE_SRGB_NONLINEAR_KHR,
                          VkSurface.format = Vk.FORMAT_B8G8R8A8_SRGB
                        }
      hasSurfaceFormat = elem surfaceFormat . V.toList $ surfaceFormats
  unless hasSurfaceFormat $ do
    let errStr = printf "Missing required surface format %s" . show
                   $ surfaceFormat
    lift . debug $ errStr
    fail errStr
  lift . debug . printf "Chosen surface format %s." . show $ surfaceFormat

  -- Make sure we have the present mode we want.
  lift . debug $ "Checking deivce present modes."
  (res1, presentModes) <- VkSurface.getPhysicalDeviceSurfacePresentModesKHR
                            device
                            surface
  when (res1 == Vk.INCOMPLETE) $
    lift $ warn "Vulkan API returned incomplete present modes list."
  lift . trace . show $ presentModes
  let presentMode = VkSurface.PRESENT_MODE_IMMEDIATE_KHR
      hasPresentMode = presentMode `elem` presentModes
  unless hasPresentMode $ do
    let errStr = "Missing required present mode PRESENT_MODE_IMMEDIATE_KHR."
    lift . debug $ errStr
    fail errStr
  lift . debug . printf "Chosen present mode %s." . show $ presentMode

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

  supportsSurface i = VkSurface.getPhysicalDeviceSurfaceSupportKHR device i
                        surface

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
