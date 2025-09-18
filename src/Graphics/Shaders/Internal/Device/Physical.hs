module Graphics.Shaders.Internal.Device.Physical (
  PhysicalDevice(..),
  SwapchainSettings(..),

  requiredDeviceExtensions,

  deviceColorImageFormat,
  deviceDepthImageFormat,

  getSuitableDevices
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString.UTF8 as UTF8
import Data.Function
import Data.List ((\\), intercalate, sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import ListT (ListT)
import qualified ListT
import Text.Printf
import qualified Vulkan.Core10.DeviceInitialization as VkDevice
import qualified Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 as VkDevice
import qualified Vulkan.Core10.Enums as Vk
import qualified Vulkan.Core10.ExtensionDiscovery as VkExtension
import Vulkan.Core10.FundamentalTypes (Extent2D(Extent2D))
import qualified Vulkan.Core10.FundamentalTypes as VkExtent2D (Extent2D(..))
import qualified Vulkan.Core10.Handles as Vk
import Vulkan.CStruct.Extends
import Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state as VkState
import qualified Vulkan.Extensions.VK_KHR_surface as VkSurface
import Vulkan.Extensions.VK_KHR_swapchain as VkSwapchain
import Witherable as W

import Control.Monad.Trans.Maybe.Extra
import Data.Bits.Extra
import Graphics.Shaders.Internal.Window
import Graphics.Shaders.Logger.Class


requiredDeviceExtensions :: [ByteString]
requiredDeviceExtensions = [
    VkSwapchain.KHR_SWAPCHAIN_EXTENSION_NAME,
    VkState.EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME
  ]

deviceColorImageFormat :: Vk.Format
deviceColorImageFormat = Vk.FORMAT_B8G8R8A8_SRGB

deviceDepthImageFormat :: Vk.Format
deviceDepthImageFormat = Vk.FORMAT_D32_SFLOAT

-- A graphics enabled physical device, such as a GPU or CPU.
data PhysicalDevice = PhysicalDevice {
    -- Queue family index and queue index
    physicalDeviceGraphicsQueue :: (Word32, Word32),
    physicalDeviceHandle :: Vk.PhysicalDevice,
    physicalDeviceMemoryProperties :: VkDevice.PhysicalDeviceMemoryProperties,
    physicalDeviceProperties :: VkDevice.PhysicalDeviceProperties,
    -- Queue family queue counts indexed by queue family device index.
    physicalDeviceQueueFamilies :: Map Word32 Int,
    physicalDeviceSwapchainSettings :: SwapchainSettings,
    physicalDeviceTransferQueue :: (Word32, Word32)
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
  -> m [PhysicalDevice]
getSuitableDevices vkInstance surface = do
  (_, devices) <- VkDevice.enumeratePhysicalDevices vkInstance
  debug . printf "%d devices found." . V.length $ devices

  -- Check each device for required extensions and features, skipping it if
  -- there are any unsupported or missing, and then return the device's
  -- properties.
  devices' <- wither (getPhysicalDeviceProperties surface) devices

  -- Finally sort all the device by a suitability score and pick the best.
  return . sortBy (compare `on` Down . scoreSuitability) . V.toList
    $ devices'

getPhysicalDeviceProperties :: (MonadIO m, MonadLogger m, HasWindow m)
  => VkSurface.SurfaceKHR
  -> Vk.PhysicalDevice
  -> m (Maybe PhysicalDevice)
getPhysicalDeviceProperties surface device = runMaybeT $ do
  properties <- VkDevice.getPhysicalDeviceProperties device
  features <- VkDevice.getPhysicalDeviceFeatures2 device

  let deviceName = UTF8.toString . VkDevice.deviceName $ properties
  debug . printf "Checking device %s." $ deviceName

  debug "Checking device extensions."
  (result, extensions) <-
    VkExtension.enumerateDeviceExtensionProperties device Nothing
  when (result == Vk.INCOMPLETE) $
    warn "Vulkan API returned incomplete device extension list."
  let unsupportedExtensions = (requiredDeviceExtensions \\) . V.toList
        . fmap VkExtension.extensionName $ extensions
  unless (null unsupportedExtensions) $ do
    let errStr = printf "Missing required extensions: %s." . intercalate ", "
          . fmap UTF8.toString $ unsupportedExtensions
    err errStr
    fail errStr
  debug "Found required extensions."

  debug "Checking device features."
  let _ ::& PhysicalDeviceVertexInputDynamicStateFeaturesEXT vertexInputDynamicState
        :& ()
        = features
  unless vertexInputDynamicState $ do
    let errStr = "Missing dynamic pipeline vertex input state."
    err errStr
    fail errStr

  debug "Finding compatible device queue families."
  queueFamilies <- getQueueFamilies device surface
  (graphicsQueueFamily, transferQueueFamily, queueFamilyQueueCounts) <- hoistMaybe
    . tryFindSuitableQueueFamilyIndices $ queueFamilies
  debug . uncurry (printf "Chosen graphics queue family %d, index %d.")
    $ graphicsQueueFamily
  debug . uncurry (printf "Chosen transfer queue family %d, index %d.")
    $ transferQueueFamily

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

  return PhysicalDevice {
      physicalDeviceGraphicsQueue = graphicsQueueFamily,
      physicalDeviceHandle = device,
      physicalDeviceMemoryProperties = memoryProperties,
      physicalDeviceProperties = properties,
      physicalDeviceQueueFamilies = queueFamilyQueueCounts,
      physicalDeviceSwapchainSettings = swapchainSupport,
      physicalDeviceTransferQueue = transferQueueFamily
    }

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


data QueueFamily = QueueFamily {
  queueFamilyQueueCount :: Word32,
  queueFamilyFlags :: VkDevice.QueueFlags,
  queueFamilySurfaceSupport :: Bool
}

getQueueFamilies
  :: forall m. MonadIO m
  => Vk.PhysicalDevice
  -> VkSurface.SurfaceKHR
  -> m (Vector QueueFamily)
getQueueFamilies device surface = do
  queueFamilyPropertiess <-
    VkDevice.getPhysicalDeviceQueueFamilyProperties device
  let queueFamilyFlagss = fmap VkDevice.queueFlags queueFamilyPropertiess
      queueFamilyQueueCounts = fmap VkDevice.queueCount queueFamilyPropertiess
  queueFamilySurfaceSupports <-
    V.iforM queueFamilyPropertiess (const . checkSurfaceSupport. fromIntegral)
  return . V.zipWith3 QueueFamily queueFamilyQueueCounts queueFamilyFlagss
    $ queueFamilySurfaceSupports
 where
  checkSurfaceSupport :: Word32 -> m Bool
  checkSurfaceSupport i =
    VkSurface.getPhysicalDeviceSurfaceSupportKHR device i surface

-- Try to find the queue family/families that will support the queues we
-- require.
tryFindSuitableQueueFamilyIndices
  :: Vector QueueFamily
  -> Maybe ((Word32, Word32), (Word32, Word32), Map Word32 Int)
tryFindSuitableQueueFamilyIndices queueFamilies = do
  let (mQueues, (_, queueFamilyQueueCounts)) =
         flip runState (queueFamilies, M.empty) . ListT.head $ do
           graphicsQueue <- findSuitableQueueFamily graphicsQueueFlags True
           transferQueue <- findSuitableQueueFamily transferQueueFlags False
           return (graphicsQueue, transferQueue)
  (graphicsQueue, transferQueue) <- mQueues
  return (graphicsQueue, transferQueue, fmap (+1) queueFamilyQueueCounts)
 where
  findSuitableQueueFamily
    :: VkDevice.QueueFlags
    -> Bool
    -> ListT (State (Vector QueueFamily, Map Word32 Int)) (Word32, Word32)
  findSuitableQueueFamily requiredFlags requiresSurface = do
    (families, queues) <- get
    -- Find the index of a suitable queue family
    (fIx, QueueFamily{..}) <- ListT.fromFoldable
      . W.filter (isSuitable . snd) . V.imap (,) $ families
    guard $ queueFamilyQueueCount > 0
    -- Deplete that queue family queue count
    let queueFamily = families V.! fIx
        queueFamily' = queueFamily {
            queueFamilyQueueCount = queueFamilyQueueCount - 1
          }
    -- Find the next available queue index
    let fIx' = fromIntegral fIx
        queues' = M.alter (\case Nothing -> Just 0; Just x -> Just $ x+1) fIx'
                     queues

    put (families V.// [(fIx, queueFamily')], queues')
    return (fromIntegral fIx, fromIntegral $ queues' M.! fIx')
   where
    isSuitable :: QueueFamily -> Bool
    isSuitable QueueFamily{..} = queueFamilyFlags .?. requiredFlags
      && (not requiresSurface || queueFamilySurfaceSupport)

  graphicsQueueFlags, transferQueueFlags :: VkDevice.QueueFlags
  graphicsQueueFlags = Vk.QUEUE_GRAPHICS_BIT .|. Vk.QUEUE_TRANSFER_BIT
  transferQueueFlags = Vk.QUEUE_TRANSFER_BIT
