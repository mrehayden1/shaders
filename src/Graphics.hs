module Graphics (
  Graphics,
  initialise,
  cleanup
) where

import Data.ByteString
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

newtype Graphics = Graphics Vk.Instance

initialise :: IO Graphics
initialise = do
  putStrLn "Initialising graphics."
  -- Create Vulkan instance
  let appInfo = Vk.ApplicationInfo Nothing 0 Nothing 0 Vk.API_VERSION_1_3
  extensions <- fmap V.fromList . mapM packCString
                  =<< GLFW.getRequiredInstanceExtensions
  let instanceInfo = Vk.InstanceCreateInfo () Vk.zero (Just appInfo) V.empty
                       extensions
  vkInstance <- Vk.createInstance instanceInfo Nothing
  -- Create logical device
  (_, devices) <- Vk.enumeratePhysicalDevices vkInstance

  return . Graphics $ vkInstance

cleanup :: Graphics -> IO ()
cleanup (Graphics vkInstance) = do
  putStrLn "Cleaning up graphics."
  Vk.destroyInstance vkInstance Nothing
