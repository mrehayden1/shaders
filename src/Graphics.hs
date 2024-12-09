module Graphics (
  Instance,
  initialise,
  cleanup
) where

import Data.ByteString
import qualified Data.Vector as V
import Graphics.UI.GLFW
import Vulkan
import Vulkan.Zero

initialise :: IO Instance
initialise = do
  putStrLn "Initialising graphics."
  let appInfo = ApplicationInfo Nothing 0 Nothing 0 API_VERSION_1_3
  extensions <- fmap V.fromList . mapM packCString
                  =<< getRequiredInstanceExtensions
  let instanceInfo = InstanceCreateInfo () zero (Just appInfo) V.empty
                       extensions
  createInstance instanceInfo Nothing

cleanup :: Instance -> IO ()
cleanup vkInstance = do
  putStrLn "Cleaning up graphics."
  destroyInstance vkInstance Nothing
