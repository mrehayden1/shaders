module Graphics.Shaders.Texture.Loader.TGA (
  TGA(..),
  decodeFile
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Data.Binary (Binary(..))
import qualified Data.Binary as B
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable.Mutable as V
import Text.Printf


data TGA = TGA {
  tgaData :: BS.ByteString,
  tgaHeight :: Int,
  tgaWidth :: Int
} deriving (Show)

instance Binary TGA where
  get = do
    idFieldLength <- fromIntegral <$> getWord8
    _ <- getWord8 -- colour map flag
    imageType <- getWord8
    let imageColor = imageType .&. 0b111 -- bits 0-2
        isCompressed = testBit imageType 3
    -- TODO Decompression
    when isCompressed $ fail "Unsupported compression image."
    (_, colorMapLength, _) <-
      (,,) <$> getWord16le <*> fmap fromIntegral getWord16le <*> getWord8
    -- x and y origin relative to bottom left
    _ <- getWord16le
    _ <- getWord16le
    imageWidth <- fromIntegral <$> getWord16le
    imageHeight <- fromIntegral <$> getWord16le
    pixelDepth <- getWord8
    imageDescriptor <- getWord8
    let alphaDepth = imageDescriptor .&. 0b1111 -- bits 0-3
        isSupportedPixelDepth = alphaDepth == 8 && pixelDepth == 32
          || alphaDepth == 0 && pixelDepth == 24
    -- We only support one byte per channel for simplicity
    unless isSupportedPixelDepth $
      fail $ "Unsupported pixel depth. Use 24 bit true-color with an optional"
               <> " 8-bit alpha."
    let numChannels = if alphaDepth == 0 then 3 else 4
        leftToRight = not $ testBit imageDescriptor 4
        topToBottom = testBit imageDescriptor 5
    -- Skip the image identifier
    skip idFieldLength
    -- Skip the color map
    skip colorMapLength
    -- Parse the image data
    let numImageBytes = imageWidth * imageHeight * numChannels
    pixels <- case imageColor of
      -- No image data
      0 -> return mempty
      -- True-color image
      2 -> do
        imageBytes <- getByteString numImageBytes
        -- We can return the raw image when the pixel data is in the right
        -- order already for the current format, otherwise we have some work to
        -- do.
        if topToBottom && leftToRight && alphaDepth == 0
        then return imageBytes
        else return $ runST $ do
          vector <- V.new $ imageWidth * imageHeight * 4
          forM_ [0..(imageHeight - 1)] $ \y -> do
            let srcRowOffset = y * imageWidth * numChannels
                dstRowOffset = imageWidth * 4 * if topToBottom
                  then y
                  else imageHeight - 1 - y
            forM_ [0..(imageWidth  - 1)] $ \x -> do
              let srcOffset = srcRowOffset + x * numChannels
                  dstOffset = dstRowOffset + 4 * if leftToRight
                    then x
                    else imageWidth - 1 - x
                  b = imageBytes `BS.index` srcOffset
                  g = imageBytes `BS.index` (srcOffset + 1)
                  r = imageBytes `BS.index` (srcOffset + 2)
                  a = if alphaDepth > 0
                      then imageBytes `BS.index` (srcOffset + 3)
                      else maxBound
              V.write vector dstOffset       r
              V.write vector (dstOffset + 1) g
              V.write vector (dstOffset + 2) b
              V.write vector (dstOffset + 3) a
          return . uncurry BS.BS $ V.unsafeToForeignPtr0 vector
        -- Read lines
      -- TODO Color-mapped image
      1 -> fail "Unsuported color-mapped image."
      -- TODO Grayscale image
      3 -> fail "Unsuported grayscale image."
      _ -> fail . printf "Unknown image type value: %d" $ imageType
    return . TGA pixels imageHeight $ imageWidth
  put = undefined

-- Decodes TGA image data into a 24-bit trucolor ByteString
decodeFile :: MonadIO m => FilePath -> m TGA
decodeFile = fmap B.decode . liftIO . LBS.readFile
