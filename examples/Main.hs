module Main (
  main
) where

import Control.Monad.Reader
import Data.Functor
import Data.IORef
import Data.Time.Clock
import Data.Word
import Linear
import Text.Printf

import Graphics.Shaders
import Graphics.Shaders.Buffer
import Graphics.Shaders.Logger
import Graphics.Shaders.Pipeline
import Graphics.Shaders.PushConstant
import Graphics.Shaders.Render
import Graphics.Shaders.Sampler
import Graphics.Shaders.Texture
import Graphics.Shaders.Texture.Loader.TGA
import Graphics.Shaders.Transfer (runTransferT, writeTexture)
import Graphics.Shaders.Uniform
import Graphics.Shaders.Window

appName :: String
appName = "Shaders Example"

appVersion :: String
appVersion = "0.1.0"

appLoggingLevel :: LogLevel
appLoggingLevel = LogTrace
--appLoggingLevel = LogDebug
--appLoggingLevel = LogInfo
--appLoggingLevel = LogWarn
--appLoggingLevel = LogError
--appLoggingLevel = LogNone

type Vertex = (B4 Float, B2 Float, V4 (B4 Float))

data Input = Input {
  inputMatrix :: Buffer (Uniform (V4 (B4 Float))),
  inputTexture :: (Texture, TextureSampler),
  inputVertices :: PrimitiveArray Triangles Vertex
}

main :: IO ()
main = do
  printf "\nStarting %s v%s...\n\n" appName appVersion
  putStrLn "Copyright 2025 Studio Bagat.\n"
  putStrLn "Initialising graphics..."

  runLoggerT appLoggingLevel . runGlfwWindowReaderT appName . runShadersT
    $ app

  putStrLn "Exited."

app :: MonadShaders m => ShadersT m ()
app = do
  let quadVertices = [
          (V4 (-1) (-1) 0 1, V2 0.05 0.05),
          (V4 (-1)   1  0 1, V2 0.05 0.95),
          (V4   1  (-1) 0 1, V2 0.95 0.05),
          (V4   1    1  0 1, V2 0.95 0.95)
        ]
  quadBuffer <- createBuffer (length quadVertices)
  let quadVertexArray = toVertexArray quadBuffer

  let quadIndices = [ 0, 1, 2, 3, 2, 1 ] :: [Word16]
  quadIndexBuffer :: Buffer (BIndex Word16) <- createBuffer (length quadIndices)
  let quadIndexArray = toIndexArray quadIndexBuffer

  let n = 10 :: Int
  let quadInstances = [0..(n^2 - 1)] <&> \x ->
        let n' = fromIntegral n
            tx = fromIntegral (x `mod` n) * (2 / n') - 1 + 1 / n'
            ty = fromIntegral (x `div` n) * (2 / n') - 1 + 1 / n'
            tz = 0
            s  = 0.75 / fromIntegral n
        in V4 (V4 s 0 0 tx) (V4 0 s 0 ty) (V4 0 0 s tz) (V4 0 0 0 1)
  quadInstanceBuffer <- createBuffer (length quadInstances)
  let quadInstanceVertexArray = toVertexArray quadInstanceBuffer

  {-
  let quadPrimitives = toPrimitiveArrayInstanced TriangleStrip quadVertexArray
          quadInstanceVertexArray
        $ \(pos, uv) tm -> (pos, uv, tm)
  -}

  let quadPrimitives = toPrimitiveArrayIndexedInstanced TriangleList
          quadIndexArray quadVertexArray quadInstanceVertexArray
        $ \(pos, uv) tm -> (pos, uv, tm)

  matrixBuffer <- createBuffer 1

  TGA{..} <- decodeFile "examples/texture-alpha.tga"
  texture <- createTexture tgaWidth tgaHeight

  textureSampler <- createTextureSampler TextureFilterLinear TextureFilterLinear
    TextureMipmapLinear TextureWrapRepeat TextureWrapRepeat

  runTransferT $ do
    writeTexture texture tgaData tgaWidth tgaHeight

  let input = Input {
      inputMatrix = matrixBuffer,
      inputTexture = (texture, textureSampler),
      inputVertices = quadPrimitives
    }

  pipeline <- compilePipeline $ do
    vertices <- toPrimitiveStream inputVertices

    matrix :: M44 (S V Float) <- getPushConstant

    sampler <- getSampler inputTexture

    let vertices' = vertices <&> \(p, uv, tm) ->
          let glPos = tm !*! matrix !* p
          in (glPos, uv)
    fragments <- rasterize vertices'
    return $ fragments <&> \uv ->
      sample sampler uv

  liftIO $ putStrLn "Finished startup."
  liftIO $ putStrLn "Running..."

  startTime <- liftIO getCurrentTime

  frameStartTimeRef <- liftIO $ newIORef startTime

  -- Initialise buffers
  renderFrame $ do
    writeBuffer quadBuffer quadVertices
    writeBuffer quadIndexBuffer quadIndices
    writeBuffer quadInstanceBuffer quadInstances

  fix $ \loop -> do
    frameStartTime <- liftIO $ readIORef frameStartTimeRef
    let simTime = realToFrac . diffUTCTime frameStartTime
                    $ startTime :: Float

    let s = min 1 $ simTime * 0.1
        matrixData =
          V4
            (V4 s   0   0   0  )
            (V4 0   s   0   0  )
            (V4 0   0   1   0  )
            (V4 0   0   0   1  )

    renderFrame $ do
      --writeBuffer matrixBuffer [ matrixData ]
      clearWindow
      drawWindow matrixData input pipeline

    logTrace "Polling window events"
    windowEvents <- pollWindowEvents
    liftIO $ print windowEvents

    logTrace "Checking if window should close"
    close <- windowShouldClose

    frameEndTime <- liftIO getCurrentTime
    liftIO $ writeIORef frameStartTimeRef frameEndTime

    let frameTime = realToFrac . diffUTCTime frameEndTime $ frameStartTime :: Float
    logTrace . printf "Frame time: %.5fms" . (* 1000) $ frameTime
    logTrace . printf "FPS: %.5f" . (1/) $ frameTime

    unless close loop

  liftIO $ putStrLn "Exiting..."
  debug "Awaiting graphics idle."
  awaitDeviceIdle
