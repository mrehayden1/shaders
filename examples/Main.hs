module Main (
  main
) where

import Control.Monad.Reader
import Data.Functor
import Data.IORef
import Data.Time.Clock
import Linear
import Text.Printf

import Graphics.Shaders
import Graphics.Shaders.Sampler
import Graphics.Shaders.Texture
import Graphics.Shaders.Uniform
import Window


appName :: String
appName = "Shaders Example"

appVersion :: String
appVersion = "0.1.0"

appLoggingLevel :: LogLevel
appLoggingLevel = LogTrace
--appLoggingLevel = LogDebug
--appLoggingLevel = LogInfo

type Vertex = (B4 Float, B4 Float, B2 Float)
type Instance =  V4 (B4 Float)

data ShaderEnv = ShaderEnv {
  envColor :: Buffer 'ReadOnly (Uniform (B4 Float)),
  envMatrix :: Buffer 'ReadWrite (Uniform (V4 (B4 Float))),
  envOpacity :: Buffer 'ReadOnly (Uniform (B Float)),
  envTexture :: Texture,
  envVertices :: PrimitiveArray Triangles Vertex Instance
}

main :: IO ()
main = do
  printf "\nStarting %s v%s...\n\n" appName appVersion
  putStrLn "Copyright 2024 Categorical Industries.\n"
  putStrLn "Initialising graphics..."

  window <- createWindow appName

  runLoggerT appLoggingLevel . runShadersT window $ do
    let triangleVertices = [
            (V2   0.0  (-0.5), V4 1.0 0.0 0.0 1),
            (V2 (-0.5)   0.5 , V4 0.0 0.0 1.0 1),
            (V2   0.5    0.5 , V4 0.0 1.0 0.0 1)
          ]

    triangleBuffer :: Buffer 'ReadOnly (V2 (B Float), V4 (B Float)) <-
      createBufferReadOnly triangleVertices
    let triangleVertexArray = toVertexArray triangleBuffer
        trianglePrimitives = toPrimitiveArray TriangleList triangleVertexArray

    let quadVertices = [
            (V4 (-1) (-1) 0 1, V4 1.0 0.0 0.0 1, V2 0 0),
            (V4 (-1)   1  0 1, V4 1.0 1.0 1.0 1, V2 0 1),
            (V4   1  (-1) 0 1, V4 0.0 0.0 1.0 1, V2 1 0),
            (V4   1    1  0 1, V4 0.0 1.0 0.0 1, V2 1 1)
          ]
    quadVertexArray <- toVertexArray <$> createBufferReadOnly quadVertices

    let n = 10 :: Int
    let quadInstances = [0..(n^2 - 1)] <&> \x ->
          let n' = fromIntegral n
              tx = fromIntegral (x `mod` n) * (2 / n') - 1 + 1 / n'
              ty = fromIntegral (x `div` n) * (2 / n') - 1 + 1 / n'
              tz = 0
              s  = 0.75 / fromIntegral n
          in V4 (V4 s 0 0 tx) (V4 0 s 0 ty) (V4 0 0 s tz) (V4 0 0 0 1)
    quadInstanceVertexArray <-
      toVertexArray <$> createBufferReadOnly quadInstances

    {-
    let quadIndices = [ 0, 1, 2, 1, 3, 2 ] :: [Word32]
    quadIndexBuffer :: Buffer (B Word32) <- createBuffer quadIndices
    let quadIndexArray = toIndexArray quadIndexBuffer

        quadPrimitives = toPrimitiveArrayIndexedInstanced TriangleList
          quadIndexArray quadVertexArray quadInstanceVertexArray
    -}
    let quadPrimitives = toPrimitiveArrayInstanced TriangleStrip
          quadVertexArray quadInstanceVertexArray

    let baseColorData = [
            V4 1 1 1 1
          ]

        opacityData = [ 1 ]

    matrixBuffer <- createBuffer 1

    shaderEnv <- ShaderEnv
      <$> createBufferReadOnly baseColorData
      <*> pure matrixBuffer
      <*> createBufferReadOnly opacityData
      <*> loadTexture "examples/texture-alpha.tga"
      <*> pure quadPrimitives

    pipeline <- compilePipeline $ do
      vertices <- toPrimitiveStream envVertices $ \(pos, color, uv) tm ->
        (pos, color, uv, tm)

      red :: V4 (S V Float) <- getUniform envColor
      matrix :: M44 (S V Float) <- getUniform envMatrix
      opacity :: S F Float <- getUniform envOpacity

      sampler <- getSampler envTexture

      let vertices' = vertices <&> \(p, baseColor, uv, tm) ->
            let glPos = tm !*! matrix !* p
            in (glPos, (uv, baseColor))
      fragments <- rasterize vertices'
      return $ fragments <&> \(uv, baseColor) ->
        sample sampler uv * baseColor

    liftIO $ putStrLn "Finished startup."
    liftIO $ putStrLn "Running..."

    startTime <- liftIO getCurrentTime

    frameStartTimeRef <- liftIO $ newIORef startTime

    fix $ \loop -> do
      frameStartTime <- liftIO $ readIORef frameStartTimeRef
      let simTime = realToFrac . diffUTCTime frameStartTime
                      $ startTime :: Float

      let sX = min 1 $ simTime * 0.1
          matrixData = [
              V4
                (V4 sX  0   0   0  )
                (V4 0   1   0   0  )
                (V4 0   0   1   0  )
                (V4 0   0   0   1  )
            ]
      writeBuffer matrixBuffer matrixData

      runPipeline shaderEnv pipeline
      swap

      frameEndTime <- liftIO getCurrentTime
      liftIO $ writeIORef frameStartTimeRef frameEndTime

      let frameTime = realToFrac . diffUTCTime frameEndTime $ frameStartTime :: Float
      logTrace . printf "Frame time: %.5fms" . (* 1000) $ frameTime
      logTrace . printf "FPS: %.5f" . (1/) $ frameTime

      logTrace "Polling events"
      liftIO pollEvents

      logTrace "Checking if window should close"
      close <- liftIO $ windowShouldClose window

      unless close loop

    liftIO $ putStrLn "Exiting..."
    debug "Awaiting graphics idle."
    awaitIdle

  destroyWindow window

  putStrLn "Exited."
