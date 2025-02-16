module Main (
  main
) where

import Control.Monad.Codensity
import Control.Monad.Reader
import Data.Functor
import Data.IORef
import Data.Time.Clock
import Text.Printf

import Data.Linear
import Graphics.Shaders
import Graphics.Shaders.Sampler
import Graphics.Shaders.Texture
import Graphics.Shaders.Uniform
import Window


appName :: String
appName = "Bagatelle"

appVersion :: String
appVersion = "0.1.0"

appLoggingLevel :: LogLevel
appLoggingLevel = LogTrace

data ShaderEnv = ShaderEnv {
  envVertices :: PrimitiveArray
    Triangles
    (B (V2 Float), B (V3 Float), B (V2 Float))
    (B (V2 Float), B Float),
  envColor :: Buffer (Uniform (B (V3 Float))),
  envMatrix :: Buffer (Uniform (B (M44 Float))),
  envOpacity :: Buffer (Uniform (B Float)),
  envTexture :: Texture
}

main :: IO ()
main = do
  liftIO $ printf "\nStarting %s v%s...\n\n" appName appVersion
  liftIO $ putStrLn "Copyright 2024 Categorical Industries.\n"

  runLoggerT appLoggingLevel . flip runCodensity return $ do
    liftIO $ putStrLn "Initialising graphics..."

    window <- withWindow appName

    runShadersT window $ do
      timeRef <- liftIO $ newIORef =<< getCurrentTime

      let triangleVertices = [
              (V2   0.0  (-0.5), V3 1.0 0.0 0.0),
              (V2 (-0.5)   0.5 , V3 0.0 0.0 1.0),
              (V2   0.5    0.5 , V3 0.0 1.0 0.0)
            ]

      triangleBuffer :: Buffer (B (V2 Float), B (V3 Float)) <-
        withBuffer triangleVertices
      let triangleVertexArray = toVertexArray triangleBuffer
          trianglePrimitives = toPrimitiveArray TriangleList triangleVertexArray

      let quadVertices = [
              (V2 (-1) (-1), V3 1.0 0.0 0.0, V2 0 0),
              (V2 (-1)   1 , V3 1.0 1.0 1.0, V2 0 1),
              (V2   1  (-1), V3 0.0 0.0 1.0, V2 1 0),
              (V2   1    1 , V3 0.0 1.0 0.0, V2 1 1)
            ]
      quadVertexArray <- toVertexArray <$> withBuffer quadVertices

      let n = 2 :: Int
      let quadInstances = [0..(n^2 - 1)] <&> \x ->
            let n' = fromIntegral n
            in (
              V2 (fromIntegral (x `mod` n) * (2 / n') - 1 + 1 / n')
                 (fromIntegral (x `div` n) * (2 / n') - 1 + 1 / n'),
              0.75 / fromIntegral n
            )
      quadInstanceVertexArray <-
        toVertexArray <$> withBuffer quadInstances

      {-
      let quadIndices = [ 0, 1, 2, 1, 3, 2 ] :: [Word32]
      quadIndexBuffer :: Buffer (B Word32) <- withBuffer quadIndices
      let quadIndexArray = toIndexArray quadIndexBuffer

          quadPrimitives = toPrimitiveArrayIndexedInstanced TriangleList
            quadIndexArray quadVertexArray quadInstanceVertexArray
      -}
      let quadPrimitives = toPrimitiveArrayInstanced TriangleStrip
            quadVertexArray quadInstanceVertexArray

      let matrixData = [
              M44
                (V4 1   0   0   0  )
                (V4 0   1   0   0  )
                (V4 0   0   1   0  )
                (V4 0.5 0.5 0   1  )
            ]

          redData = [
              V3 1 0 0
            ]

          opacityData = [ 1 ]

      shaderEnv <- ShaderEnv quadPrimitives
        <$> withBuffer redData
        <*> withBuffer matrixData
        <*> withBuffer opacityData
        <*> loadTexture "examples/texture-alpha.tga"

      pipeline <- compilePipeline $ do
        vertices <- toPrimitiveStream envVertices $ \(pos, color, uv) (t, s) ->
          (pos, color, uv, t, s)

        red :: S V (V3 Float) <- getUniform envColor
        matrix :: S V (M44 Float) <- getUniform envMatrix
        opacity :: S F Float <- getUniform envOpacity

        sampler <- getSampler envTexture

        let vertices' = vertices <&> \(p, baseColor, uv, t, s) ->
              let s' = s * 1.1
                  glPos = vec4 (_x p * s' + _x t) (_y p * s' + _y t) 0 1
              in (glPos, uv)
        fragments <- rasterize vertices'
        return $ fragments <&> \uv -> texture sampler uv

      liftIO $ putStrLn "Finished startup."
      liftIO $ putStrLn "Running...\n"

      let loop = do
            awaitIdle
            runPipeline shaderEnv pipeline
            swap
            startTime <- liftIO $ readIORef timeRef
            endTime <- liftIO getCurrentTime
            liftIO $ writeIORef timeRef endTime
            let frameTime = realToFrac . diffUTCTime endTime
                              $ startTime :: Float
            logTrace . printf "Frame time: %.5fms" . (* 1000) $ frameTime
            logTrace . printf "FPS: %.5f" . (1/) $ frameTime
            liftIO pollEvents
            close <- liftIO $ windowShouldClose window
            unless close loop

      loop

      liftIO $ putStrLn "Exiting..."
      debug "Awaiting graphics idle."
      awaitIdle

  liftIO $ putStrLn "Exited."
