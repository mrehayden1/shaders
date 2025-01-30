module Graphics.Shaders.Pipeline (
  Pipeline,
  CompiledPipeline,

  FragmentStream,

  compilePipeline,
  runPipeline,

  toPrimitiveStream,
  rasterize
) where

import Graphics.Shaders.Internal.Pipeline
