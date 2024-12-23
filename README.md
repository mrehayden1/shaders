# Shaders - Typesafe GPU programming.

This library aims to provide a purely functional interface for GPU programming using the the Vulkan API.

This library is heavily influenced by [GPipe](https://hackage.haskell.org/package/GPipe) and much credit is owed to its author, Tobias Bexelius for its inspiration.

Some attempt at a denotation of GPU programming is attempted but this is not meant to be satisfactory to all interested in such topics.

# Design goals

The overall goal is to support rendering pipeline features which are ubiquitous in rendering modern 3D scenes. The following list is by no means complete and acts as a starting point for development.


- [] Meshes (vertex data)
  - [] Vertices
  - [] Indexes
  - [] Instances

- [] Rasterization
  - [] Primitive topologies
  - [] Interpolation

- [] Uniforms

- [] Texture images
  - [] Mip maps

- [] Sampling
  - [] Wrapping
  - [] Filtering

- [] Drawing
  - [] Clearing
  - [] Blending
  - [] Face culling
  - [] Depth testing

- [] Framebuffers
  - [] Surface attachments
  - [] Image attachments

- [] Shaders
  - Vertex
  - Fragment

## Interface design

### Buffers

- Vertex buffers
- Index buffers
- Instance buffers

Buffers can be combined into vertices which can be transformed by vertex shaders.

### Vertices

Vertices can be rasterized into primitives along with a primitive topology and interpolation.
