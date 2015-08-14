## SmallGL ##

A small, simple F# graphics library written on top of OpenGL, using the OpenTK bindings. This repository also contains a simple implementation of two-player Pong, which uses SmallGL.

### Goals ###

This library focused on simplifying OpenGL to a core of modern features. These features are presented with much more type information and safety, without introducing significant overhead or obscuring the nature of the OpenGL pipeline. It was written to facilitate a prototype which required high performance graphics code, full control of the pipeline and support for real-time geometry generation. A further, more recent goal has been to investigate replacing most of the library's API with a pure functional DSL, without any loss of control or performance.

### Highlights ###

* For an algorithm written in a pure functional style, see [TextureAtlas.fs](https://github.com/nepp2/SmallGL/blob/master/SmallGL/TextureAtlas.fs).
* For some ordinary OpenGL code, see [Guts.fs](https://github.com/nepp2/SmallGL/blob/master/SmallGL/Guts.fs).
* To see the beginnings of a pure functional DSL for generating shaders and controlling the graphics pipeline, see [ShaderDSL.fs](https://github.com/nepp2/SmallGL/blob/master/SmallGL/ShaderDSL.fs).
