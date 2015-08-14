
/// Magic module for drawing simple 2D textured geometry
module SmallGL.Simple2D

open SmallGL
open Context
open Geometry
open Types
open Texture
open GLSL

open OpenTK
   
/// The vertex shader source code
let vertexSource = """
#version 130
attribute vec2 aVertexPosition;
attribute vec2 aTexCoord;
varying vec2 vTexCoord;
uniform mat4 uTransform;
void main() {
  	vTexCoord = aTexCoord;
   gl_Position = uTransform * vec4(aVertexPosition, 0, 1);
}
"""

/// The fragment shader source code
let fragmentSource = """
#version 130
precision mediump float;
varying vec2 vTexCoord;
uniform sampler2D uTexture;
void main() {
  	gl_FragColor = texture2D(uTexture, vTexCoord);
}
"""

open System.Runtime.InteropServices
#nowarn "9" // Disable struct layout warning in this file (warns against any use of struct layout)

/// Defines the attributes that make up a single vertex (matches the vertex shader)
[<Struct>] [<StructLayout(LayoutKind.Sequential, Pack=1)>]
type Vertex =
   val aVertexPosition : vec2<float32>
   val aTexCoord : vec2<float32>
   new (x, y, tc : Vector2) =
      {aVertexPosition = vec2 (x, y); aTexCoord = vec2 (tc.X, tc.Y) }
   new (x, y, tx, ty) =
      {aVertexPosition = vec2 (x, y); aTexCoord = vec2 (tx, ty) }

open System.Runtime.CompilerServices

[<Extension>]
type GeometryExtension () =

   [<Extension>]
   static member inline addQuad(g: GeomArray<Vertex, uint16>, bottomLeft, bottomRight, topRight, topLeft) =
      let vertexIndex =
         g.addVertices (bottomLeft, bottomRight, topRight, topLeft)
      let inline u offset = uint16 (vertexIndex + offset)
      g.addIndices (u 0, u 3, u 1)
      g.addIndices (u 3, u 2, u 1)

   [<Extension>]
   static member inline addQuad(g: GeomArray<Vertex, uint16>, x, y, width, height, qcs : QuadCoords) =
      GeometryExtension.addQuad (g,
         Vertex (x, y, qcs.bottomLeft),
         Vertex (x + width, y, qcs.bottomRight),
         Vertex (x + width, y + height, qcs.topRight),
         Vertex (x, y + height, qcs.topLeft)      
      )

let createGeomArray maxQuads = GeomArray (maxQuads * 4, maxQuads * 6)

/// This function exists to specify the order of the uniform array
let createUniformArray (uTexture : 'a, uTransform : 'a) = [| uTexture ; uTransform |]

let createUniformValueArray (uTexture, uTransform) = createUniformArray (UTexture uTexture, UMat4 uTransform)

/// Creates a transform such that the pixel coordinates start in the top left
/// corner, and a distance of 1.f is equivalent to a single pixel.
let createStandardTransform screenWidth screenHeight =
   Matrix4.CreateScale(2.f / screenWidth, -2.f / screenHeight, 1.f) *
      Matrix4.CreateTranslation(Vector3 (-1.f, 1.f, 0.f))

/// A render flag configuration appropriate for simple 2D rendering
let renderFlags2D =
   { depthFlag = NoDepthBuffer; cullFace = false; blendFlag = UseBlending }

/// Create a pipeline configuration appropriate for simple 2D rendering
let createPipeline (ctx : context, renderFlags) =
   // Create a shader program
   let shaderProgram =
      ctx.createShaderProgram (ctx.createVertexShader vertexSource, ctx.createFragmentShader fragmentSource)
   // Link uniforms to the shader program
   let uniforms = ctx.createShaderUniforms (shaderProgram, createUniformArray ("uTexture", "uTransform") )
   // Create attribute objects for each attribute in the vertex shader
   let attributes =
      let ts = [| "aVertexPosition", AttributeType.Float, 2 ; "aTexCoord", AttributeType.Float, 2 |]
      ctx.createShaderAttributes (shaderProgram, ts )
   // Create iterable attributes
   let iterableAttributes = createIterableAttributes attributes
   // Create draw configuration
   Pipeline(shaderProgram, iterableAttributes, uniforms, renderFlags)

/// Update geometry in graphics memory
let updateGeometryBuffer (ctx : context, gb : GeometryBuffer, g : GeomArray<Vertex, uint16>) =
   ctx.writeToVertexBuffer (gb.vertexBuffer, g.vertexArray, g.vertexCount)
   ctx.writeToIndexBuffer (gb.indexBuffer, g.indexArray, g.indexCount)
   gb.setIndexCount (g.indexCount, Index16)
