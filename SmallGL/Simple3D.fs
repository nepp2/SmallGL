
/// Magic module for drawing simple 3D textured geometry
module SmallGL.Simple3D

open Geometry
open Types
open Texture
open Context
open GLSL

open OpenTK
   
/// The vertex shader source code
let vertexSource = """
#version 130
attribute vec3 aVertexPosition;
attribute vec2 aTexCoord;
varying vec2 vTexCoord;
uniform mat4 uTransform;
void main() {
  	vTexCoord = aTexCoord;
   gl_Position = uTransform * vec4(aVertexPosition, 1);
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
   val aVertexPosition : vec3<float32>
   val aTexCoord : vec2<float32>
   new (pos : Vector3, tc : Vector2) =
      {aVertexPosition = vec3 (pos.X, pos.Y, pos.Z); aTexCoord = vec2 (tc.X, tc.Y) }
   new (x, y, z, tx, ty) =
      {aVertexPosition = vec3 (x, y, z); aTexCoord = vec2 (tx, ty) }   

let inline private addCentredQuad(convert : int -> 'u, g: GeomArray<Vertex, 'u>, centre : Vector3, a : Vector3, b : Vector3, qcs : QuadCoords) =
   let vertexIndex =
      g.addVertices (
         Vertex (centre - (a + b), qcs.topLeft),
         Vertex (centre + (a - b), qcs.topRight),
         Vertex (centre + a + b, qcs.bottomRight),
         Vertex (centre + (b - a), qcs.bottomLeft)
      )
   let inline u offset = convert (vertexIndex + offset)
   g.addIndices (u 0, u 1, u 3)
   g.addIndices (u 3, u 1, u 2) 

let inline private addQuad(convert : int -> 'u, g: GeomArray<Vertex, 'u>, corner : Vector3, a : Vector3, b : Vector3, qcs : QuadCoords) =
   let vertexIndex =
      g.addVertices (
         Vertex (corner, qcs.topLeft),
         Vertex (corner + a, qcs.topRight),
         Vertex (corner + a + b, qcs.bottomRight),
         Vertex (corner + b, qcs.bottomLeft)
      )
   let inline u offset = convert (vertexIndex + offset)
   g.addIndices (u 0, u 1, u 3)
   g.addIndices (u 3, u 1, u 2)

open System.Runtime.CompilerServices
[<Extension>]
type GeometryExtension () =
   /// Generates a quad with two sides using vector 'a' and two sides of vector 'b'
   [<Extension>]
   static member inline addQuad(g, corner, a, b, qcs) =
      addQuad (uint16, g, corner, a, b, qcs)

   /// Generates a quad with two sides using vector 'a' and two sides of vector 'b'
   [<Extension>]
   static member inline addQuad(g, corner, a, b, qcs) =
      addQuad (uint32, g, corner, a, b, qcs)

   /// Generates a centred quad with two sides using vector '2a' and two sides of vector '2b'
   [<Extension>]
   static member inline addCentredQuad(g, centre, a, b, qcs) =
      addCentredQuad (uint16, g, centre, a, b, qcs)

   /// Generates a centred quad with two sides using vector '2a' and two sides of vector '2b'
   [<Extension>]
   static member inline addCentredQuad(g, centre, a, b, qcs) =
      addCentredQuad (uint32, g, centre, a, b, qcs)

/// This function exists to specify the order of the uniform array
let private createUniformArray (uTexture : 'a, uTransform : 'a) = [| uTexture ; uTransform |]

let createUniformValueArray (uTexture, uTransform) = createUniformArray (UTexture uTexture, UMat4 uTransform)

/// A render flag configuration appropriate for simple 3D rendering
let renderFlags3D =
   { depthFlag = UseDepthBuffer (true); cullFace = true; blendFlag = NoBlending }

/// Create a pipeline configuration appropriate for simple 3D rendering
let createPipeline (ctx : context, renderFlags) =
   // Create a shader program
   let shaderProgram =
      ctx.createShaderProgram (ctx.createVertexShader vertexSource, ctx.createFragmentShader fragmentSource)
   // Link uniforms to the shader program
   let uniforms = ctx.createShaderUniforms (shaderProgram, createUniformArray ("uTexture", "uTransform") )
   // Create attribute objects for each attribute in the vertex shader
   let attributes =
      let ts = [| "aVertexPosition", AttributeType.Float, 3 ; "aTexCoord", AttributeType.Float, 2 |]
      ctx.createShaderAttributes (shaderProgram, ts)
   // Create iterable attributes
   let iterableAttributes = Types.createIterableAttributes attributes
   // Create draw configuration
   Pipeline(shaderProgram, iterableAttributes, uniforms, renderFlags)

/// Update geometry in graphics memory
let updateGeometryBuffer (ctx : context, gb : GeometryBuffer, g : GeomArray<Vertex, 'ind>) =
   ctx.writeToVertexBuffer (gb.vertexBuffer, g.vertexArray, g.vertexCount)
   ctx.writeToIndexBuffer (gb.indexBuffer, g.indexArray, g.indexCount)
   let indexWidth = if sizeof<'ind> = 16 then Index16 else Index32
   gb.setIndexCount (g.indexCount, indexWidth)
