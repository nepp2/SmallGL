
/// Wrappers for various different OpenGL types
module SmallGL.Types

open OpenTK.Graphics.OpenGL

/// This type represents an OpenGL context and is used as a part of all operations with involve reading or
/// writing OpenGL state.
///
/// This type should be explicitly disposed of, as the lifetime of a single instance of it corresponds to
/// the lifetime of the underlying OpenGL context. Here, "disposing" refers to use of the IDisposable
/// interface, and has nothing to do with garbage collection.
type context = { gameWindow : OpenTK.GameWindow } with
   member x.Dispose () =
      x.gameWindow.Dispose ()
   interface System.IDisposable with member x.Dispose () = x.Dispose ()

/// Defines the type of an attribute
[<Struct>]
type AttributeType =
   val sizeInBytes : int
   val typeFlag : VertexAttribPointerType
   new(_sizeInBytes, _typeFlag) = {sizeInBytes = _sizeInBytes; typeFlag = _typeFlag}

   // Attribute types (TODO: support all the types)
   static member Byte = AttributeType (1, VertexAttribPointerType.Byte)
   static member Short = AttributeType (2, VertexAttribPointerType.Short)
   static member Float = AttributeType (4, VertexAttribPointerType.Float)

/// This object describes a single attribute in a particular vertex shader.
[<Struct>]
type ShaderAttribute =
   val name : string
   val valuesPerVertex : int
   val valueType : AttributeType
   val location : int
   new(_name, _valuesPerVertex, _valueType, _location) =
      {name = _name; valuesPerVertex = _valuesPerVertex;
      valueType = _valueType; location = _location }

/// Represents an attribute that is ready for use. The attribute is linked to a particular location
/// in a shader, and knows how its values will be formatted in their buffer, so that OpenGL can
/// iterate through them to pass them into the vertex shader.
type IterableAttribute =
   val attribute: ShaderAttribute
   val byteStridePerVertex: int
   val byteOffset: int
   new(_attribute, _byteStridePerVertex, _byteOffset) =
      {attribute = _attribute; byteStridePerVertex = _byteStridePerVertex; byteOffset = _byteOffset }

/// Represents a fragment shader
[<Struct>]
type FragmentShader =
   val glShaderHandle : int
   new(_glShaderHandle) = {glShaderHandle = _glShaderHandle}

/// Represents a vertex shader
[<Struct>]
type VertexShader =
   val glShaderHandle : int
   new(_glShaderHandle) = {glShaderHandle = _glShaderHandle}

/// Represents a shader program
[<Struct>]
type ShaderProgram =
   val glProgramHandle : int
   new(_glProgramHandle) =
      {glProgramHandle = _glProgramHandle}

/// Represents a vertex buffer, storing attribute data for a shader program.
[<Struct>]
type VertexBuffer =
   val vboHandle : int
   new(_vboHandle) = {vboHandle = _vboHandle}

/// Represents an index buffer, storing indices into a vertex buffer.
[<Struct>]
type IndexBuffer =
   val vboHandle : int
   new(_vboHandle) = {vboHandle = _vboHandle}

/// Represents a buffered texture
type Texture =
   val glTextureHandle : int
   val width : int
   val height : int
   new(_glTextureHandle, _width, _height) =
      {glTextureHandle = _glTextureHandle; width = _width; height = _height }

/// Represents a shader uniform
[<Struct>]
type ShaderUniform =
   val location : int
   val name : string
   val index : int
   new(_location, _name, _index) = {location = _location; name = _name; index = _index}

/// A value suitable to be passed into a shader uniform
type UniformValue =
   | UMat4 of OpenTK.Matrix4
   | UMat3 of OpenTK.Matrix3
   | UMat2 of OpenTK.Matrix2
   | UTexture of Texture

/// Flag for controlling the behaviour of the depth buffer
type DepthFlag = NoDepthBuffer | UseDepthBuffer of depthClamp : bool

/// Flag for controlling blending behaviour
type BlendFlag =
   | NoBlending
   | UseBlending

/// Flags for controlling the behaviour of certain aspects of the rendering pipeline
type RenderFlags =
   { depthFlag : DepthFlag; cullFace : bool; blendFlag : BlendFlag }

/// An object which defines a particular configuration for OpenGL's pipeline
type Pipeline (shaderProgram, iterableAttributes, shaderUniforms, renderFlags) =
   member val shaderProgram : ShaderProgram = shaderProgram
   member val iterableAttributes : IterableAttribute[] = iterableAttributes
   member val shaderUniforms : ShaderUniform[] = shaderUniforms
   member val renderFlags : RenderFlags = renderFlags

type IndexWidth = Index16 | Index32

// An object which represents a geometry buffer in graphics memory
type GeometryBuffer (vertexBuffer, indexBuffer, indexWidth : IndexWidth, indexCount : int) =
   let mutable index_width = indexWidth
   let mutable index_count = indexCount
   member val vertexBuffer: VertexBuffer = vertexBuffer
   member val indexBuffer: IndexBuffer = indexBuffer
   member x.indexWidth with get () = index_width
   member x.numTriangles with get () = index_count / 3
   member x.indexCount with get () = index_count
   member x.setIndexCount (new_index_count, new_index_width) =
      index_count <- new_index_count
      index_width <- new_index_width

/// Creates an array of iterable attributes whose data is interleaved in a single attribute buffer, in the same
/// order that the attributes are placed in the array.
let createIterableAttributes (attributes: seq<ShaderAttribute>) : IterableAttribute[] =
   // Check that attribs ain't empty
   if Seq.isEmpty attributes then failwith "There must be at least one attribute in an interleaved source"
   // Calculate the stride per vertex
   // (the number of floats in the buffer per vertex, which is the sum of the attribute sizes)
   let byteStridePerVertex = attributes |> Seq.fold (fun v sa -> v + sa.valuesPerVertex * sa.valueType.sizeInBytes) 0
   // Return a sequence of attribute sources
   [|
      let iter =
         attributes |>
         Seq.scan (fun byteOffset a -> byteOffset + a.valuesPerVertex * a.valueType.sizeInBytes) 0 |>
         Seq.zip attributes
      for (a, byteOffset) in iter do
         yield IterableAttribute (a, byteStridePerVertex, byteOffset)
   |]

