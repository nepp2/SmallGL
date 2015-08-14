
/// This module attempts to encapsulate all direct interaction with OpenGL, such that no
/// calls to an OpenGL procedure are made anywhere else in this library. It is likely to be
/// the most fragile, unsafe code in this library, and should be approached with caution.
///
/// The functions provided in this module are intended for use by other library code, not by
/// ordinary users of the library.
module SmallGL.Guts

open OpenTK.Graphics.OpenGL
open SmallGL.Types

/// Sets the clear colour and clears the screen
let clear (r, g, b) : unit =
   GL.ClearColor (r, g, b, 1.f) // TODO is it worth avoiding resetting the colour?
   GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

let private textureUnits =
   [| TextureUnit.Texture0 ; TextureUnit.Texture1 ; TextureUnit.Texture2 ; TextureUnit.Texture3
      TextureUnit.Texture4 ; TextureUnit.Texture5 ; TextureUnit.Texture6 ; TextureUnit.Texture7 |]

/// Draws triangles using the shader program and vertex attribute buffer provided.
let drawTriangles (geometry : GeometryBuffer, pipeline : Pipeline, uniformValues : UniformValue[]) : unit =
   // Make sure everything has been constructed properly
   if Seq.isEmpty pipeline.iterableAttributes
      then failwith "There must be at least one attribute to draw something"
   let nameString () = pipeline.shaderUniforms |> Seq.map (fun u -> u.name) |> String.concat ", "
   if pipeline.shaderUniforms.Length > uniformValues.Length then
      failwith (sprintf "Uniforms '%s' did not match up with uniform values found in draw data." (nameString ()))
   if pipeline.shaderUniforms.Length < uniformValues.Length then
      failwith (sprintf "Excess uniform values provided. Pipeline configuration only defines these uniforms: '%s'." (nameString ()))
   // Apply render flag configuration to OpenGL flags
   let rf = pipeline.renderFlags
   match rf.blendFlag with
      | NoBlending ->
         GL.Disable (EnableCap.Blend)
      | UseBlending ->
         GL.Enable (EnableCap.Blend)
         GL.BlendFunc (BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha)
   match rf.depthFlag with
      | NoDepthBuffer ->
         GL.Disable(EnableCap.DepthTest)
      | UseDepthBuffer depthClamp ->
         GL.Enable (EnableCap.DepthTest)
         GL.DepthFunc (DepthFunction.Less)
         if depthClamp
            then GL.Enable (EnableCap.DepthClamp)
            else GL.Disable (EnableCap.DepthClamp)
   if rf.cullFace
      then GL.Enable (EnableCap.CullFace)
      else GL.Disable (EnableCap.CullFace)

   // Set uniform values (TODO: don't set any uniform that is already correctly set)
   let mutable tex_count = 0
   for u in pipeline.shaderUniforms do
      match uniformValues.[u.index] with
         // Matrix uniforms
         | UMat2 mat ->
            let mutable m = mat
            GL.UniformMatrix2 (u.location, false, &m)
         | UMat3 mat ->
            let mutable m = mat
            GL.UniformMatrix3 (u.location, false, &m)
         | UMat4 mat ->
            let mutable m = mat
            GL.UniformMatrix4 (u.location, false, &m)
         // Texture uniform
         | UTexture tex ->
            GL.Uniform1 (u.location, tex_count)
            GL.ActiveTexture (textureUnits.[tex_count])
            GL.BindTexture (TextureTarget.Texture2D, tex.glTextureHandle)
            tex_count <- tex_count + 1

   // Bind shader (TODO: don't do this if not necessary)
   GL.UseProgram pipeline.shaderProgram.glProgramHandle
   // Bind vertex buffer
   GL.BindBuffer (BufferTarget.ArrayBuffer, geometry.vertexBuffer.vboHandle)
   // Setup all of the attribute sources properly
   for a in pipeline.iterableAttributes do
      // Enable the attribute
      GL.EnableVertexAttribArray a.attribute.location
      // Set its location in the buffer
      GL.VertexAttribPointer
         (a.attribute.location, a.attribute.valuesPerVertex,
         a.attribute.valueType.typeFlag, false, a.byteStridePerVertex, a.byteOffset)
   // Bind index buffer
   GL.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.indexBuffer.vboHandle)
   // Draw the triangles
   let indexWidth =
      match geometry.indexWidth with
      | Index16 -> DrawElementsType.UnsignedShort
      | Index32 -> DrawElementsType.UnsignedInt
   GL.DrawElements (PrimitiveType.Triangles, geometry.numTriangles * 3, indexWidth, System.IntPtr.Zero)

   // Disable the attributes again. TODO is this necessary?
   for a in pipeline.iterableAttributes do
      GL.DisableVertexAttribArray a.attribute.location

/// Create a texture and returns its integer handle.
let createTexture () : int =
   let texture = GL.GenTexture ()
   GL.BindTexture (TextureTarget.Texture2D, texture)
   GL.PixelStore (PixelStoreParameter.UnpackAlignment, 1) // TODO: not sure about this
   GL.TexParameter (TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
   GL.TexParameter (TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.LinearMipmapLinear) //Nearest)
   texture

/// Write image data provided to texture. Input data is expected to have format "ARGB", with one byte per channel.
let writeToTexture (glTextureHandle : int, width : int, height : int, border : int, argb8888Data : nativeint) : unit =
   GL.BindTexture (TextureTarget.Texture2D, glTextureHandle)
   GL.TexImage2D (TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, width, height, border, PixelFormat.Bgra, PixelType.UnsignedInt8888Reversed, argb8888Data)
   GL.GenerateMipmap(GenerateMipmapTarget.Texture2D)


/// Creates a vertex buffer object.
let createVBO () : int =
   // Create the OpenGL vertex buffer object and retrieve the handle
   let mutable vboHandle = 0 
   GL.GenBuffers (1, &vboHandle)
   // Return the handle
   vboHandle

/// Writes some data to a vertex buffer object
let writeDataToVBO (vboHandle : int, bufferTarget : BufferTarget, data: 'T[], numBytes : int) : unit =
   // Check that OpenGL is not going to read beyond the end of the data buffer
   let bytesAvailable = sizeof<'T> * data.Length
   if numBytes > bytesAvailable then
      failwith "Attempted to write more bytes to vertex buffer than were provided in the data array."
   // Bind the buffer, for the sake of the glBufferData call
   GL.BindBuffer (bufferTarget, vboHandle)      
   // Load the vertex data into graphics memory
   GL.BufferData(bufferTarget, nativeint(numBytes), data, BufferUsageHint.DynamicDraw)

/// Writes some data to a VBO configured to act as a vertex buffer, storing attribute data for vertices.
let writeDataToVertexBuffer (vboHandle, data, numBytes) =
   writeDataToVBO (vboHandle, BufferTarget.ArrayBuffer, data, numBytes)

/// Writes some data to a VBO configured to act as an index buffer, storing indices into a vertex buffer.
let writeDataToIndexBuffer (vboHandle, data, numBytes) =
   writeDataToVBO (vboHandle, BufferTarget.ElementArrayBuffer, data, numBytes)

/// Creates a shader object, throws error if compilation fails.
let private createShader (source: string, shaderType: ShaderType) : int =
   // Create the OpenGL shader object and retrieve the handle
   let glShaderHandle = GL.CreateShader shaderType
   // Provide the shader source code to OpenGL
   GL.ShaderSource (glShaderHandle, source)
   // Tell OpenGL to compile the source code
   GL.CompileShader glShaderHandle
   // Read the "compile status" shader parameter. If it is false, compilation failed and an error should be thrown.
   let GetShaderi (handle : int) (param : ShaderParameter) =
      let mutable result = 0
      GL.GetShader(handle, param, &result)
      result
   if (GetShaderi glShaderHandle ShaderParameter.CompileStatus) = 0 then
      // Throw an error whose message contains the compilation error message that OpenGL provides
      failwith (shaderType.ToString() + " failed to compile:\n\n" + (GL.GetShaderInfoLog glShaderHandle))
   // Return the handle
   glShaderHandle

/// Creates a vertex shader object, throws error if compilation fails.
let createVertexShader (source: string) : int = createShader (source, ShaderType.VertexShader)

/// Creates a fragment shader object, throws error if compilation fails.
let createFragmentShader (source: string) : int = createShader (source, ShaderType.FragmentShader)

/// Links an attribute in a given shader program to a particular vertex buffer.
/// Returns the attribute location.
let getAttributeLocation (attributeName: string, shaderProgramHandle: int) : int =
   // Retrive the attribute location. This is just a number indicating which "slot" the attribute 
   // has been assigned to, and is presumably an offset into some array on the graphics card. OpenGl
   // exposes this value as a handle, because OpenGL loves exposing itself.
   let attribLocation = GL.GetAttribLocation (shaderProgramHandle, attributeName)
   if attribLocation < 0 then failwith "glGetAttribLocation returned negative value"
   attribLocation

/// Links a uniform in a given shader program to a particular vertex buffer.
/// Returns the uniform location.
let getUniformLocation (glProgramHandle : int, uniformName : string) : int =
   // Retrive the uniform location. See "getAttributeLocation" for some idiotic discussion on this subject.
   GL.GetUniformLocation (glProgramHandle, uniformName)

/// Creates a program object, throws error if linking fails.
let createProgram (vertexShaderHandle: int, fragmentShaderHandle: int) : int =
   // Create the OpenGL program object and retrieve the handle
   let glProgramHandle = GL.CreateProgram()
   // Attach the vertex shader object to the program object
   GL.AttachShader (glProgramHandle, vertexShaderHandle)
   // Attach the fragment shader object to the program object
   GL.AttachShader(glProgramHandle, fragmentShaderHandle)
   // Link the compiled fragment and shader programs into a single program
   GL.LinkProgram glProgramHandle
   // Read the "link status" program parameter. If it is false (zero), the program failed to link and an error should be thrown.
   if GL.GetProgram (glProgramHandle, GetProgramParameterName.LinkStatus) = 0 then
      // Throw an error whose message contains the linker error message that OpenGL provides
      failwith ("Shader program failed to link:\n\n" + (GL.GetProgramInfoLog glProgramHandle))
   // Validate the resulting program (my assumption is that an incorrect program can link successfully, so a validation step is necessary too)
   GL.ValidateProgram glProgramHandle
   // Read the "validate status" program parameter. If it is false (zero), the program is not functional and an error should be thrown.
   if GL.GetProgram (glProgramHandle, GetProgramParameterName.ValidateStatus) = 0 then
      // Throw an error whose message contains the compilation error message that OpenGL provides
      failwith ("Shader failed to compile:\n\n" + (GL.GetProgramInfoLog glProgramHandle))
   glProgramHandle
