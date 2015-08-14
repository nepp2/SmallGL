
(* TODO:
   This library currently pays very little attention to freeing resources.
   It should not be used for anything important until that issue is addressed.
*)

/// This module provides the implementation for the context type. All procedure calls
/// which have an effect on OpenGL's internal state should be made via an instance
/// of this type.
module SmallGL.Context

open OpenTK
open Types

/// Creates a SmallGL context. See the documentation on "context" for more information.
let createContext () =
   // Create a game window (and, by extension, a GL context)
   { context.gameWindow = new GameWindow() }

type context with

   /// Ends the rendering and closes the window
   member x.exit () =
      x.gameWindow.Exit ()

   /// Initialises an OpenGL fragment shader. Throws error if the source fails to compile.
   member x.createFragmentShader (source : string) =
      FragmentShader(Guts.createFragmentShader source)

   /// Initialises an OpenGL vertex shader. Throws error if the source fails to compile.
   member x.createVertexShader (source : string)  =
      VertexShader(Guts.createVertexShader source)

   /// Initialises an OpenGL shader program. Throws error if the source fails to compile.
   member x.createShaderProgram (vs : VertexShader, fs : FragmentShader) =
      ShaderProgram(Guts.createProgram (vs.glShaderHandle, fs.glShaderHandle))

   /// Creates an array of attributes and links them to the given shader program.
   /// Note: tuple contents are (attributeName, valueType, valuesPerVertex).
   member x.createShaderAttributes (sp : ShaderProgram, attributes : (string * AttributeType * int)[]) =
      attributes |> Array.map
         (fun (attributeName, valueType, valuesPerVertex) ->
            let attributeLocation = Guts.getAttributeLocation (attributeName, sp.glProgramHandle)
            ShaderAttribute (attributeName, valuesPerVertex, valueType, attributeLocation))

   /// Creates an array of uniforms and links them to the given shader program
   member x.createShaderUniforms (shaderProgram : ShaderProgram, uniformNames : string[]) =
      uniformNames |> Array.mapi
         (fun i uniformName ->
            let location = Guts.getUniformLocation (shaderProgram.glProgramHandle, uniformName)
            ShaderUniform(location, uniformName, i))

   /// Creates a vertex buffer
   member x.createVertexBuffer () = VertexBuffer (Guts.createVBO ())

   /// Creates an index buffer
   member x.createIndexBuffer () = IndexBuffer (Guts.createVBO ())

   /// Creates a geometry buffer
   member x.createGeometryBuffer (indexWidth) =
      // Create a vertex buffer
      let vertexBuffer = x.createVertexBuffer ()
      // Create an index buffer
      let indexBuffer = x.createIndexBuffer ()
      GeometryBuffer(vertexBuffer, indexBuffer, indexWidth, 0)

   /// Creates a texture using the data provided
   member x.createTextureWithData (width, height, argb8888Data) =
      let handle = Guts.createTexture ()
      Guts.writeToTexture (handle, width, height, 0, argb8888Data)
      Texture (handle, width, height)

   /// Writes the provided vertices to the vertex buffer
   member x.writeToVertexBuffer (vertexBuffer : VertexBuffer, vertices : 'vert[], vertexCount : int) : unit =
         Guts.writeDataToVertexBuffer (vertexBuffer.vboHandle, vertices, vertexCount * sizeof<'vert>)

   /// Writes the provided indices to the index buffer
   member x.writeToIndexBuffer (indexBuffer : IndexBuffer, indices : 'ind[], indexCount : int) : unit =
         Guts.writeDataToIndexBuffer (indexBuffer.vboHandle, indices, indexCount * sizeof<'ind>)

   /// Clears the screen
   member x.clearScreen (red, green, blue) = Guts.clear (red, green, blue)

   /// Draws some geometry using a given pipeline and uniform value configuration
   member x.draw (geometry, pipeline, uniformValues) = Guts.drawTriangles (geometry, pipeline, uniformValues)

   /// This is a blocking call which begins the execution of a render & update loop
   member x.run (width : float32, height : float32, title, ?load : unit -> unit, ?resize : unit -> unit,
                                       ?update : float -> unit, ?render : float -> unit) =
      let gameWindow = x.gameWindow
      // Setup the window
      gameWindow.Resize.Add(fun a ->
         Graphics.OpenGL.GL.Viewport(gameWindow.ClientRectangle)
      )
      gameWindow.Width <- int width
      gameWindow.Height <- int height
      gameWindow.Title <- title
      gameWindow.VSync <- VSyncMode.On
      // Set the callbacks
      match load with
         | Some l -> gameWindow.Load.Add (fun a -> l ()) | None -> ()
      match resize with
         | Some r -> gameWindow.Resize.Add (fun a -> r ()) | None -> ()
      match update with
         | Some u -> gameWindow.UpdateFrame.Add (fun a -> u a.Time) | None -> ()
      match render with
         | Some r ->
            gameWindow.RenderFrame.Add (
               fun a ->
                  r a.Time
                  gameWindow.SwapBuffers ()
            )
         | None -> ()
      // Start the window running
      gameWindow.Run ()
