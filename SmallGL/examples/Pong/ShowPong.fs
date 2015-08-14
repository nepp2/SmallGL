module ShowPong

open SmallGL
open Context
open Types
open Simple2D
open Shape2D
open Vectors
open System.Drawing

/// Launches pong
let startPong () =

   // Create a context and use it for the lifetime of this scope
   use ctx = Context.createContext ()
   
   // Create the graphics stuff
   let pipeline = Simple2D.createPipeline (ctx, Simple2D.renderFlags2D)
   let geometryBuffer = ctx.createGeometryBuffer Index16
   let texture =
      use bitmap = Shape2D.createColourPalette ()
      Texture.createTextureFromBitmap (ctx, bitmap)
   let geometry = Simple2D.createGeomArray 50
   // Define the screen size
   let screenWidth, screenHeight = 800.f, 600.f
   let transform = Simple2D.createStandardTransform screenWidth screenHeight
   // Create the uniform value map for the shader program
   let uniformValues = Simple2D.createUniformValueArray( texture, transform )
   // Create the initial game state
   let gameData = Pong.createGameData
                     (float32 screenWidth, float32 screenHeight,
                        10.f, 60.f,
                        10.f, 4.f)

   // Provide an update function
   let r = System.Random (1)
   let update (time:float) : unit =
      Pong.update gameData
      geometry.reset ()
      let inline genPaddle (r:RectangleF, c) =
         let hw = r.Width / 2.f
         let x = r.Left + hw
         geometry.addLine ( v2.xy x r.Top, v2.xy x r.Bottom, r.Width, c )
         geometry.addCircle (x, r.Top, hw, 10, c)
         geometry.addCircle (x, r.Bottom, hw, 10, c)
      let inline genBall (r:RectangleF, c) =
         let x, y = r.Left + r.Width / 2.f, r.Top + r.Height / 2.f
         geometry.addCircle (x, y, r.Width/2.f, 10, c)

      genBall (gameData.pong.ball.r, Color.OrangeRed)
      genPaddle (gameData.pong.p1, Color.Cyan)
      genPaddle (gameData.pong.p2, Color.Cyan)

   // Provide a rendering function
   let render (time:float) : unit =
      // Update geometry in graphics memory
      Simple2D.updateGeometryBuffer (ctx, geometryBuffer, geometry)
      // Clear screen and redraw geometry
      ctx.clearScreen (0.f, 0.f, 0.f)
      ctx.draw (geometryBuffer, pipeline, uniformValues)

   // Run the demo (blocking call)
   ctx.run(screenWidth, screenHeight, "Pong", render = render, update = update)

[<EntryPoint>]
let main args =
   startPong ()
   0