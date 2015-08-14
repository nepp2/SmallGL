
/// A module providing some functions for drawing simple coloured shapes, primarily as extension methods for
/// the GeomArray type. These shapes are coloured using a colour palette and texture coordinates, so that
/// they can be drawn in the same rendering pass as normal textured geometry where necessary, with any kind
/// of shader effect.
module SmallGL.Shape2D

open SmallGL
open Geometry
open Simple2D

open OpenTK

open Vectors

open System.Drawing
open System.Drawing.Imaging
open System.Drawing.Drawing2D

// Some value to define the colour palette that is in use

let private bitDepth = 64
let private normaliser = 256 / bitDepth

let private paletteWidth, paletteHeight =
   let mutable x, y = bitDepth, bitDepth * bitDepth
   while y > x do
      x <- x * 2
      y <- y / 2
   x, y

let private rgbToXyInt r g b =
   let linearpos = r * bitDepth * bitDepth + g * bitDepth + b
   linearpos / paletteHeight, linearpos % paletteHeight

/// Convert R G B colour values (0 to 255) into texture coordinate which index the corresponding colour in
/// the palette.
let rgbToXy (r : byte) (g : byte) (b : byte) =
   let x, y = rgbToXyInt (int r / normaliser) (int g / normaliser) (int b / normaliser)
   let w, h = float32 paletteWidth, float32 paletteHeight
   (float32 x + 0.5f) / w, (float32 y + 0.5f) / h

/// Convert a Color struct into texture coordinates which index the corresponding colour in the palette.
let colourToXy (colour : Color) =
   rgbToXy colour.R colour.G colour.B

let createColourPalette () =
   let bitmap = new Bitmap(paletteWidth, paletteHeight, PixelFormat.Format24bppRgb)
   use graphics = Graphics.FromImage(bitmap)
   use brush = new SolidBrush (Color.Red)
   for r = 0 to bitDepth - 1 do
      for g = 0 to bitDepth - 1 do
         for b = 0 to bitDepth - 1 do
            brush.Color <- Color.FromArgb (r * normaliser, g * normaliser, b * normaliser)
            let x, y = rgbToXyInt r g b
            graphics.FillRectangle (brush, x, y, 1, 1)
   bitmap

open System.Runtime.CompilerServices

[<Extension>]
type GeometryExtension () =
   
   /// Takes a range of numbers, which are assumed to index an ordered sequence of vertices which outline a convex polygon.
   /// The configuration of indices needed to draw this polygon as a triangle fan are added to the geometry array provided.
   [<Extension>]
   static member addConvexPolygonIndices(g: GeomArray<Vertex, uint16>, fromIndex : int, toIndex : int) =
      // Use the index range specified to make a triangle fan. E.g. the range 1 to 5 results in [1, 2, 3], [1, 3, 4] and [1, 4, 5] ) 
      for i = fromIndex + 1 to toIndex - 1 do
         g.addIndices(uint16 fromIndex, uint16 i, uint16 (i + 1))

   [<Extension>]
   static member addCircle (g: GeomArray<Vertex, uint16>, centreX, centreY, radius, numPoints, colour : Color) =
      let div = float32 numPoints
      let colourX, colourY = colourToXy colour
      let fromIndex = g.vertexCount
      // Generate vertices
      for i = 0 to numPoints - 1 do
         let angle = ((float32 i) / div) * MathHelper.TwoPi
         ignore <| g.addVertex (Vertex (sin angle * radius + centreX, cos angle * radius + centreY, colourX, colourY))
      // Generate indices
      GeometryExtension.addConvexPolygonIndices (g, fromIndex, fromIndex + (numPoints - 1))

   [<Extension>]
   static member addLine(g: GeomArray<Vertex, uint16>, a: vec2, b: vec2, thickness : float32, colour : Color) =
      // Calculate the line vector
      let ab = b - a
      // Calculate the vector perpendicular to the line, normalised to a length of 0.5
      let p = ab.PerpendicularRight.Normalized () * 0.5f * thickness
      // Create the four corners of the line
      let colourX, colourY = colourToXy colour
      let inline vertex (v : vec2) = Vertex(v.X, v.Y, colourX, colourY)
      g.addQuad (
         vertex (a - p),
         vertex (a + p),
         vertex (b + p),
         vertex (b - p))
