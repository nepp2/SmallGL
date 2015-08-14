
module SmallGL.Texture

open OpenTK
open System.Drawing
open System.Drawing.Drawing2D

open Types
open Context
open TextureAtlas

/// Stores the texture coordinates for a quad
type QuadCoords = {
   bottomLeft : Vector2
   bottomRight : Vector2
   topRight : Vector2
   topLeft : Vector2
   pixelWidth : int
   pixelHeight : int
}

/// Create texture
let createTextureFromBitmap (ctx : context, bitmap : Bitmap) =
   // Get the data out of the image
   let textureData = 
      bitmap.LockBits(
         Rectangle (0, 0, bitmap.Width, bitmap.Height),
         Imaging.ImageLockMode.ReadOnly,
         Imaging.PixelFormat.Format32bppArgb
      )
   // Create the texture
   let t = ctx.createTextureWithData (bitmap.Width, bitmap.Height, textureData.Scan0)
   // Free the data
   bitmap.UnlockBits(textureData)
   // Return the texture
   t

/// Invokes a file loader, applies a procedure to the resulting files, and then dispose of the files
let useFiles func (filePaths : string[]) : 'result =
   let files = filePaths |> Array.map Image.FromFile
   let result = func files
   for f in files do
      f.Dispose ()
   result

/// Finds all the png and jpg files in a particular directory
let findImages directoryPath : string[] =
      let fs = System.IO.Directory.GetFiles(directoryPath)
      let filter ext = fs |> Seq.filter (fun s -> s.EndsWith ext)
      Seq.append (filter ".png") (filter ".jpg") |> Seq.toArray

/// Create a texture atlas layout
let createAtlasLayout (images : Image[]) =
   let textures = images |> Array.mapi (fun i image -> {width = image.Width ; height = image.Height})
   TextureAtlas.layoutTextureAtlas textures

/// Create an atlas bitmap texture
let createAtlasTexture (layout : AtlasLayout, images : Image[]) =
   let bitmap = new Bitmap (layout.atlasWidth, layout.atlasHeight)
   use canvas = Graphics.FromImage (bitmap)
   canvas.InterpolationMode <- InterpolationMode.NearestNeighbor
   layout.positions |> Array.iteri (fun index p ->
      let image = images.[index]
      if p.rotated then
         image.RotateFlip (RotateFlipType.Rotate90FlipNone)
      canvas.DrawImage (image, p.rect)
   )
   bitmap

/// Create quad texture coordinates
let createQuadCoordinates (layout : AtlasLayout) =
   let atlasWidth, atlasHeight = float32 layout.atlasWidth, float32 layout.atlasHeight
   layout.positions |>
   // Map position rectangles to floating point rectangles, normalised relative to an atlas width and height of 1.0
   Array.map (fun p ->
      let x, y = (float32 p.rect.X) / atlasWidth, (float32 p.rect.Y) / atlasHeight
      let width, height = (float32 p.rect.Width) / atlasWidth, (float32 p.rect.Height) / atlasHeight
      // Convert (x, y, width, height) to coordinates of corners.
      let topLeft, topRight = Vector2 (x, y + height), Vector2 (x + width, y + height)
      let bottomLeft, bottomRight = Vector2 (x, y), Vector2 (x + width, y)
      // Check if rectangle needs to be rotated
      if p.rotated then
         {bottomLeft = topLeft; bottomRight = bottomLeft;
         topRight = bottomRight; topLeft = topRight;
         pixelWidth = p.rect.Width; pixelHeight = p.rect.Height }
      else
         {bottomLeft = bottomLeft; bottomRight = bottomRight;
         topRight = topRight; topLeft = topLeft;
         pixelWidth = p.rect.Width; pixelHeight = p.rect.Height }
   )

/// Create an atlas
let createAtlas (imagePaths : string[], imagesIn : Image[]) =
   let f (fileImages : Image[]) =
      let images = Array.append fileImages imagesIn
      let layout = createAtlasLayout images
      createAtlasTexture (layout, images), createQuadCoordinates layout
   useFiles f imagePaths

let createAtlasFromFiles (imagePaths : string[]) =
   createAtlas (imagePaths, [||])

let createAtlasFromImages (images : Image[]) =
   createAtlas ([||], images)
