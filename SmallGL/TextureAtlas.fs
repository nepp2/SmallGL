
/// This module contains an algorithm for generating texture atlases which fit onto power-of-2 textures.
///
/// The algorithm and implementation could both be improved a lot, but the performance and result are fine
/// for the expected use-case (a single, upfront calculation. Not something that happens every frame.)
module SmallGL.TextureAtlas

open System.Drawing

type TextureSize = { width : int; height : int}

/// Indexed wrapper. This is required so that the texture positions calculated by the algorithm can be
/// resorted into the original order of the texture size array.
type IW<'a> = {a: 'a ; index : int }

/// Represents a texture position. If the rotated flag is true, it means that the original texture was
/// rotated by 90 degrees when placed.
type TexturePosition = { rect : Rectangle ; rotated : bool } with
   member x.topRight = Point (x.rect.Right, x.rect.Top)
   member x.bottomLeft = Point (x.rect.Left, x.rect.Bottom)

let private createTexPos tex_size (p : Point) rotated =
   let w, h = if rotated then tex_size.a.height, tex_size.a.width else tex_size.a.width, tex_size.a.height
   { a = { rect = Rectangle (p.X, p.Y, w, h) ; rotated = rotated }; index = tex_size.index }

/// The output of the atlas layout algorithm
type AtlasLayout = { positions : TexturePosition[] ; atlasWidth : int ; atlasHeight : int }

/// This function recurses, increasing the atlasWidth and atlasHeight values as necessary
/// until all of the textures fit onto the atlas together.
let rec private placeTextures texturesByDescendingArea atlasWidth atlasHeight : AtlasLayout =
   let atlasRect = Rectangle (0, 0, atlasWidth, atlasHeight)
   // Recursive backtracking algorithm trying all sensible configurations (the textures are always axis-aligned,
   // and a texture is always placed such that one of its corners is touching a corner of another texture that
   // has already been placed.)
   let rec placeTexturesInner texture_sizes (positions : List<IW<TexturePosition>>) =
      match texture_sizes with
      | [] -> Some positions
      | tex :: texturesTail ->
         // Create a sequence of every possible position for the new texture
         // Start from the top corner, with both rotations.
         seq {
               yield createTexPos tex Point.Empty true
         } |>
         // Include all positions such that the top left corner is either touching the bottom left or
         // top right corner of a texture that is already placed, with both rotations for each.
         Seq.append
            (positions |> Seq.collect (
               fun pos -> seq {
                  let bottomLeft, topRight = pos.a.bottomLeft, pos.a.topRight
                  yield createTexPos tex bottomLeft false
                  yield createTexPos tex bottomLeft true
                  yield createTexPos tex topRight false
                  yield createTexPos tex topRight true
               }
            )) |>
         // Filter out any position that collides with an existing rectangle
         Seq.filter (
            fun newPos ->
               atlasRect.Contains newPos.a.rect &&
                  positions |> Seq.forall (fun p -> not (p.a.rect.IntersectsWith newPos.a.rect) )
         ) |>
         // For all remaining positions, assume that the position is used and recurse to try to complete the atlas
         Seq.choose (fun newPos -> placeTexturesInner texturesTail (newPos :: positions) ) |>
         // Pick the first completed atlas found. This algorithm is expressed as a big sequence expression (which
         // will evaluate lazily), so no more than one completed atlas will actually be calculated.
         Seq.tryFind (fun _ -> true)

   let positions = placeTexturesInner texturesByDescendingArea []
   match positions with
      | Some p ->
         // Sort the textures back into their original order using the indexed wrapper records
         let ps = p |> List.sortBy (fun p -> p.index) |> Seq.map (fun p -> p.a) |> Seq.toArray
         // Return a completed AtlasLayout record
         { positions = ps; atlasWidth = atlasWidth; atlasHeight = atlasHeight }
      | None ->
         // No valid layout was found for the textures to fit into the current size. Increase the
         // atlas size and then try to place the textures again.
         if atlasWidth <= atlasHeight
            then placeTextures texturesByDescendingArea (atlasWidth * 2) atlasHeight
            else placeTextures texturesByDescendingArea atlasWidth (atlasHeight * 2)

/// Accepts an array of texture sizes and returns an AtlasLayout.
let layoutTextureAtlas (textures : TextureSize[]) : AtlasLayout =
   // Calculate total area, max width and max height
   let totalArea, biggestWidth, biggestHeight =
      textures |> Array.fold (
         fun (ta, mw, mh) texture ->
            let w, h = texture.width, texture.height
            max ta (w * h), max mw w, max mh h) 
            (0, 0, 0)
   // Calculate a starting size for the texture atlas
   let rec startingSize w h =
      if w * h > totalArea then w, h
      elif w > h then startingSize w (h * 2)
      else startingSize (w * 2) h
   let roundUpToPowerOf2 i =
      let mutable powerOf2 = 2
      while i > powerOf2 do powerOf2 <- powerOf2 * 2
      powerOf2
   let atlasWidth, atlasHeight = startingSize (roundUpToPowerOf2 biggestWidth) (roundUpToPowerOf2 biggestHeight)
   // Sort by ascending area
   let texturesByDescendingArea =
      textures |>
      // Wrap with an index so the textures can be sorted back to their original order later
      Seq.mapi (fun i t -> {a = t; index = i}) |>
      // Sort by texture area for the sake of the placement algorithm
      Seq.sortBy (fun t -> -(t.a.width * t.a.height)) |>
      Seq.toList
   // Do some kind of recursive backtracking thing to fit all of the images onto the atlas
   placeTextures texturesByDescendingArea atlasWidth atlasHeight
