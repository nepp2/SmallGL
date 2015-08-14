module SmallGL.Geometry

/// A buffer in main memory for the fast creation of geometry
type GeomArray<'vert, 'index> (maxVerts : int, maxIndices : int) =

   member val vertexArray = Array.zeroCreate<'vert> maxVerts
   member val indexArray = Array.zeroCreate<'index> maxIndices
   member val vertexCount = 0 with get, set
   member val indexCount = 0 with get, set
      
   member x.reset () =
      x.vertexCount <- 0
      x.indexCount <- 0

   /// Returns the index that the vertex was added at
   member x.addVertex v : int =
      let vc = x.vertexCount
      x.vertexArray.[vc] <- v
      x.vertexCount <- vc + 1
      vc

   /// Returns the index that the first vertex was added at
   member x.addVertices (v0, v1) : int =
      let vc, a = x.vertexCount, x.vertexArray 
      a.[vc] <- v0; a.[vc+1] <- v1
      x.vertexCount <- vc + 2
      vc

   /// Returns the index that the first vertex was added at
   member x.addVertices (v0, v1, v2) : int =
      let vc, a = x.vertexCount, x.vertexArray 
      a.[vc] <- v0; a.[vc+1] <- v1; a.[vc+2] <- v2
      x.vertexCount <- vc + 3
      vc

   /// Returns the index that the first vertex was added at
   member x.addVertices (v0, v1, v2, v3) : int =
      let vc, a = x.vertexCount, x.vertexArray
      a.[vc] <- v0; a.[vc+1] <- v1; a.[vc+2] <- v2; a.[vc+3] <- v3
      x.vertexCount <- vc + 4
      vc

   member x.addIndex i : unit =
      x.indexArray.[x.indexCount] <- i
      x.indexCount <- x.indexCount + 1

   member x.addIndices (i0, i1) : unit =
      let ic, a = x.indexCount, x.indexArray
      a.[ic] <- i0; a.[ic+1] <- i1
      x.indexCount <- ic + 2

   member x.addIndices (i0, i1, i2) : unit =
      let ic, a = x.indexCount, x.indexArray
      a.[ic] <- i0; a.[ic+1] <- i1; a.[ic+2] <- i2
      x.indexCount <- ic + 3
