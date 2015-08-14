module SmallGL.GLSL

open System.Runtime.InteropServices
#nowarn "9" // Disable struct layout warning in this file (warns against any use of struct layout)

/// Matches the GLSL type of the same name
[<Struct>] [<StructLayout(LayoutKind.Sequential, Pack=1)>]
type vec2<'f when 'f : unmanaged > =
   val x : 'f
   val y : 'f
   new (_x, _y) = {x = _x; y = _y }

/// Matches the GLSL type of the same name
[<Struct>] [<StructLayout(LayoutKind.Sequential, Pack=1)>]
type vec3<'f when 'f : unmanaged > =
   val x : 'f
   val y : 'f
   val z : 'f
   new (_x, _y, _z) = {x = _x; y = _y; z = _z }
