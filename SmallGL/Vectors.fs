module SmallGL.Vectors
   
open OpenTK

type vec3 = Vector3
type vec2 = Vector2
type mat4 = Matrix4

module v2 =
   let inline xy x y = vec2(x,y)

   let inline x v = xy v 0.f
   let inline y v = xy 0.f v

   let toXZ (xz : vec2) = vec3(xz.X, 0.f, xz.Y)
   let toXZwithY (xz : vec2) y = vec3(xz.X, y, xz.Y)

   /// Set both components to the same value
   let inline vv v = xy v v

   let zero = vv 0.f

module v3 =

   let inline xyz x y z = vec3(x, y, z)

   let inline x v = xyz v 0.f 0.f
   let inline y v = xyz 0.f v 0.f
   let inline z v = xyz 0.f 0.f v

   let inline xy x y = xyz x y 0.f
   let inline xz x z = xyz x 0.f z
   let inline yz y z = xyz 0.f y z

   /// Set all components to the same value
   let inline vvv v = xyz v v v

   let zero = vvv 0.f
