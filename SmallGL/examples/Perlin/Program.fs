
open SmallGL.Shader

open DSL

open Microsoft.FSharp.Reflection

// ######
let vpos = attribute ("aVertexPosition", Vec2)
let tcoord = attribute ("aTexCoord", Vec2)
let uTransform = uniform ("uTransform", Mat4)
let gl_position = uTransform * New.vec4(vpos, f 0.f, f 1.f)

let vTexCoord = varying (tcoord)
let uTexture = uniform ("uTexture", Sampler2D)

let gl_FragColor = Call.texture2D(uTexture, vTexCoord);

let generator = { vertexPosition = gl_position ; fragmentColour = gl_FragColor }

// ######

/// The vertex shader source code
let vertexSource = """
#version 130
attribute vec2 aVertexPosition;
attribute vec2 aTexCoord;
varying vec2 vTexCoord;
uniform mat4 uTransform;
void main() {
  	vTexCoord = aTexCoord;
   gl_Position = uTransform * vec4(aVertexPosition, 0, 1);
}
"""

/// The fragment shader source code
let fragmentSource = """
#version 130
precision mediump float;
varying vec2 vTexCoord;
uniform sampler2D uTexture;
void main() {
  	gl_FragColor = texture2D(uTexture, vTexCoord);
}
"""

open System.Collections.Generic
open AST

let boundNames = Dictionary<expr, string> ()

printfn "%s" <| Codegen.generateGLSLExpressionBlock ("gl_position", gl_position.e, boundNames)


