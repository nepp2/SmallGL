module SmallGL.Shader

/// Shader abstract syntax tree
module AST =

   /// Shader types
   type stype =
      | SFloat
      | SVec2
      | SVec3
      | SVec4
      | SMat4
      | SSampler2D

   /// Operators
   type operator =
      | Sub
      | Mul
      | Add

   // Literals
   type literal =
      | FloatLiteral of float32

   /// Operations
   type operation =
      | Operator of expr * operator * expr
      | Function of name : string * expr[]
      | Construct of expr[]
      | Varying of expr
      | Attribute of string
      | Uniform of string
      | Literal of literal


   /// Expression
   and expr = { stype : stype ; op : operation }

   //type expr = { op : op ; ptype : ptype ; es : expr[] }

/// Shader domain specific language
module DSL =
   open AST
   
   type value_type =
      abstract member stype : stype

   type float_type = Float with
      interface value_type with member x.stype = SFloat
   type vec2_type = Vec2 with
      interface value_type with member x.stype = SVec2
   type vec3_type = Vec3 with
      interface value_type with member x.stype = SVec3
   type vec4_type = Vec4 with
      interface value_type with member x.stype = SVec4
   type mat4_type = Mat4 with
      interface value_type with member x.stype = SMat4
   type sampler2d_type = Sampler2D with
      interface value_type with member x.stype = SSampler2D

   [<Struct>]
   type value<'vt when 'vt :> value_type > =
      val e : expr
      val value_type : 'vt
      new(e, value_type) = { e = e ; value_type = value_type}

   let private toValue vt e = value(e, vt)

   let private to_stype x =
      (x :> value_type).stype

   let private expr (v : value<_>) = v.e

   // Define some shorthand functions for creating different kinds of expressions
   let private operation vtype optn = toValue vtype { stype = to_stype vtype ; op = optn }
   let private operator optr vtype e1 e2 = toValue vtype { stype = to_stype vtype ; op = Operator (e1, optr, e2) }
   let private call name vtype es = toValue vtype { stype = to_stype vtype; op = Function (name, es) }
   let private construct vtype es = toValue vtype { stype = to_stype vtype; op = Construct es }

   /// Error if the type of the value does not match the type provided
   let inline private is (v : value<'a>) (value_type : 'a) = v.e

   /// Define operators for different value type combinations
   type value<'vt, 'st> with

      static member (+) (a, b) = operator Add Float (is a Float) (is b Float)
      static member (-) (a, b) = operator Sub Float (is a Float) (is b Float)
      static member (*) (a, b) = operator Mul Float (is a Float) (is b Float)

      static member (+) (a, b) = operator Add Vec4 (is a Vec4) (is b Vec4)
      static member (-) (a, b) = operator Sub Vec4 (is a Vec4) (is b Vec4)
      static member (*) (a, b) = operator Mul Vec4 (is a Float) (is b Vec4)

      static member (*) (a, b) = operator Mul Vec4 (is a Vec4) (is b Float)
      static member (*) (a, b) = operator Mul Vec4 (is a Mat4) (is b Vec4)


   /// Unary operators

   let varying (v : value<_>) = operation v.value_type (Varying v.e)
   let uniform (name, vtype) = operation vtype (Uniform name)
   let attribute (name, vtype) = operation vtype (Attribute name)
   let f v = operation Float (Literal (FloatLiteral v))

   type Call =
      static member texture2D (tex, coord) = call "texture2D" Vec4 [| is tex Sampler2D ; is coord Vec2 |]

   /// Define a bunch of typesafe constructors for different types
   type New =
      static member vec2 (x, y) = construct Vec2 [| is x Float ; is y Float |]

      static member vec3 (x, y, z) = construct  Vec3 [| is x Float ; is y Float ; is z Float |]

      static member vec4 (x, y, z, w) = construct Vec4 [| is x Float ; is y Float ; is z Float ; is w Float |]
      static member vec4 (xy, z, w) = construct Vec4 [| is xy Vec2 ; is z Float ; is w Float |]

   /// A pure expression describing the creation of an image
   type image_generator = {
      vertexPosition : value<vec4_type>
      fragmentColour : value<vec4_type>
   }

module Codegen =
   open AST
   open DSL

   /// Sequence of expressions in the order they will be evaluated, except
   /// for any branch which does not pass the "shouldYield" test.
   let rec exprAsSeq (shouldYield : expr -> bool) (expr : expr) : seq<expr> =
      seq {
         if shouldYield(expr) then
            let inline visitArray es = Seq.collect (exprAsSeq shouldYield) es
            match expr.op with
               | Operator (e1, _, e2) ->
                  yield! exprAsSeq shouldYield e1
                  yield! exprAsSeq shouldYield e2
               | Function (_, es) -> yield! visitArray es
               | Construct es -> yield! visitArray es
               | Varying e -> yield! exprAsSeq shouldYield e
               | Attribute _ | Uniform _ | Literal _ -> ()
            yield expr
      }

   /// Sequence of all expressions in the order they will be evaluated
   let allExprsAsSeq expr = exprAsSeq (fun e -> true) expr

   open System.Collections.Generic

   /// Tracks the expressions which have been bound to variables, and other variable names which have been reserved.
   type bindings = { boundNames : Set<string> ; boundExprs : Map<expr, string> }

   let isExprBound bs expr = bs.boundExprs.ContainsKey expr

   let isNameBound bs name = bs.boundNames.Contains name

   let getBoundName bs expr = bs.boundExprs.[expr]

   let reserveName (bs, name) =
      if bs.boundNames.Contains name then failwith "Name already bound!"
      else { bs with boundNames = bs.boundNames.Add name }

   let bindExpr (bs, expr, name) =
      if bs.boundNames.Contains name then failwith "Name already bound!"
      elif bs.boundExprs.ContainsKey expr then failwith "Expression already bound!"
      else
         { boundExprs = bs.boundExprs.Add (expr, name) ;
           boundNames = bs.boundNames.Add name }

   let uniqueName bindings name  =
      if isNameBound bindings name then
         Seq.initInfinite (fun i -> sprintf "%s_%i" name i) |> Seq.find (fun n -> isNameBound bindings n |> not)
      else
         name

   /// Generates a block of code that calculates the value of the given expression and binds it to a variable
   /// with the name provided.
   let generateGLSLExpressionBlock (bindName : string, expr : expr, bindings : bindings) : string =
      /// Find the order of the first appearance of each expression, and the number of times each one appears
      let rec findReferenceInfo (expr : expr, reforder : List<expr>, refcounts : Dictionary<expr,int>) =
         let visitor expr =
            // Count this reference
            if refcounts.ContainsKey expr then
               do refcounts.[expr] <- refcounts.[expr] + 1
            else
               // Log the expression
               do refcounts.[expr] <- 1
               do reforder.Add expr
         allExprsAsSeq expr |> Seq.iter visitor
      
      // Figure out which expressions need to be bound to a name (because their result is
      // referenced multiple times), and in what order this must be done.
      let bindOrder =
         let reforder, refcounts = List<expr> (), Dictionary<expr,int> ()
         do findReferenceInfo (expr, reforder, refcounts)
         reforder |> Seq.filter (fun e -> refcounts.[e] > 1 ) |> Seq.toArray

      let stype_to_GLSL (stype : stype) : string =
         match stype with
            | SFloat -> "float" | SMat4 -> "mat4" | SVec2 -> "vec2"
            | SVec3 -> "vec3" | SVec4 -> "vec4" | SSampler2D -> "sampler2D"

      let rec genExpressionCode (expr : expr, bindings : bindings) = seq {
         if isExprBound bindings expr then
            // Reference an expression that has already been bound to a variable
            yield (getBoundName bindings expr)
         else
            // Generate the expression
            let genCall (name, subexprs : expr[]) = seq {
               yield name
               yield "("
               yield! genExpressionCode(subexprs.[0], bindings)
               for i = 1 to subexprs.Length - 1 do
                  yield ", "
                  yield! genExpressionCode(subexprs.[i], bindings)
               yield ")"
            }
            match expr.op with
            | Construct subexprs -> yield! genCall (stype_to_GLSL expr.stype, subexprs)
            | Function (name, subexprs) -> yield! genCall (name, subexprs)
            | Attribute _ | Uniform _ | Varying _ ->
               failwith "Should have been bound to a variable, and caught by the very first case."
            | Operator (sub1, op, sub2) ->
               // Binary expression
               let opstring = 
                  match op with
                  | Sub -> " - "
                  | Mul -> " * "
                  | Add -> " + "
               yield "("
               yield! genExpressionCode(sub1, bindings)
               yield opstring
               yield! genExpressionCode(sub2, bindings)
               yield ")"
            | Literal l ->
               match l with
               | FloatLiteral f -> yield (f |> double |> string)
      }

      let genBindCode (expr, bindName) =
         sprintf "%s %s = " (stype_to_GLSL expr.stype) bindName

      // Create a sequence of strings which will make up the code block
      let code =
         bindOrder |>
         Seq.fold (
            fun (code, bs, i) exprToBind ->
               let newBindName = uniqueName bs (sprintf "%s_%i" (stype_to_GLSL exprToBind.stype) i)
               let moreCode = Seq.append code (seq {
                  yield genBindCode (expr, newBindName)
                  yield! genExpressionCode (exprToBind, bindings)
                  yield ";\n"
               })
               let newBindings = bindExpr (bindings, exprToBind, newBindName)
               code, newBindings, i+1
         ) (Seq.empty, bindings, 0) |>
         (fun (a, _, _) -> a)

      // Concatenate the code strings into one long string
      String.concat "" code

   /// TODO: Laughably incomplete list of reserved words (real list might have as many as 100 reserved words)
   let reservedGLSLWords =
      [| "gl_Position" ; "gl_FragColor" ; "vec2" ; "vec3" ; "vec4" ; "float" ; "mat4" ; "attribute" ; "varying" ;
         "version" ; "uniform" ; "sampler2D" ; "mediump" ; "highp" ; "lowp" |]

   /// Generate a full GLSL vertex and fragment shader
   let generateShaderGLSL (gen : image_generator) =
      // Find varyings, attributes and uniforms
      let distinctExpr = Seq.append (allExprsAsSeq gen.vertexPosition.e) (allExprsAsSeq gen.fragmentColour.e) |> Seq.distinct |> Seq.toArray
      //let varyings = distinctExpr |> Seq.filter (fun e -> match e.op with Varying _ -> true | _ -> false) |>
      let attributes = distinctExpr |> Array.choose (fun e -> match e.op with Attribute name -> Some (e, name) | _ -> None)
      let uniforms = distinctExpr |> Array.choose (fun e -> match e.op with Uniform name -> Some (e, name) | _ -> None)

      let reservedNames = reservedGLSLWords |> Seq.append (attributes |> Seq.map snd) |> Seq.append (uniforms |> Seq.map snd)

      let varyingNameStart =
         let rec findUniqueStart n =
            if reservedNames |> Seq.exists (fun s -> s.StartsWith n) then
               findUniqueStart (sprintf "%s_" n)
            else n
         findUniqueStart "varying_"

      let varyings = distinctExpr |> Seq.filter (fun e -> match e.op with Varying _ -> true | _ -> false)
                     |> Seq.mapi (fun i e -> e, sprintf "%s%i" varyingNameStart i)
      
      let boundNames = reservedNames |> Seq.append (varyings |> Seq.map snd) |> Set.ofSeq
      let boundExprs = attributes |> Seq.append uniforms |> Seq.append varyings |> Map.ofSeq
      

      let vertexPositionName, bindings =
         let bs = { boundNames = boundNames ; boundExprs = boundExprs }
         let n = uniqueName bs "vertexPosition"
         n, reserveName (bs, n)
      let vertexPositionBlock = generateGLSLExpressionBlock (vertexPositionName, gen.vertexPosition.e, bindings)
      ()