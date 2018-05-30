(** CAD in 1D, 2D, 3D *)

open Util
open RStream
open NumSys
open Geom
open Mesh

module type CAD1 = sig
  (** 1D CAD *)

  type num
  type mesh

  type unop =
    | Hull
    | Trans of num
    | Scale of num

  type binop =
    | Union
    | Diff
    | Inter

  type t =
    | Empty
    | Unit
    | Mesh  of mesh
    | Unop  of unop * t
    | Binop of binop * t * t

  val compile   : t -> mesh
  val num_prims : t -> int
  val simplify  : t -> t
  val rand      : int -> t
  val to_string : t -> string

end

module RawCAD1
  (N : NUM)
  (M : MESH1 with type num = N.t
              and type pt  = N.t)
= struct

  type num  = N.t
  type mesh = M.t

  type unop =
    | Hull
    | Trans of num
    | Scale of num

  type binop =
    | Union
    | Diff
    | Inter

  type t =
    | Empty
    | Unit
    | Mesh  of mesh
    | Unop  of unop * t
    | Binop of binop * t * t

  (** [size] returns the number of nodes
    in a 1D CAD AST *)
  let rec size = function
    | Empty  -> 1
    | Unit   -> 1
    | Mesh _ -> 1
    | Unop  (op, c) -> 1 + size c
    | Binop (op, c1, c2) -> 1 + size c1 + size c2

  (** [height] returns the depth of a 1D CAD AST *)
  let rec height = function
    | Empty  -> 0
    | Unit   -> 0
    | Mesh _ -> 0
    | Unop  (op, c) -> 1 + height c
    | Binop (op, c1, c2) -> 1 + Pervasives.max (height c1)
                                               (height c2)
  (** [num_prims] returns the number of
    primitives in a 1D CAD AST *)
  let rec num_prims = function
    | Empty  -> 1
    | Unit   -> 1
    | Mesh _ -> 1
    | Unop  (op, c) -> num_prims c
    | Binop (op, c1, c2) -> num_prims c1 + num_prims c2

  let transl_unop = function
    | Hull    -> M.hull
    | Trans d -> M.trans d
    | Scale f -> M.scale f

  let transl_binop = function
    | Union -> M.union
    | Diff  -> M.diff
    | Inter -> M.inter

  (** 1D CAD compiler calling mesh functions *)
  let rec compile = function
    | Empty ->
        M.mesh []
    | Unit ->
        M.mesh [N.n0; N.n1]
    | Mesh m ->
        m
    | Unop (op, c) ->
        transl_unop op (compile c)
    | Binop (op, cA, cB) ->
        transl_binop op (compile cA) (compile cB)

  (** [simplify] tries to remove nodes from the AST that
    do not have any effect on the mesh *)
  let rec simplify c =
    let m = compile c in
    match c with
    | Empty  -> Empty
    | Unit   -> Unit
    | Mesh m -> Mesh m
    | Unop (op, c) ->
        let sc = simplify c in
        if M.equiv m (compile sc) then
          sc
        else
          Unop (op, sc)
    | Binop (op, cA, cB) ->
        if m = compile Empty then
          Empty
        else
          let sA = simplify cA in
          let sB = simplify cB in
          if M.equiv m (compile sA) then
            sA
          else if M.equiv m (compile sB) then
            sB
          else
            Binop (op, sA, sB)

  let rec rand n =
    if n <= 0 then
      Unit
    else
      match Random.int 7 with
      | 0 -> Unit
      | 1 -> Unop  (Hull, rand (n / 2))
      | 2 -> Unop  (Trans (N.rand ()), rand (n / 2))
      | 3 -> Unop  (Scale (N.rand ()), rand (n / 2))
      | 4 -> Binop (Union, rand (n / 2), rand (n / 2))
      | 5 -> Binop (Diff,  rand (n / 2), rand (n / 2))
      | 6 -> Binop (Inter, rand (n / 2), rand (n / 2))
      | _ -> failwith "CAD1.rand: off the rails"

  let string_of_unop = function
    | Hull ->
        "Hull"
    | Trans d ->
        Printf.sprintf "Trans(%s)"
          (N.to_string d)
    | Scale f ->
        Printf.sprintf "Scale(%s)"
          (N.to_string f)

  let string_of_binop = function
    | Union -> "Union"
    | Diff  -> "Diff"
    | Inter -> "Inter"

  let rec to_string_aux n = function
    | Empty ->
        [indent n "Empty"]
    | Unit ->
        [indent n "Unit"]
    | Mesh m ->
        let ms = M.to_string m in
        let ss = String.split_on_char '\n' ms in
        indent n ("Mesh" ^ " {") ::
        List.map (indent (n + 2)) ss @
        [indent n "}"]
    | Unop (op, c) ->
        indent n (string_of_unop op ^ " {") ::
        to_string_aux (n + 2) c @
        [indent n "}"]
    | Binop (op, cA, cB) ->
        indent n (string_of_binop op ^ " {") ::
        to_string_aux (n + 2) cA @
        to_string_aux (n + 2) cB @
        [indent n "}"]

  let to_string c =
    c |> to_string_aux 0
      |> String.concat "\n"
      |> (^) "CAD1\n\n"
end

module CAD1
  (N : NUM)
  (M : MESH1 with type num = N.t
              and type pt  = N.t)
  : (CAD1 with type num  = N.t
           and type mesh = M.t)
= RawCAD1(N)(M)

module type CAD2 = sig
  (** 2D CAD *)

  type num
  type mesh

  type unop =
    | Hull
    | Trans  of (num * num)
    | Home   of (num * num)
    | Scale  of (num * num)
    | Fit    of (num * num)
    | Rotate of num

  type binop =
    | Union
    | Diff
    | Inter

  type t =
    | Empty
    | Unit
    | Mesh  of mesh
    | Unop  of unop  * t
    | Binop of binop * t * t

  val mkhole  : mesh  -> t
  val mkunop  : unop  -> t -> t
  val mkbinop : binop -> t -> t -> t
  val cmp     : t -> t -> cmp

  exception Compile of t * string
  val compile : t -> mesh
  val num_prims : t -> int
  val simplify  : t -> t
  val rand      : int -> t
  val to_string : t -> string

  val smaller_fail : (t -> 'a) -> t -> t option

end

module RawCAD2
  (N : NUM)
  (M : MESH2 with type num = N.t
              and type pt  = N.t * N.t)
= struct

  type num  = N.t
  type mesh = M.t

  type unop =
    | Hull
    | Trans  of (num * num)
    | Home   of (num * num)
    | Scale  of (num * num)
    | Fit    of (num * num)
    | Rotate of num

  type binop =
    | Union
    | Diff
    | Inter

  type t =
    | Empty
    | Unit
    | Mesh  of mesh
    | Unop  of unop * t
    | Binop of binop * t * t

  exception Compile of t * string

  let mkhole m =
    Mesh m

  let mkunop op c =
    Unop (op, c)

  let mkbinop op cA cB =
    Binop (op, cA, cB)

  let string_of_unop = function
    | Hull ->
        "Hull"
    | Trans (x, y) ->
        Printf.sprintf "Trans(%s,%s)"
          (N.to_string x)
          (N.to_string y)
    | Home (x, y) ->
        Printf.sprintf "Home(%s,%s)"
          (N.to_string x)
          (N.to_string y)
    | Scale (x, y) ->
        Printf.sprintf "Scale(%s,%s)"
          (N.to_string x)
          (N.to_string y)
    | Fit (x, y) ->
        Printf.sprintf "Fit(%s,%s)"
          (N.to_string x)
          (N.to_string y)
    | Rotate deg ->
        Printf.sprintf "Rotate(%s)"
          (N.to_string deg)

  let string_of_binop = function
    | Union -> "Union"
    | Diff  -> "Diff"
    | Inter -> "Inter"

  let rec to_string_aux n = function
    | Empty ->
        [indent n "Empty"]
    | Unit ->
        [indent n "Unit"]
    | Mesh m ->
        let ms = M.to_string m in
        let ss = String.split_on_char '\n' ms in
        indent n ("Mesh" ^ " {") ::
        List.map (indent (n + 2)) ss @
        [indent n "}"]
    | Unop (op, c) ->
        indent n (string_of_unop op ^ " {") ::
        to_string_aux (n + 2) c @
        [indent n "}"]
    | Binop (op, cA, cB) ->
        indent n (string_of_binop op ^ " {") ::
        to_string_aux (n + 2) cA @
        to_string_aux (n + 2) cB @
        [indent n "}"]

  let to_string c =
    c |> to_string_aux 0
      |> String.concat "\n"
      |> (^) "CAD2\n\n"

  let rec first_diff cA cB =
      match cA, cB with
      | Empty, Empty ->
          None
      | Unit, Unit ->
          None
      | Mesh x, Mesh y ->
          if M.equiv x y
          then None
          else Some (cA, cB)
      | Unop (opA, xA), Unop(opB, xB) ->
          if opA = opB then
            first_diff xA xB
          else
            Some (cA, cB)
      | Binop (opA, xA, yA), Binop (opB, xB, yB) ->
          if opA = opB then
            begin match first_diff xA xB with
            | Some (l, r) -> Some (l, r)
            | None -> first_diff yA yB
            end
          else
            Some (cA, cB)
      | _, _ ->
          Some (cA, cB)

  let cmp_unop opA opB =
    match opA, opB with
    | Hull, Hull -> EQ
    | Hull, _    -> LT
    | _, Hull    -> GT
    | Trans dA, Trans dB ->
        Util.cmp_pairs N.equiv N.cmp dA dB
    | Trans _, _ -> LT
    | _, Trans _ -> GT
    | Home dA, Home dB ->
        Util.cmp_pairs N.equiv N.cmp dA dB
    | Home _, _ -> LT
    | _, Home _ -> GT
    | Scale dA, Scale dB ->
        Util.cmp_pairs N.equiv N.cmp dA dB
    | Scale _, _ -> LT
    | _, Scale _ -> GT
    | Fit dA, Fit dB ->
        Util.cmp_pairs N.equiv N.cmp dA dB
    | Fit _, _ -> LT
    | _, Fit _ -> GT
    | Rotate dA, Rotate dB ->
        N.cmp dA dB

  let cmp_binop opA opB =
    match opA, opB with
    | Union, Union -> EQ
    | Union, _     -> LT
    | _, Union     -> GT
    | Diff, Diff   -> EQ
    | Diff, _      -> LT
    | _, Diff      -> GT
    | Inter, Inter -> EQ

  let cmp cA cB =
    match first_diff cA cB with
    | None -> EQ
    | Some (Empty, _)  -> LT
    | Some (_, Empty)  -> GT
    | Some (Unit, _)   -> LT
    | Some (_, Unit)   -> GT
    | Some (Mesh m, _) -> LT
    | Some (_, Mesh m) -> GT
    | Some (Unop (opA, _), Unop (opB, _)) ->
        cmp_unop opA opB
    | Some (Unop _, _) -> LT
    | Some (_, Unop _) -> GT
    | Some (Binop (opA, _, _), Binop (opB, _, _)) ->
        cmp_binop opA opB

  (** [size] returns the number of nodes
    in a 2D CAD AST *)
  let rec size = function
    | Empty  -> 1
    | Unit   -> 1
    | Mesh _ -> 1
    | Unop  (op, c) -> 1 + size c
    | Binop (op, c1, c2) -> 1 + size c1 + size c2

  (** [height] returns the depth of a 2D CAD AST *)
  let rec height = function
    | Empty  -> 0
    | Unit   -> 0
    | Mesh _ -> 0
    | Unop  (op, c) -> 1 + height c
    | Binop (op, c1, c2) -> 1 + Pervasives.max (height c1)
                                               (height c2)
  (** [num_prims] returns the number of
    primitives in a 2D CAD AST *)
  let rec num_prims = function
    | Empty  -> 1
    | Unit   -> 1
    | Mesh _ -> 1
    | Unop  (op, c) -> num_prims c
    | Binop (op, c1, c2) -> num_prims c1 + num_prims c2

  let transl_unop = function
    | Hull       -> M.hull
    | Trans d    -> M.trans d
    | Home  p    -> M.home  p
    | Scale f    -> M.scale f
    | Fit   b    -> M.fit   b
    | Rotate deg -> M.rotate deg

  let transl_binop = function
    | Union -> M.union
    | Diff  -> M.diff
    | Inter -> M.inter

  (** 2D CAD compiler calling mesh functions *)
  let rec compile = function
    | Empty ->
        M.mesh []
    | Unit ->
        M.path
          [ (N.n0, N.n0)
          ; (N.n0, N.n1)
          ; (N.n1, N.n1)
          ; (N.n1, N.n0)
          ]
    | Mesh m ->
        m
    | Unop (op, c) ->
        transl_unop op
          (compile c)
    | Binop (op, cA, cB) ->
        transl_binop op
          (compile cA)
          (compile cB)

  (** [simplify] tries to remove nodes from the AST that
    do not have any effect on the mesh *)
  let rec simplify c =
    let m = compile c in
    match c with
    | Empty  -> Empty
    | Unit   -> Unit
    | Mesh m -> Mesh m
    | Unop (op, c) ->
        let sc = simplify c in
        if M.equiv m (compile sc) then
          sc
        else
          Unop (op, sc)
    | Binop (op, cA, cB) ->
        if m = compile Empty then
          Empty
        else
          let sA = simplify cA in
          let sB = simplify cB in
          if M.equiv m (compile sA) then
            sA
          else
          if M.equiv m (compile sB) then
            sB
          else
            Binop (op, sA, sB)

  let rec rand n =
    if n <= 0 then
      Unit
    else
      match Random.int 10 with
      | 0 -> Unit
      | 1 -> Unop  (Hull,  rand (n / 2))
      | 2 -> Unop  (Trans  (N.rand(), N.rand()), rand (n / 2))
      | 3 -> Unop  (Home   (N.rand(), N.rand()), rand (n / 2))
      | 4 -> Unop  (Scale  (N.rand(), N.rand()), rand (n / 2))
      | 6 -> Unop  (Fit    (N.rand(), N.rand()), rand (n / 2))
      | 5 -> Unop  (Rotate (N.of_int (Random.int 360)), rand (n / 2))
      | 7 -> Binop (Union, rand (n / 2), rand (n / 2))
      | 8 -> Binop (Diff,  rand (n / 2), rand (n / 2))
      | 9 -> Binop (Inter, rand (n / 2), rand (n / 2))
      | _ -> failwith "CAD2.rand: off the rails"

  (* DEBUG CODE *)

  let rec variations = function
    | Empty ->
        Stream.single Empty
    | Unit ->
        Stream.single Unit
    | Mesh m ->
        Stream.single (Mesh m)
    | Unop (op, c) ->
        let vs = variations c in
        let vs' =
          Stream.map (mkunop op) vs
        in
        Stream.app vs vs'
    | Binop (op, cA, cB) ->
        let vsA = variations cA in
        let vsB = variations cB in
        let vs' =
          Stream.map (uncurry @@ mkbinop op) @@
            Stream.xprod vsA vsB
        in
        Stream.app vsA (Stream.app vsB vs')

  let smaller_fail f c =
    let test c =
      match f c with
      | _ -> false
      | exception _ -> true
    in
    c |> variations
      |> Stream.uniq
      |> Stream.find test
      |> Util.obind (fun c' ->
          if cmp c c' = EQ
          then None
          else Some c')

  (* HACKY DEBUGGING CODE *)

  let canonicalize cad =
    let rec loop = function
      | Empty ->
          Empty
      | Unit ->
          Unit
      | Mesh m ->
          Mesh m
      | Unop (op, cA) ->
          Unop (op, loop cA)
      | Binop (Union, cA, cB) ->
          let cA' = loop cA in
          let cB' = loop cB in
          let (cL, cR) =
            if cmp cA' cB' = LT
            then (cA', cB')
            else (cB', cA')
          in
          Binop (Union, cL, cR)
      | Binop (Diff, cA, cB) ->
          Binop ( Diff
                , loop cA
                , loop cB)
      | Binop (Inter, cA, cB) ->
          let cA' = loop cA in
          let cB' = loop cB in
          let (cL, cR) =
            if cmp cA' cB' = LT
            then (cA', cB')
            else (cB', cA')
          in
          Binop (Inter, cL, cR)
    in
    loop cad

  let equiv cA cB =
    let ccA = canonicalize cA in
    let ccB = canonicalize cB in
    cmp ccA ccB = EQ

  let normalize cad =
    let rec step = function
      | Unop (Hull, Empty) ->
          Empty
      | Unop (Hull, Unit) ->
          Unit
      | Unop (Hull, c) ->
          Unop (Hull, step c)
      | Unop (Trans (x1, y1), Unop (Trans (x2, y2), c)) ->
          Unop (Trans (N.add x1 x2, N.add y1 y2), c)
      | Unop (Trans d1, c) ->
          Unop (Trans d1, step c)
      | Unop (Scale (x1, y1), Unop (Scale (x2, y2), c)) ->
          Unop (Scale (N.add x1 x2, N.add y1 y2), c)
      | Unop (Scale d1, c) ->
          Unop (Scale d1, step c)
      | Unop (Rotate d1, Unop (Rotate d2, c)) ->
          Unop (Rotate (N.add d1 d2), c)
      | Unop (Rotate d1, c) ->
          Unop (Rotate d1, step c)
      | Binop (Union, cA, Empty) ->
          cA
      | Binop (Union, Empty, cB) ->
          cB
      | Binop (Union, cA, cB) ->
          if equiv cA cB
          then cA
          else Binop (Union, step cA, step cB)
      | Binop (Diff, Empty, cB) ->
          Empty
      | Binop (Diff, cA, Empty) ->
          cA
      | Binop (Diff, cA, cB) ->
          if equiv cA cB
          then Empty
          else Binop (Diff, step cA, step cB)
      | Binop (Inter, cA, Empty) ->
          Empty
      | Binop (Inter, Empty, cB) ->
          Empty
      | Binop (Inter, cA, cB) ->
          if equiv cA cB
          then cA
          else Binop (Inter, step cA, step cB)
      (* no rewrite matches *)
      | c -> c
    in
    let rec loop c =
      let c' = canonicalize (step c) in
      if equiv c c'
      then c
      else loop c'
    in
    loop cad
end

module CAD2
  (N : NUM)
  (M : MESH2 with type num = N.t
              and type pt  = N.t * N.t)
  : (CAD2 with type num  = N.t
           and type mesh = M.t)
= RawCAD2(N)(M)


module type CAD3 = sig
  (** 3D CAD *)

  type num
  type mesh

  type unop =
    | Hull
    | Trans   of (num * num * num)
    | Home    of (num * num * num)
    | Scale   of (num * num * num)
    | Fit     of (num * num * num)
    | RotateX of num
    | RotateY of num
    | RotateZ of num

  type binop =
    | Union
    | Diff
    | Inter

  type t =
    | Empty
    | Unit
    | Sphere
    | Cylinder
    | Pentagon
    | Hexagon
    | Mesh  of mesh
    | Unop  of unop * t
    | Binop of binop * t * t

  val mkhole  : mesh  -> t
  val mkunop  : unop  -> t -> t
  val mkbinop : binop -> t -> t -> t
  val cmp     : t -> t -> cmp

  val compile   : t -> mesh
  val num_prims : t -> int
  val simplify  : t -> t
  val rand      : int -> t

  val to_string : t -> string
  val to_scad   : t -> string

end

module RawCAD3
  (N : NUM)
  (M : MESH3 with type num = N.t
              and type pt  = N.t * N.t * N.t)
= struct
  include MakeTaggedLogging(struct let tag = "CAD3" end)

  type num  = N.t
  type mesh = M.t

  type unop =
    | Hull
    | Trans   of (num * num * num)
    | Home    of (num * num * num)
    | Scale   of (num * num * num)
    | Fit     of (num * num * num)
    | RotateX of num
    | RotateY of num
    | RotateZ of num

  type binop =
    | Union
    | Diff
    | Inter

  type t =
    | Empty
    | Unit
    | Sphere
    | Cylinder
    | Pentagon
    | Hexagon
    | Mesh  of mesh
    | Unop  of unop * t
    | Binop of binop * t * t

  let mkhole m =
    Mesh m

  let mkunop op c =
    Unop (op, c)

  let mkbinop op cA cB =
    Binop (op, cA, cB)

  let string_of_unopV op (a, b, c) =
    Printf.sprintf "%s(%s, %s, %s)" op
      (N.to_string a)
      (N.to_string b)
      (N.to_string c)

  let string_of_unopN op n =
    Printf.sprintf "%s(%s)" op
      (N.to_string n)

  let string_of_unop = function
    | Hull      -> "Hull"
    | Trans   v -> string_of_unopV "Trans"   v
    | Home    v -> string_of_unopV "Home"    v
    | Scale   v -> string_of_unopV "Scale"   v
    | Fit     v -> string_of_unopV "Fit"     v
    | RotateX n -> string_of_unopN "RotateX" n
    | RotateY n -> string_of_unopN "RotateY" n
    | RotateZ n -> string_of_unopN "RotateZ" n

  let string_of_binop = function
    | Union -> "Union"
    | Diff  -> "Diff"
    | Inter -> "Inter"

  let rec summary_string = function
    | Empty    -> "Empty"
    | Unit     -> "Unit"
    | Sphere   -> "Sphere"
    | Cylinder -> "Cylinder"
    | Pentagon -> "Pentagon"
    | Hexagon  -> "Hexagon"
    | Mesh _   -> "MESH"
    | Unop (op, a) ->
        Printf.sprintf "%s { %s }"
          (string_of_unop op)
          (summary_string a)
    | Binop (op, a, b) ->
        Printf.sprintf "%s { %s %s }"
          (string_of_binop op)
          (summary_string a)
          (summary_string b)

  let to_string c =
    let rec loop n = function
      | Empty ->
          [indent n "Empty"]
      | Unit ->
          [indent n "Unit"]
      | Sphere ->
          [indent n "Sphere"]
      | Cylinder ->
          [indent n "Cylinder"]
      | Pentagon ->
          [indent n "Pentagon"]
      | Hexagon ->
          [indent n "Hexagon"]
      | Mesh m ->
          [ indent n "Mesh {"
          ; indents (n + 2) (M.to_string m)
          ; indent n "}"]
      | Unop (op, a) ->
          indent n (string_of_unop op ^ " {") ::
          loop (n + 2) a @
          [indent n "}"]
      | Binop (op, a, b) ->
          indent n (string_of_binop op ^ " {") ::
          loop (n + 2) a @
          loop (n + 2) b @
          [indent n "}"]
    in
    c |> loop 0
      |> String.concat "\n"
      |> (^) "CAD3\n\n"
      |> flip (^) "\n"

  let scad_unopv op (a, b, c) =
    Printf.sprintf "%s([%s, %s, %s])" op
      (N.to_string a)
      (N.to_string b)
      (N.to_string c)

  let scad_unop = function
    | Hull      -> "hull()"
    | Trans   v -> scad_unopv "translate" v
    | Home    v -> failwith "CAD3.scad_unop: Home unsupported"
    | Scale   v -> scad_unopv "scale" v
    | Fit     v -> scad_unopv "resize" v
    | RotateX n -> scad_unopv "rotate" (n, N.n0, N.n0)
    | RotateY n -> scad_unopv "rotate" (N.n0, n, N.n0)
    | RotateZ n -> scad_unopv "rotate" (N.n0, N.n0, n)

  let scad_binop = function
    | Union -> "union()"
    | Diff  -> "difference()"
    | Inter -> "intersection()"

  let to_scad c =
    let rec loop n = function
      | Empty ->
          [indent n "/* EMPTY */ sphere(r = 0);"]
      | Unit ->
          [indent n "cube([1, 1, 1]);"]
      (* TODO: fragments are hardcoded for now. *)
      (* dimensions are all unit *)
      | Sphere ->
          [indent n "sphere($fn = 35);"]
      | Cylinder ->
          [indent n "cylinder($fn = 50, center = true, r = 0.5);"]
      | Hexagon ->
          [indent n "scale([0.5, 0.577, 1]) cylinder($fn = 6, center = true);"]
      | Pentagon ->
          [indent n "rotate([0, 0, 18]) cylinder ();"]
      | Mesh m ->
          let h =
            Util.with_tmp' "openscad" ".stl" (fun stlP ->
              Util.to_file stlP (M.to_stl m);
              Printf.sprintf "import(%s);" stlP)
          in
          [indent n h]
      | Unop (op, a) ->
          indent n (scad_unop op ^ " {") ::
          loop (n + 2) a @
          [indent n "}"]
      | Binop (op, a, b) ->
          indent n (scad_binop op ^ " {") ::
          loop (n + 2) a @
          loop (n + 2) b @
          [indent n "}"]
    in
    c |> loop 0
      |> String.concat "\n"

  (* TODO: add other prim cases *)
  let rec first_diff cA cB =
      match cA, cB with
      | Empty, Empty ->
          None
      | Unit, Unit ->
          None
      | Sphere, Sphere ->
          None
      | Cylinder, Cylinder ->
          None
      | Hexagon, Hexagon ->
          None
      | Pentagon, Pentagon ->
          None
      | Mesh x, Mesh y ->
          if M.equiv x y
          then None
          else Some (cA, cB)
      | Unop (opA, xA), Unop(opB, xB) ->
          if opA = opB then
            first_diff xA xB
          else
            Some (cA, cB)
      | Binop (opA, xA, yA), Binop (opB, xB, yB) ->
          if opA = opB then
            begin match first_diff xA xB with
            | Some (l, r) -> Some (l, r)
            | None -> first_diff yA yB
            end
          else
            Some (cA, cB)
      | _, _ ->
          Some (cA, cB)

  let cmp_unop opA opB =
    match opA, opB with
    | Hull, Hull -> EQ
    | Hull, _    -> LT
    | _, Hull    -> GT
    | Trans dA, Trans dB ->
        Util.cmp_triples N.equiv N.cmp dA dB
    | Trans _, _ -> LT
    | _, Trans _ -> GT
    | Home dA, Home dB ->
        Util.cmp_triples N.equiv N.cmp dA dB
    | Home _, _ -> LT
    | _, Home _ -> GT
    | Scale dA, Scale dB ->
        Util.cmp_triples N.equiv N.cmp dA dB
    | Scale _, _ -> LT
    | _, Scale _ -> GT
    | Fit dA, Fit dB ->
        Util.cmp_triples N.equiv N.cmp dA dB
    | Fit _, _ -> LT
    | _, Fit _ -> GT
    | RotateX dA, RotateX dB ->
        N.cmp dA dB
    | RotateX _, _ -> LT
    | _, RotateX _ -> GT
    | RotateY dA, RotateY dB ->
        N.cmp dA dB
    | RotateY _, _ -> LT
    | _, RotateY _ -> GT
    | RotateZ dA, RotateZ dB ->
        N.cmp dA dB

  let cmp_binop opA opB =
    match opA, opB with
    | Union, Union -> EQ
    | Union, _     -> LT
    | _, Union     -> GT
    | Diff, Diff   -> EQ
    | Diff, _      -> LT
    | _, Diff      -> GT
    | Inter, Inter -> EQ

  (* TODO: add other prim cases *)
  let cmp cA cB =
    match first_diff cA cB with
    | None -> EQ
    | Some (Empty, _)    -> LT
    | Some (_, Empty)    -> GT
    | Some (Unit, _)     -> LT
    | Some (_, Unit)     -> GT
    | Some (Sphere, _)   -> LT
    | Some (_, Sphere)   -> GT
    | Some (Cylinder, _) -> LT
    | Some (_, Cylinder) -> GT
    | Some (Hexagon, _)  -> LT
    | Some (_, Hexagon)  -> GT
    | Some (Pentagon, _) -> LT
    | Some (_, Pentagon) -> GT
    | Some (Mesh m, _) -> LT
    | Some (_, Mesh m) -> GT
    | Some (Unop (opA, _), Unop (opB, _)) ->
        cmp_unop opA opB
    | Some (Unop _, _) -> LT
    | Some (_, Unop _) -> GT
    | Some (Binop (opA, _, _), Binop (opB, _, _)) ->
        cmp_binop opA opB


  (** [size] returns the number of nodes
    in a 3D CAD AST *)
  let rec size = function
    | Empty    -> 1
    | Unit     -> 1
    | Sphere   -> 1
    | Cylinder -> 1
    | Hexagon  -> 1
    | Pentagon -> 1
    | Mesh _   -> 1
    | Unop  (op, c) -> 1 + size c
    | Binop (op, c1, c2) -> 1 + size c1 + size c2

  (** [height] returns the depth of a 3D CAD AST *)
  let rec height = function
    | Empty    -> 0
    | Unit     -> 0
    | Sphere   -> 0
    | Cylinder -> 0
    | Hexagon  -> 0
    | Pentagon -> 0
    | Mesh _   -> 0
    | Unop  (op, c) -> 1 + height c
    | Binop (op, c1, c2) -> 1 + Pervasives.max (height c1)
                                               (height c2)
  (** [num_prims] returns the number of
    primitives in a 3D CAD AST *)
  let rec num_prims = function
    | Empty    -> 1
    | Unit     -> 1
    | Sphere   -> 1
    | Cylinder -> 1
    | Hexagon  -> 1
    | Pentagon -> 1
    | Mesh _   -> 1
    | Unop  (op, c) -> num_prims c
    | Binop (op, c1, c2) -> num_prims c1 + num_prims c2


  let transl_unop = function
    | Hull        -> M.hull
    | Trans d     -> M.trans d
    | Home  p     -> M.home  p
    | Scale f     -> M.scale f
    | Fit   b     -> M.fit   b
    | RotateX deg -> M.rotateX deg
    | RotateY deg -> M.rotateY deg
    | RotateZ deg -> M.rotateZ deg

  let transl_binop = function
    | Union -> M.union
    | Diff  -> M.diff
    | Inter -> M.inter

  (** 3D CAD compiler calling mesh functions *)
  let compile c =
    let rec loop d c =
      logd "compile" d (summary_string c);
      begin match c with
      | Empty ->
          M.mesh []
      | Unit ->
          let num_of_int i =
            N.of_string (string_of_int i) in
          (* bottom *)
          [ ((0, 0, 0), (1, 0, 0), (1, 1, 0))
          ; ((0, 0, 0), (1, 1, 0), (0, 1, 0))
          (* top *)
          ; ((0, 0, 1), (1, 0, 1), (1, 1, 1))
          ; ((0, 0, 1), (1, 1, 1), (0, 1, 1))
          (* front *)
          ; ((0, 0, 0), (1, 0, 0), (1, 0, 1))
          ; ((0, 0, 0), (1, 0, 1), (0, 0, 1))
          (* back *)
          ; ((0, 1, 0), (1, 1, 0), (1, 1, 1))
          ; ((0, 1, 0), (1, 1, 1), (0, 1, 1))
          (* left *)
          ; ((0, 0, 0), (0, 1, 0), (0, 1, 1))
          ; ((0, 0, 0), (0, 1, 1), (0, 0, 1))
          (* right *)
          ; ((1, 0, 0), (1, 1, 0), (1, 1, 1))
          ; ((1, 0, 0), (1, 1, 1), (1, 0, 1)) ]
            |> List.map
                (Util.triple_map (Util.triple_map num_of_int))
            |> M.mesh
      | Mesh m -> m
      | Sphere ->
          let num_of_float i =
            N.of_string (string_of_float i) in
          List.find
            (fun (n, m) -> n = "Sphere") Primitives.prims3
            |> snd
            |> List.map
                (Util.triple_map (Util.triple_map num_of_float))
            |> M.mesh
      | Cylinder ->
          let num_of_float i =
            N.of_string (string_of_float i) in
          List.find
            (fun (n, m) -> n = "Cylinder") Primitives.prims3
            |> snd
            |> List.map
                (Util.triple_map (Util.triple_map num_of_float))
            |> M.mesh
      | Pentagon ->
          let num_of_float i =
            N.of_string (string_of_float i) in
          List.find
            (fun (n, m) -> n = "Pentagon") Primitives.prims3
            |> snd
            |> List.map
                (Util.triple_map (Util.triple_map num_of_float))
            |> M.mesh
      | Hexagon ->
          let num_of_float i =
            N.of_string (string_of_float i) in
          List.find
            (fun (n, m) -> n = "Hexagon") Primitives.prims3
            |> snd
            |> List.map
                (Util.triple_map (Util.triple_map num_of_float))
            |> M.mesh
      | Unop (op, c) ->
          transl_unop op
            (loop (d + 1) c)
      | Binop (op, cA, cB) ->
          transl_binop op
            (loop (d + 1) cA)
            (loop (d + 1) cB)
      end
      >> (fun m ->
        logd "compile" d
          ("done, mesh size " ^ string_of_int (M.nfaces m)))
    in begin
      log ("compiling\n" ^ to_string c);
      loop 0 c
      >> (fun m ->
        log ("compiled, mesh size " ^ string_of_int (M.nfaces m)))
    end

  (** [simplify] tries to remove nodes from the AST that
    do not have any effect on the mesh *)
  let rec simplify c =
    let m = compile c in
    match c with
    | Empty    -> Empty
    | Unit     -> Unit
    | Sphere   -> Sphere
    | Cylinder -> Cylinder
    | Hexagon  -> Hexagon
    | Pentagon -> Pentagon
    | Mesh m   -> Mesh m
    | Unop (op, c) ->
        let sc = simplify c in
        if M.equiv m (compile sc) then
          sc
        else
          Unop (op, sc)
    | Binop (op, cA, cB) ->
        if m = compile Empty then
          Empty
        else
          let sA = simplify cA in
          let sB = simplify cB in
          if M.equiv m (compile sA) then
            sA
          else
          if M.equiv m (compile sB) then
            sB
          else
            Binop (op, sA, sB)

(* TODO: add other primitives *)
  let rec rand n =
    if n <= 0 then
      Unit
    else
      match Random.int 11 with
      |  0 -> Unit
      |  1 -> Unop  (Trans   (N.rand (), N.rand(), N.rand()), rand (n / 2))
      |  2 -> Unop  (Home    (N.rand (), N.rand(), N.rand()), rand (n / 2))
      |  3 -> Unop  (Scale   (N.rand (), N.rand(), N.rand()), rand (n / 2))
      |  7 -> Unop  (Fit     (N.rand (), N.rand(), N.rand()), rand (n / 2))
      |  4 -> Unop  (RotateX (N.of_int (Random.int 360)), rand (n / 2))
      |  5 -> Unop  (RotateY (N.of_int (Random.int 360)), rand (n / 2))
      |  6 -> Unop  (RotateZ (N.of_int (Random.int 360)), rand (n / 2))
      |  8 -> Binop (Union,  rand (n / 2), rand (n / 2))
      |  9 -> Binop (Diff,   rand (n / 2), rand (n / 2))
      | 10 -> Binop (Inter,  rand (n / 2), rand (n / 2))
      | 11 -> Unop  (Hull,   rand (n / 2))
      | _ -> failwith "CAD3.rand: off the rails"

end

module CAD3
  (N : NUM)
  (M : MESH3 with type num = N.t
              and type pt  = N.t * N.t * N.t)
  : (CAD3 with type num  = N.t
           and type mesh = M.t)
= RawCAD3(N)(M)


