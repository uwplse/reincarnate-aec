open Util
open RStream
open NumSys
open Geom
open LinAlg

module type MESH1 = sig

  type num
  type pt
  type t

  exception OddFaces    of string
  exception RepeatFaces of string

  type region =
    | Outside
    | Boundary
    | Inside

  val mesh      : pt list -> t
  val faces     : t -> pt list
  val denote    : t -> pt -> region
  val hull      : t -> t
  val trans     : num -> t -> t
  val scale     : num -> t -> t
  val union     : t -> t -> t
  val diff      : t -> t -> t
  val inter     : t -> t -> t

  val to_string : t -> string
  val equiv     : t -> t -> bool

end

module RawMesh1 (N : NUM) (G : GEOM1 with type num = N.t) = struct

  type num = N.t
  exception OddFaces    of string
  exception RepeatFaces of string

  (* TODO : use Geom point types in meshes *)
  type pt = num

  type face =
    Face of G.pt

  let mkface pt =
    Face pt

  let pt_of_face = function Face pt ->
    pt

  let string_of_face f =
    let aux p =
      let sp = String.split_on_char '}'
                (G.string_of_pt p)
      in
      let x =
        sp |> flip List.nth 0
           |> String.split_on_char '='
           |> flip List.nth 1
           |> String.trim
      in
      Printf.sprintf "%s" x
    in
    aux (pt_of_face f)

  let equiv_face f1 f2 =
    let p1 = pt_of_face f1 in
    let p2 = pt_of_face f2 in
    G.equiv_pt p1 p2

  (* lifting some Geom1 functionality *)
  let lift_ptop_face op f =
    f |> pt_of_face
      |> op
      |> mkface

  type t =
    Mesh of face list

  type region =
    | Outside
    | Boundary
    | Inside

  let raise_of msg = raise (OddFaces msg)
  let raise_rf msg = raise (RepeatFaces msg)

  let faces_of_mesh = function Mesh fs ->
    fs

  let faces m =
    let x p = p.G.x in
    m |> faces_of_mesh
      |> List.map pt_of_face
      |> List.map x

  (** check invariants and make mesh*)
  let mkmesh fs =
    let repeat_face fs =
      fs |> Util.choose2
         |> List.find (Util.uncurry equiv_face)
    in
    let odd_face fs =
      List.length fs mod 2 <> 0
    in
    try
      fs |> repeat_face
         |> Util.pair_map string_of_face
         |> Util.uncurry (Printf.sprintf "(%s, %s)")
         |> raise_rf
    with Not_found ->
      if odd_face fs then
        raise_of "Odd number faces"
      else
        Mesh fs

  let mesh ps =
    ps |> List.map G.mkpt
       |> List.map mkface
       |> mkmesh

  let to_string m =
    m |> faces_of_mesh
      |> List.map string_of_face
      |> String.concat "\n; "
      |> Printf.sprintf "[ %s\n]"

  let equiv m1 m2 =
    let f1s = faces_of_mesh m1 in
    let f2s = faces_of_mesh m2 in
    if List.length f1s <> List.length f2s then
      false
    else
      let p1s = List.map pt_of_face f1s in
      let p2s = List.map pt_of_face f2s in
      List.for_all2 G.equiv_pt
        (sort G.cmp_pt p1s)
        (sort G.cmp_pt p2s)

  (* val part : t -> pt -> pt list * pt list * pt list *)
  (* Partition the given mesh wrt the given point, returning lists of
     faces that are left of, equal to, and right of the point. *)
  let part m pt =
    let fs = faces_of_mesh m in
    let ps = List.map pt_of_face fs in
    List.fold_left
      (fun (lt, on, rt) face ->
        match G.cmp_pt face pt with
        | LT -> (face :: lt, on, rt)
        | EQ -> (lt, face :: on, rt)
        | GT -> (lt, on, face :: rt))
      ([], [], [])
      ps

  type mem =
    | Out
    | In
    | On of face

  (** where does [pt] lie w.r.t. [m]? *)
  let denote' m pt =
    match part m pt with
    | (lt, [], rt) ->
        let parityL = List.length lt mod 2 in
        let parityR = List.length rt mod 2 in
        if parityL <> parityR then
          failwith (
            Printf.sprintf "Mesh1.denote: parity mismatch around %s\n%s\n"
              (G.string_of_pt pt)
              (to_string m))
        else if parityL = 0 then
          Out
        else
          In
    | (_, [f], _) ->
        On (Face f)
    | (_, fs, _) ->
        failwith (
          Printf.sprintf "Mesh1.denote: repeat faces %s\n%s\n"
            (fs |> List.map G.string_of_pt
                |> String.concat ";"
            )
            (to_string m))

  let denote m pt =
    match denote' m (G.mkpt pt) with
    | In    -> Inside
    | On fs -> Boundary
    | Out   -> Outside

  (* normals point toward outside *)
  type norm = L | R

  let norm m pt =
    match part m pt with
    | (_, [], _) ->
        failwith (
          Printf.sprintf "Mesh1.norm: %s not a face\n%s\n"
            (G.string_of_pt pt)
            (to_string m))
    | (lt, [_], rt) ->
        let parityL = List.length lt mod 2 in
        let parityR = List.length rt mod 2 in
        begin match parityL, parityR with
        | 0, 1 -> L
        | 1, 0 -> R
        | _, _ ->
          failwith (
            Printf.sprintf "Mesh1.norm: parity mismatch around %s\n%s\n"
              (G.string_of_pt pt)
              (to_string m))
        end
    | (_, fs, _) ->
        failwith (
          Printf.sprintf "Mesh1.norm: repeat faces %s\n%s\n"
            (fs |> List.map G.string_of_pt
                |> String.concat ";"
            )
            (to_string m))

  let lift_ptop_mesh op m =
    m |> faces_of_mesh
      |> List.map (lift_ptop_face op)
      |> mkmesh

  let trans delta =
    let d = G.mkpt delta in
    lift_ptop_mesh (G.add d)

  let scale factor m =
    let fct = G.mkpt factor in
    if G.cmp_pt fct (G.mkpt N.n0) = EQ then
      mkmesh []
    else
      lift_ptop_mesh (G.mul fct) m

  let hull m =
    let fs = faces_of_mesh m in
    if fs = [] then
      mkmesh []
    else
      let ps = List.map pt_of_face fs
      in
      mkmesh [ mkface (minl G.cmp_pt ps)
             ; mkface (maxl G.cmp_pt ps)
             ]

  let union mA mB =
    let fAs = faces_of_mesh mA in
    let fBs = faces_of_mesh mB in
    let fAs' =
      List.filter
        (fun fA ->
          let pA = pt_of_face fA in
          match denote' mB pA with
          | Out   -> true
          | On fB -> norm mA pA = norm mB (pt_of_face fB)
          | In    -> false)
        fAs
    in
    let fBs' =
      List.filter
        (fun fB ->
          denote' mA (pt_of_face fB) = Out
        ) fBs
    in
    mkmesh (fAs' @ fBs')

  let diff mA mB =
    let fAs = faces_of_mesh mA in
    let fBs = faces_of_mesh mB in
    let fAs' =
      List.filter
        (fun fA ->
          let pA = pt_of_face fA in
          match denote' mB pA with
          | Out   -> true
          | On fB -> norm mA pA <> norm mB (pt_of_face fB)
          | In    -> false)
        fAs
    in
    let fBs' =
      List.filter
        (fun fB ->
          denote' mA (pt_of_face fB) = In
        ) fBs
    in
    mkmesh (fAs' @ fBs')

  let inter mA mB =
    let fAs = faces_of_mesh mA in
    let fBs = faces_of_mesh mB in
    let fAs' =
      List.filter
        (fun fA ->
          let pA = pt_of_face fA in
          match denote' mB pA with
          | Out   -> false
          | On fB -> norm mA pA = norm mB (pt_of_face fB)
          | In    -> true)
        fAs
    in
    let fBs' =
      List.filter
        (fun fB ->
          denote' mA (pt_of_face fB) = In
        ) fBs
    in
    mkmesh (fAs' @ fBs')

end

module Mesh1 (N : NUM) (G : GEOM1 with type num = N.t)
  : (MESH1 with type num = N.t
            and type pt  = N.t)
  = RawMesh1(N)(G)

module type MESH2 = sig

  type num
  type pt
  type t

  type region =
    | Outside
    | Boundary
    | Inside

  exception Bogus
  exception EmptyFace  of string
  exception RepeatFace of string
  exception OddVertex  of string
  exception NeedsSplit of string

  val mesh     : (pt * pt) list -> t
  val path     : pt list -> t
  val faces    : t -> (pt * pt) list
  val verts    : t -> pt list
  val area     : t -> num
  val denote   : t -> pt -> region
  val contains : t -> t  -> bool
  val splits   : (t * t) -> (t * t)

  val cycles : t -> pt list list
  val hull   : t -> t
  val trans  : num * num -> t -> t
  val home   : num * num -> t -> t
  val scale  : num * num -> t -> t
  val fit    : num * num -> t -> t
  val rotate : num -> t -> t
  val union  : t -> t -> t
  val diff   : t -> t -> t
  val inter  : t -> t -> t

  val to_string : t -> string
  val to_tikz   : t -> string
  val equiv     : t -> t -> bool

end

module RawMesh2 (N : NUM) (G : GEOM2 with type num = N.t) = struct

  type num = N.t
  type pt  = N.t * N.t

  exception Bogus
  exception EmptyFace  of string
  exception RepeatFace of string
  exception OddVertex  of string
  exception NeedsSplit of string

  let string_of_point (x, y) =
    Printf.sprintf "(%s, %s)"
      (N.to_string x)
      (N.to_string y)

  (** faces *)
  type face =
    Face of G.dseg

  let mkface s =
    if G.empty_dseg s then
      raise (EmptyFace (G.string_of_dseg s))
    else if G.cmp_pt s.G.head s.G.tail = GT then
      (* reorder to canonicalize *)
      Face (G.mkdseg s.G.tail s.G.head)
    else
      Face s

  let dseg_of_face = function Face s ->
    s

  let verts_of_faces fs =
    fs |> List.map dseg_of_face
       |> G.pts_of_dsegs

  let string_of_face f =
    f |> dseg_of_face
      |> G.pts_of_dseg
      |> Util.pair_map (fun p -> (p.G.x, p.G.y))
      |> Util.pair_map string_of_point
      |> Util.uncurry @@ Printf.sprintf "(%s, %s)"

  let equiv_face f1 f2 =
    let d1 = dseg_of_face f1 in
    let d2 = dseg_of_face f2 in
    G.equiv_dseg d1 d2

  (* lifting some Geom2 functionality *)
  let lift_ptop_face op f =
    f |> dseg_of_face
      |> G.lift_ptop_dseg op
      |> mkface

  let on_face f pt =
    f |> dseg_of_face
      |> Util.flip G.on_dseg pt

  (** meshes *)

  type t =
    Mesh of face list

  type region =
    | Outside
    | Boundary
    | Inside

  (* constructors are not functions :\ *)
  let raise_rf msg = raise (RepeatFace msg)
  let raise_ov msg = raise (OddVertex  msg)
  let raise_ns msg = raise (NeedsSplit msg)

  let faces_of_mesh = function Mesh fs ->
    fs

  let faces m =
    let aux (a, b) =
      ((a.G.x, a.G.y), (b.G.x, b.G.y))
    in
    m |> faces_of_mesh
      |> List.map dseg_of_face
      |> List.map G.pts_of_dseg
      |> List.map aux

  let verts_of_mesh m =
    m |> faces_of_mesh
      |> verts_of_faces

  let verts m =
    m |> verts_of_mesh
      |> List.map (fun p -> (p.G.x, p.G.y))

  (** check invariants and make mesh*)
  let mkmesh fs =
    let repeat_face fs =
      fs |> Util.choose2
         |> List.find (Util.uncurry equiv_face)
    in
    let vert_odd fs =
      let end_pt v f =
        let s = dseg_of_face f in
        G.equiv_pt v s.G.head ||
        G.equiv_pt v s.G.tail
      in
      let odd_vert v =
        fs |> List.filter (end_pt v)
           |> List.length
           |> Util.of_parity false true
      in
      fs |> verts_of_faces
         |> List.find odd_vert
    in
    let needs_split fs =
      let splittable (sa, sb) =
        match
          G.part_dseg sb sa,
          G.part_dseg sa sb
        with
        | [_], [_] -> false
        |  _ ,  _  -> true
      in
      fs |> List.map dseg_of_face
         |> Util.choose2
         |> List.find splittable
    in
    try
      fs |> repeat_face
         |> Util.pair_map string_of_face
         |> Util.uncurry (Printf.sprintf "(%s, %s)")
         |> raise_rf
    with Not_found -> try
      fs |> vert_odd
         |> G.string_of_pt
         |> raise_ov
    with Not_found -> try
      fs |> needs_split
         |> Util.pair_map G.string_of_dseg
         |> Util.uncurry (Printf.sprintf "(%s, %s)")
         |> raise_ns
    with Not_found ->
      Mesh fs

  (** create a 2D mesh from a list of pairs of points *)
  let mesh pps =
    let aux ((ax, ay), (bx, by)) =
      G.mkdseg
        (G.mkpt ax ay)
        (G.mkpt bx by)
    in
    pps |> List.map aux
        |> List.map mkface
        |> mkmesh

  (** make a mesh by first creating a path
    between a list of points, [pts] *)
  let path pts =
    pts |> List.map (Util.uncurry G.mkpt)
        |> G.dseg_circuit
        |> List.map mkface
        |> mkmesh

  let to_string m =
    m |> faces_of_mesh
      |> List.map string_of_face
      |> String.concat "\n; "
      |> Printf.sprintf "[ %s\n]"

  let equiv m1 m2 =
    let f1s = faces_of_mesh m1 in
    let f2s = faces_of_mesh m2 in
    if List.length f1s <> List.length f2s then
      false
    else
      (* TODO sort? *)
      List.for_all
        (fun x -> List.exists (equiv_face x) f2s)
        f1s

  type mem =
    | Out
    | On of face list
    | In

  (** does any point lie on the line
    through [p1] and p[2] *)
  let thru_vert p1 p2 vs =
    List.exists
      (G.on_line (G.mkline p1 p2))
      vs

  let is_vert' vs pt =
    List.exists (G.equiv_pt pt) vs

  let is_vert m pt =
    is_vert' (verts_of_mesh m) pt

  (** pick a destination point for a ray
    from [pt] to pass though, so that the ray
    satisfies some invariants *)
  let dest_pt m pt =
    match verts_of_mesh m with
    | [] ->
        G.add pt (G.mkpt N.n1 N.n1)
    | vs ->
        if is_vert' vs pt then
          failwith "Mesh2.dest_pt: pt is on mesh"
        else
          let bad_dest dpt =
            is_vert' vs dpt ||
            G.equiv_pt pt dpt ||
            thru_vert pt dpt vs
          in
          vs |> G.bbox
             |> G.around_bbox
             |> Stream.find (Util.notp bad_dest)
             |> Util.valOf

  (**
    [directed_isects m a b] returns all faces of [m] that
    are intersected by the ray starting at [a] and going
    towards [b].

    NOTE does not include any intersection at [a]!
  *)
  let directed_isects m a b =
    let r = G.mkray (G.mkdseg a b) in
    let aux f =
      let s = dseg_of_face f in
      if G.equiv_pt a (G.midpt s) then
        (* TODO: do we still need this after bbox based dest_pt? *)
        (* NOTE hack due to numerical issues *)
        false
      else
        match G.isect_rs r s with
        | G.SNone    -> false
        | G.SOver  _ -> failwith "Mesh2.directed_isects: bogus"
        | G.SCross _ -> true
    in
    m |> faces_of_mesh
      |> List.filter aux

  (** where does [pt] lie w.r.t. [m]? *)
  let denote' m pt =
    let on_fs =
      m |> faces_of_mesh
        |> List.filter (Util.flip on_face pt)
    in
    if on_fs <> [] then
      On on_fs
    else
      pt |> dest_pt m
         |> directed_isects m pt
         |> List.length
         |> Util.of_parity Out In

  let denote m (x, y) =
    match denote' m (G.mkpt x y) with
    | In    -> Inside
    | On fs -> Boundary
    | Out   -> Outside

  (** [contains] checks whether m2 is inside m1 *)
  let contains m1 m2 =
    (* all points of [m2] should be inside [m1] *)
    m2 |> verts_of_mesh
       |> List.map (fun v -> (v.G.x, v.G.y))
       |> List.for_all (fun v ->
                        denote m1 v = Inside ||
                        denote m1 v = Boundary
                       )

  let lift_ptop_mesh op m =
    m |> faces_of_mesh
      |> List.map (lift_ptop_face op)
      |> mkmesh

  let trans delta =
    let d = Util.uncurry G.mkpt delta in
    lift_ptop_mesh (G.add d)

  let home (px, py) m =
    let bb =
      m |> verts_of_mesh
        |> G.bbox
    in
    let dx = N.sub bb.G.xmax bb.G.xmin in
    let dy = N.sub bb.G.ymax bb.G.ymin in
    let ax = N.add bb.G.xmin (N.mul px dx) in
    let ay = N.add bb.G.ymin (N.mul py dy) in
    trans (N.neg ax, N.neg ay) m

  let scale (fx, fy) m =
    if N.cmp fx N.n0 = EQ
    || N.cmp fy N.n0 = EQ then
      mkmesh []
    else
      let factor = G.mkpt fx fy in
      lift_ptop_mesh (G.mul factor) m

  let fit (dx', dy') m =
    let bb =
      m |> verts_of_mesh
        |> G.bbox
    in
    let dx = N.sub bb.G.xmax bb.G.xmin in
    let dy = N.sub bb.G.ymax bb.G.ymin in
    let fx = N.div dx' dx in
    let fy = N.div dy' dy in
    scale (fx, fy) m

  let rotate deg =
    lift_ptop_mesh (G.rotate deg)

  let rec modfst e m =
    match m with
    | [] -> failwith "modfst: did not find elt"
    | f :: fs ->
        if equiv_face f e
        then fs
        else f :: modfst e fs

  let rec cycle tgt path m =
    match path with
    | [] -> failwith "cycle: empty path"
    | x :: xs ->
        if G.equiv_pt x tgt then
          (path, m)
        else if List.exists (G.equiv_pt x) xs then
          raise Bogus
        else
          let rec loop = function
            | [] -> raise Bogus
            | f :: m' ->
                let (a, b) = G.pts_of_dseg (dseg_of_face f) in
                if G.equiv_pt a x then
                  try cycle tgt (b :: path) (modfst f m)
                  with Bogus -> loop m'
                else if G.equiv_pt b x then
                  try cycle tgt (a :: path) (modfst f m)
                  with Bogus -> loop m'
                else
                  loop m'
          in loop m

  let rec cycles m =
    let faces = faces_of_mesh m in
    match faces with
    | [] -> []
    | f :: fs ->
        let (a, b) = G.pts_of_dseg (dseg_of_face f) in
        let (c', es') = cycle a [b] fs in
        let c = List.map (fun p -> (p.G.x, p.G.y)) c' in
        c :: cycles (mkmesh es')

  let string_of_cycle c =
    c |> List.map (Util.uncurry G.mkpt)
      |> List.map G.string_of_pt
      |> String.concat "\n; "
      |> Printf.sprintf "[ %s\n]"

  let string_of_cycles cs =
    cs |> List.map string_of_cycle
       |> String.concat "\n\n"

  let area m =
    m |> cycles
      |> List.map
          (fun x ->
            List.map (fun p -> Util.uncurry G.mkpt p) x)
      |> List.map G.mkpgon
      |> List.fold_left
          (fun acc x -> N.add acc (G.area x))
          N.n0

  let hull m =
    m |> verts_of_mesh
      |> G.hull
      |> G.pgon_of_hull
      |> G.dsegs_of_pgon
      |> List.map mkface
      |> mkmesh

  let face_splits m f =
    let mss =
      m |> faces_of_mesh
        |> List.map dseg_of_face
    in
    f |> dseg_of_face
      |> G.part_dsegs mss
      |> List.map mkface

  (* split mB with respect to mA *)
  let splits' mA mB =
    mB |> faces_of_mesh
       |> Util.flatmap (face_splits mA)
       |> mkmesh

  (* split two meshes with respect to one another *)
  let split_aux (mA, mB) =
    ( splits' mB mA
    , splits' mA mB )

  let rec splits (mA, mB) =
    let (mA', mB') = split_aux (mA, mB) in
    if equiv mA mA' && equiv mB mB'
    then
      (mA, mB)
    else
      splits (mA', mB')

  (* membership of a face after splitting *)
  (* only checking mid point suffices *)
  let face_mem m f =
    denote' m (G.midpt (dseg_of_face f))

  type norm = L | R

  let norm m f =
    let s   = dseg_of_face f in
    let mid = G.midpt s in
    let rpt = dest_pt m mid in
    let trn = G.turn s rpt in
    let toward_out =
      rpt |> directed_isects m mid
          |> List.length
          |> Util.of_parity true false
    in
    match trn, toward_out with
    | G.CLK, true  -> R
    | G.CCW, false -> R
    | G.CLK, false -> L
    | G.CCW, true  -> L
    | G.COL, _     -> failwith "bogus ray endpoint"

  let faces_to_tikz fs =
    let tikz_of_point p =
      Printf.sprintf "\\fill %s circle[radius=0.02];"
                     (string_of_point p)
    in
    let tikz_of_segment (p1, p2) =
      Printf.sprintf "\\draw %s -- %s;"
                     (string_of_point p1)
                     (string_of_point p2)
    in
    let tikz_of_face f =
      let (p1, p2) =
        f |> dseg_of_face
          |> G.pts_of_dseg
          |> Util.pair_map (fun p -> (p.G.x, p.G.y))
      in
      String.concat "\n  "
      [ tikz_of_point p1
      ; tikz_of_point p2
      ; tikz_of_segment (p1, p2)
      ]
    in
    let tikzpicture body =
      Printf.sprintf
"\\documentclass[tikz,border=10pt]{standalone}
\\usepackage{tikz}
\\begin{document}
\\begin{tikzpicture}
  %s
\\end{tikzpicture}
\\end{document}"
    body
    in
    fs |> List.map tikz_of_face
       |> String.concat "\n  "
       |> tikzpicture

  let to_tikz m =
    m |> faces_of_mesh
      |> faces_to_tikz

  let binop ?name:(name = "(anonymous)") keepA keepB mA mB =
    let (smA, smB) = splits (mA, mB) in
    let mA' =
      List.filter
        (keepA smA smB)
        (faces_of_mesh smA)
    in
    let mB' =
      List.filter
        (keepB smA smB)
        (faces_of_mesh smB)
    in
    mkmesh (mA' @ mB')

  let union =
    let keepA smA smB fA =
      match face_mem smB fA with
      | Out -> true
      | In  -> false
      | On fs ->
          let na = norm smA fA in
          let keep =
            fs |> List.map (norm smB)
               |> List.for_all ((=) na)
          in
          keep
    in
    let keepB smA smB fB =
      face_mem smA fB = Out
    in
    binop ~name:"union" keepA keepB

  let diff =
    let keepA smA smB fA =
      match face_mem smB fA with
      | Out -> true
      | In  -> false
      | On fs ->
          let na = norm smA fA in
          let keep =
            fs |> List.map (norm smB)
               |> List.for_all ((<>) na)
          in
          keep
    in
    let keepB smA smB fB =
      face_mem smA fB = In
    in
    binop ~name:"diff" keepA keepB

  let inter =
    let keepA smA smB fA =
      match face_mem smB fA with
      | Out -> false
      | In  -> true
      | On fs ->
          let na = norm smA fA in
          let keep =
            fs |> List.map (norm smB)
               |> List.for_all ((=) na)
          in
          keep
    in
    let keepB smA smB fB =
      face_mem smA fB = In
    in
    binop ~name:"inter" keepA keepB

end

module Mesh2 (N : NUM) (G : GEOM2 with type num = N.t)
  : (MESH2 with type num = N.t
            and type pt  = N.t * N.t)
  = RawMesh2(N)(G)

module type MESH3 = sig

  type num
  type pt
  type t

  type region =
    | Outside
    | Boundary
    | Inside

  val mesh   : (pt * pt * pt) list -> t
  val faces  : t -> (pt * pt * pt) list
  val nfaces : t -> int
  val verts  : t -> pt list
  val vol    : t -> num

  val equiv    : t -> t  -> bool
  val contains : t -> t  -> bool
  val denote   : t -> pt -> region
  val splits   : (t * t) -> (t * t)

  val hull    : t   -> t
  val trans   : pt  -> t -> t
  val home    : pt  -> t -> t
  val scale   : pt  -> t -> t
  val fit     : pt  -> t -> t
  val rotateX : num -> t -> t
  val rotateY : num -> t -> t
  val rotateZ : num -> t -> t

  val union : t -> t -> t
  val diff  : t -> t -> t
  val inter : t -> t -> t
  val euler : pt -> pt -> pt -> (num * num * num)

  val cycles       : t -> t list
  val convex_split : t -> (t * t) option

  val to_string  : t -> string
  val to_stl     : t -> string
  val to_threejs : string -> t -> string
  val to_tikz    : t -> string

  val set_invariants : bool -> unit

end

module RawMesh3 (N : NUM) (G : GEOM3 with type num = N.t) = struct
  include MakeTaggedLogging(struct let tag = "Mesh3" end)

  type num = N.t
  type pt  = N.t * N.t * N.t

  let inv = ref true

  exception RepeatFace of string
  exception EmptyFace  of string
  exception OddEdge    of string
  exception NeedsSplit of string

  let set_invariants b = inv := b

  let () =
    Printexc.register_printer (function
      | RepeatFace s -> Some ("RepeatFace:\n\n" ^ s)
      | EmptyFace  s -> Some ("EmptyFace:\n\n"  ^ s)
      | OddEdge    s -> Some ("OddEdge:\n\n"    ^ s)
      | NeedsSplit s -> Some ("NeedsSplit:\n\n" ^ s)
      | _ -> None)

  let string_of_point (x, y, z) =
    Printf.sprintf "(%s, %s, %s)"
      (N.to_string x)
      (N.to_string y)
      (N.to_string z)

  (** faces *)
  type face =
    Face of G.tri

  let mkface t =
    if G.empty_tri t then
      raise (EmptyFace (G.string_of_tri t))
    else
      let vs =
        Util.sort G.cmp_pt [t.G.a; t.G.b; t.G.c] in
      Face (G.mktri (List.nth vs 0)
                    (List.nth vs 1)
                    (List.nth vs 2))

  let face (a, b, c) =
    let aux ( (ax, ay, az)
            , (bx, by, bz)
            , (cx, cy, cz)) =
      G.mktri
        (G.mkpt ax ay az)
        (G.mkpt bx by bz)
        (G.mkpt cx cy cz)
    in
    let t = aux (a, b, c) in
    mkface t

  let tri_of_face = function Face t ->
    t

  let verts_of_faces fs =
    fs |> List.map tri_of_face
       |> G.pts_of_tris

  let dsegs_of_tri t =
    let s1 = G.mkdseg t.G.a t.G.b in
    let s2 = G.mkdseg t.G.b t.G.c in
    let s3 = G.mkdseg t.G.c t.G.a in
    [s1; s2; s3]

  let dsegs_of_faces fs =
    fs |> List.map tri_of_face
       |> Util.flatmap dsegs_of_tri

  let string_of_pts_triple pts =
    pts |> Util.triple_map (fun p -> (p.G.x, p.G.y, p.G.z))
        |> Util.triple_map string_of_point
        |> Util.uncurry3 @@ Printf.sprintf "(%s, %s, %s)"

  let string_of_face f =
    f |> tri_of_face
      |> G.pts_of_tri
      |> string_of_pts_triple

  let equiv_face f1 f2 =
    let t1 = tri_of_face f1 in
    let t2 = tri_of_face f2 in
    G.equiv_tri t1 t2

  (* lifting some Geom3 functionality *)
  let lift_ptop_face op f =
    f |> tri_of_face
      |> G.lift_ptop_tri op
      |> mkface

  let on_face f pt =
    f |> tri_of_face
      |> Util.flip G.on_tri pt

  (** meshes *)

  type t =
    Mesh of face list

  type region =
    | Outside
    | Boundary
    | Inside

  (* constructors are not functions :\ *)
  let raise_rf msg = raise (RepeatFace msg)
  let raise_oe msg = raise (OddEdge    msg)
  let raise_ns msg = raise (NeedsSplit msg)

  let faces_of_mesh = function Mesh fs ->
    fs

  let nfaces = function Mesh fs ->
    List.length fs

  let faces m =
    let aux (a, b, c) =
      ( (a.G.x, a.G.y, a.G.z)
      , (b.G.x, b.G.y, b.G.z)
      , (c.G.x, c.G.y, c.G.z))
    in
    m |> faces_of_mesh
      |> List.map tri_of_face
      |> List.map G.pts_of_tri
      |> List.map aux

  let verts_of_mesh m =
    m |> faces_of_mesh
      |> verts_of_faces

  let verts m =
    m |> verts_of_mesh
      |> List.map (fun p -> (p.G.x, p.G.y, p.G.z))

  let find_repeat_face fs =
    fs |> Util.choose2
       |> List.find (Util.uncurry equiv_face)

  let find_odd_edge fs =
    let same_dseg s1 s2 =
      let (p11, p12) = G.pts_of_dseg s1 in
      let (p21, p22) = G.pts_of_dseg s2 in
      (*
      (G.equiv_pt p11 p21 || G.equiv_pt p11 p22) &&
      (G.equiv_pt p12 p21 || G.equiv_pt p12 p22)
      *)
      (G.equiv_pt p11 p21 && G.equiv_pt p12 p22) ||
      (G.equiv_pt p11 p22 && G.equiv_pt p12 p21)
    in
    let is_edge s f =
      let t = tri_of_face f  in
      let sab = G.mkdseg t.G.a t.G.b in
      let sbc = G.mkdseg t.G.b t.G.c in
      let sca = G.mkdseg t.G.c t.G.a in
      same_dseg sab s ||
      same_dseg sbc s ||
      same_dseg sca s
    in
    let odd_edge s =
      fs |> List.filter (is_edge s)
         |> List.length
         |> Util.of_parity false true
    in
    fs |> dsegs_of_faces
       |> List.find odd_edge

  let find_splittable_face fs =
    let splittable (t1, t2) =
      match
        G.part_tri t2 t1,
        G.part_tri t1 t2
      with
      | [_], [_] -> false
      | t1s, t2s -> begin
          let s_t1s =
            t1s |> List.map G.string_of_tri
                |> String.concat "\n" in
          let s_t2s =
            t2s |> List.map G.string_of_tri
                |> String.concat "\n" in
          log (String.concat "\n"
            [ "ERROR: find_splittable"
            ; "t1 ="
            ; G.string_of_tri t1
            ; "t2 = "
            ; G.string_of_tri t2
            ; "can split t1 into:"
            ; s_t1s
            ; "can split t2 into:"
            ; s_t2s
            ]);
          true
        end
    in
    fs |> List.map tri_of_face
       |> Util.choose2
       |> List.find splittable

  (** check invariants and make mesh *)
  let mkmesh fs =
    if !inv = false then
      Mesh fs
    else
      try
        fs |> find_repeat_face
           |> Util.pair_map string_of_face
           |> Util.uncurry (Printf.sprintf "%s\n%s")
           |> raise_rf
      with Not_found -> try
        fs |> find_odd_edge
           |> G.string_of_dseg
           |> raise_oe
      with Not_found -> try
        fs |> find_splittable_face
           |> Util.pair_map G.string_of_tri
           |> Util.uncurry (Printf.sprintf "%s\n\n%s")
           |> raise_ns
      with Not_found ->
        Mesh fs

  let mesh pps =
    let aux ( (ax, ay, az)
            , (bx, by, bz)
            , (cx, cy, cz)) =
      G.mktri
        (G.mkpt ax ay az)
        (G.mkpt bx by bz)
        (G.mkpt cx cy cz)
    in
    pps |> List.map aux
        |> List.map mkface
        |> mkmesh

  let string_of_faces fs =
    fs |> List.map string_of_face
       |> String.concat "\n; "
       |> Printf.sprintf "[ %s\n]"

  let to_string m =
    m |> faces_of_mesh
      |> string_of_faces

  let faces_to_tikz fs =
    let tikz_of_point p =
      Printf.sprintf "\\fill %s circle[radius=0.02];"
                     (string_of_point p)
    in
    let tikz_of_segment (p1, p2) =
      Printf.sprintf "\\draw %s -- %s;"
                     (string_of_point p1)
                     (string_of_point p2)
    in
    let tikz_of_face' (p1, p2, p3) =

      Printf.sprintf "\\fill[white] %s -- %s -- %s -- cycle;"
                     (string_of_point p1)
                     (string_of_point p2)
                     (string_of_point p3)
    in
    let tikz_of_face f =
      let (p1, p2, p3) =
        f |> tri_of_face
          |> G.pts_of_tri
          |> Util.triple_map (fun p -> (p.G.x, p.G.y, p.G.z))
      in
      String.concat "\n  "
      [ tikz_of_point p1
      ; tikz_of_point p2
      ; tikz_of_point p3
      ; tikz_of_segment (p1, p2)
      ; tikz_of_segment (p1, p3)
      ; tikz_of_segment (p2, p3)
      ; tikz_of_face' (p1, p2, p3)
      ]
    in
    let tikzpicture body =
(*      Printf.sprintf
"\\documentclass[tikz,border=10pt]{standalone}
\\usepackage{tikz}
\\begin{document}
\\begin{tikzpicture}
  %s
\\end{tikzpicture}
\\end{document}" *)
    body
    in
    fs |> List.map tikz_of_face
       |> String.concat "\n  "
       |> tikzpicture

  let to_tikz m =
    m |> faces_of_mesh
      |> faces_to_tikz

  let equiv m1 m2 =
    let f1s = faces_of_mesh m1 in
    let f2s = faces_of_mesh m2 in
    if List.length f1s <> List.length f2s then
      false
    else
      (* TODO sort? *)
      List.for_all
        (fun x -> List.exists (equiv_face x) f2s)
        f1s

  type mem =
    | Out
    | On of face list
    | In

  (** does any point lie on the line
    through [p1] and p[2] *)
  let thru_vert p1 p2 vs =
    List.exists
      (G.on_line (G.mkline p1 p2))
      vs

  (** does the ray through [pt] and [dpt]
    pass through a face edge in [m] *)
  let thru_edge pt dpt m =
    let ds = dsegs_of_faces
              (faces_of_mesh m)
    in
    List.exists
      (fun x ->
        let dseg = G.mkdseg pt dpt in
        let i = G.isect_rs (G.mkray dseg) x
        in
        match i with
        | G.SCross _ -> true
        | G.SNone    -> false
        | G.SOver  _ -> true
      )
      ds

  (** are [p1] and [p2] on the same plane? *)
  let same_plane p1 p2 ts =
    let aux p1 p2 t =
      let (a, b, c) = G.pts_of_tri t in
      G.coplanar a b c p1 &&
      G.coplanar a b c p2
    in
    List.exists (aux p1 p2) ts

  let is_vert' vs pt =
    List.exists (G.equiv_pt pt) vs

  let is_vert m pt =
    is_vert' (verts_of_mesh m) pt

  (* does [dpt] lie on a face edge of [m]? *)
  let on_edges m dpt =
    let ds = dsegs_of_faces
              (faces_of_mesh m)
    in
    List.exists
      (fun x -> G.on_dseg x dpt)
      ds

  (** pick a destination point for a ray
    from [pt] to pass though, so that the ray
    satisfies some invariants *)
  let dest_pt m pt =
   let ts = List.map
             (tri_of_face)
             (faces_of_mesh m)
   in
    match verts_of_mesh m with
    | [] ->
        G.add pt (G.mkpt N.n1 N.n1 N.n1)
    | vs ->
        if is_vert' vs pt then
          failwith "Mesh3.dest_pt: pt is on mesh"
        else
          let bad_dest dpt =
            is_vert' vs dpt      ||
            G.equiv_pt pt dpt    ||
            thru_vert pt dpt vs  ||
            thru_edge pt dpt m   ||
            same_plane pt dpt ts ||
            on_edges m dpt
          in
          vs |> G.bbox
             |> G.around_bbox
             |> Stream.find (Util.notp bad_dest)
             |> Util.valOf

  (**
    [directed_isects m a b] returns all faces of [m] that
    are intersected by the ray starting at [a] and going
    towards [b].

    NOTE does not include any intersection at [a]!
  *)
  let directed_isects m a b =
    let r = G.mkray (G.mkdseg a b) in
    let aux f =
      let t = tri_of_face f in
      if G.on_tri t a then
        false
      else
        match G.isect_tr t r with
        | G.TLNone   -> false
        | G.TLThru _ -> true
        | G.TLCut  _ -> failwith "Mesh3.directed_isects: bogus"
    in
    m |> faces_of_mesh
      |> List.filter aux

  let faces' fs =
    let aux (a, b, c) =
      ( (a.G.x, a.G.y, a.G.z)
      , (b.G.x, b.G.y, b.G.z)
      , (c.G.x, c.G.y, c.G.z)
      )
    in
    fs |> List.map tri_of_face
       |> List.map G.pts_of_tri
       |> List.map aux

  let directed_isects' m (ax, ay, az) (bx, by, bz) =
    directed_isects m (G.mkpt ax ay az) (G.mkpt bx by bz)
      |> faces'

  (** where does [pt] lie w.r.t. [m]? *)
  let denote' m pt =
    let on_fs =
      m |> faces_of_mesh
        |> List.filter (Util.flip on_face pt)
    in
    if on_fs <> [] then
      On on_fs
    else
      pt |> dest_pt m
         |> directed_isects m pt
         |> List.length
         |> Util.of_parity Out In

  let denote m (x, y, z) =
    match denote' m (G.mkpt x y z) with
    | In    -> Inside
    | On fs -> Boundary
    | Out   -> Outside

  (** [contains] checks whether m2 is inside m1 *)
  let contains m1 m2 =
    (* all points of [m2] should be inside [m1] *)
    m2 |> verts_of_mesh
       |> List.map (fun v -> (v.G.x, v.G.y, v.G.z))
       |> List.for_all (fun v ->
                        denote m1 v = Inside ||
                        denote m1 v = Boundary
                       )

  let lift_ptop_mesh op m =
    m |> faces_of_mesh
      |> List.map (lift_ptop_face op)
      |> mkmesh

  type norm = L | R

  let string_of_norm n =
    match n with
    | L -> "L"
    | R -> "R"

  let norm m f =
    let t   = tri_of_face f in
    let mid = G.centroid t in
    let rpt = dest_pt m mid in
    let sde = G.side_plane t rpt in
    let toward_out =
      rpt |> directed_isects m mid
          |> List.length
          |> Util.of_parity true false
    in
    match sde, toward_out with
    | G.POS, true  -> R
    | G.NEG, false -> R
    | G.POS, false -> L
    | G.NEG, true  -> L
    | G.COP, _     -> failwith "bogus ray endpoint"

  let abs_norm m f =
    let t = tri_of_face f in
    match norm m f with
    | L -> G.cross_prod (G.sub t.G.c t.G.a) (G.sub t.G.b t.G.a)
    | R -> G.cross_prod (G.sub t.G.b t.G.a) (G.sub t.G.c t.G.a)

  let rhr_pts m f =
    let (p1, p2, p3) =
      G.pts_of_tri (tri_of_face f)
    in
    match norm m f with
    | L -> (p3, p2, p1)
    | R -> (p1, p2, p3)

  let vol m =
    let anchor = G.mkpt N.n0 N.n0 N.n0 in
    m |> faces_of_mesh
      |> List.map (rhr_pts m)
      |> List.map (fun (a, b, c) -> G.tetraVol anchor a b c)
      |> List.fold_left N.add N.n0
      |> N.abs

  let hull m =
    m |> verts_of_mesh
      |> G.hull
      |> G.pdon_of_hull
      |> G.tris_of_pdon
      |> List.map mkface
      |> mkmesh

  let trans delta =
    let d = Util.uncurry3 G.mkpt delta in
    lift_ptop_mesh (G.add d)

  let home (px, py, pz) m =
    let bb =
      m |> verts_of_mesh
        |> G.bbox
    in
    let dx = N.sub bb.G.xmax bb.G.xmin in
    let dy = N.sub bb.G.ymax bb.G.ymin in
    let dz = N.sub bb.G.zmax bb.G.zmin in
    let ax = N.add bb.G.xmin (N.mul px dx) in
    let ay = N.add bb.G.ymin (N.mul py dy) in
    let az = N.add bb.G.zmin (N.mul pz dz) in
    trans (N.neg ax, N.neg ay, N.neg az) m

  let scale (fx, fy, fz) m =
    if N.cmp fx N.n0 = EQ
    || N.cmp fy N.n0 = EQ
    || N.cmp fz N.n0 = EQ then
      mkmesh []
    else
      let factor = G.mkpt fx fy fz in
      lift_ptop_mesh (G.mul factor) m

  let fit (dx', dy', dz') m =
    let bb =
      m |> verts_of_mesh
        |> G.bbox
    in
    let dx = N.sub bb.G.xmax bb.G.xmin in
    let dy = N.sub bb.G.ymax bb.G.ymin in
    let dz = N.sub bb.G.zmax bb.G.zmin in
    let fx = N.div dx' dx in
    let fy = N.div dy' dy in
    let fz = N.div dz' dz in
    scale (fx, fy, fz) m

  let rotateX deg =
    lift_ptop_mesh (G.rotateX deg)

  let rotateY deg =
    lift_ptop_mesh (G.rotateY deg)

  let rotateZ deg =
    lift_ptop_mesh (G.rotateZ deg)

  module FactorManifolds : sig
    val factor : t -> t list
  end = struct
    module Q = RQueue.Queue

    let __seen : face list ref =
      ref []

    let mark f =
      __seen := f :: !__seen

    let seen f =
      List.memq f !__seen

    let init () =
      __seen := []

    let rhrule_tri f m =
      let (a, b, c) =
        G.pts_of_tri (tri_of_face f) in
      match norm m f with
      | R -> (a, b, c)
      | L -> (c, b, a)

    (* return faces which are manifold-adjacent to f in m *)
    let get_manifold_adj f m =
      let (fA, fB, fC) =
        rhrule_tri f m in
      let fTri =
        G.mktri fA fB fC in
      let fPlane =
        G.plane_of_tri fTri in
      let fSide pt =
        G.side_plane fTri pt in

      (*  (p, q) is an edge of f
       *
       *  r is the "other" point of f
       *
       *  p, q, r are ordered by RHR (as in fTri)
       *
       *  p, q, r are in fPlane
       *
       *  fSide indicates whether a point is above fPlane (by RHR)
       *
       *)
      let edge_neighbor p q r =
        (* we need the plane normal to fPlane that (p, q) is in *)
        let nTri =
          let xp = G.cross_prod (G.sub q p) (G.sub r p) in
          let n  = G.add p xp in
          (* r should be above nTri *)
          G.mktri p n q
        in
        let nSide pt =
          G.side_plane nTri pt in

        let neighbor_angle g =
          let (gA, gB, gC) =
            rhrule_tri g m in
          let g_other =
            let pq_equiv t u =
              (G.equiv_pt p t && G.equiv_pt q u) ||
              (G.equiv_pt p u && G.equiv_pt q t) in
            if pq_equiv gA gB then
              Some gC
            else if pq_equiv gB gC then
              Some gA
            else if pq_equiv gC gA then
              Some gB
            else
              None
          in
          match g_other with
          | None -> []
          | Some gR ->
              let pa =
                G.plane_angle fPlane (G.mkplane gA gB gC) in
              (* plane angles are always acute, so need to
               * adjust based on which "quadrant" gR is in *)
              (* TODO : probably wrong, needs tests *)
              if fSide gR = G.POS then
                if nSide gR = G.POS then
                  [(g, pa)]
                else
                  [(g, N.sub N.pi pa)]
              else
                if nSide gR = G.POS then
                  [(g, N.sub N.twoPi pa)]
                else
                  [(g, N.add N.pi pa)]
        in
        (* we take the neighbor with largest angle in direction of normal *)
        m |> faces_of_mesh
          |> List.filter (notp (equiv_face f))
          |> Util.flatmap neighbor_angle
          |> Util.sort (fun (_, a1) (_, a2) -> N.cmp a2 a1)
          |> List.hd
          |> fst
      in
      (* note: right rule must be preserved here! *)
      [ edge_neighbor fA fB fC
      ; edge_neighbor fB fC fA
      ; edge_neighbor fC fA fB
      ]

    (* assumes m is non-empty *)
    let extract_atom m =
      let rec loop atom q =
        match Q.pop q with
        | None ->
            ( mkmesh atom
            , m |> faces_of_mesh
                |> List.filter (fun f -> not (List.mem f atom))
                |> mkmesh )
        | Some (f, q') ->
            m |> get_manifold_adj f
              |> List.filter (notp seen)
              >> List.iter mark
              |> Q.pushl q'
              |> loop (f :: atom)
      in
      m |> faces_of_mesh
        |> List.hd
        >> mark
        |> Q.single
        |> loop []

    let factor m =
      init ();
      let rec loop acc m =
        if faces_of_mesh m = [] then
          acc
        else
          let (atom, m') = extract_atom m in
          loop (atom :: acc) m'
      in
      loop [] m
  end

  let cycles m =
    FactorManifolds.factor m

  let face_splits fs f =
    let ts =
      List.map tri_of_face fs
    in
    f |> tri_of_face
      |> G.part_tris ts
      |> List.map mkface

  (* split faces [fBs] with respect to [fAs] *)
  let splits' fAs fBs =
    Util.flatmap (face_splits fAs) fBs

  (* split two meshes with respect to one another *)
  let splits_aux (mA, mB) =
    let fsA = faces_of_mesh mA in
    let fsB = faces_of_mesh mB in
    let fsA' = splits' fsB fsA in
    let fsB' = splits' fsA' fsB in
    (mkmesh fsA', mkmesh fsB')

  let splits (mA, mB) = begin
    log (Printf.sprintf "splits pre: %d left faces, %d right faces"
      (nfaces mA) (nfaces mB));
    let rec loop i (mA, mB) =
      let (mA', mB') = splits_aux (mA, mB) in
      log (Printf.sprintf "splits iter %d: %d left faces, %d right faces"
        i (nfaces mA') (nfaces mB'));
      if equiv mA mA' && equiv mB mB'
      then (mA, mB)
      else loop (i + 1) (mA', mB')
    in
    loop 1 (mA, mB)
    >> (fun (mA', mB') ->
      log (Printf.sprintf "splits post: %d left faces, %d right faces"
        (nfaces mA') (nfaces mB')))
  end

  let face_mem m f =
    denote' m (G.centroid (tri_of_face f))

  let normed_string_of_face m f =
    match norm m f with
    | R -> string_of_face f
    | L ->
       let (p1, p2, p3) = G.pts_of_tri (tri_of_face f)
       in string_of_pts_triple (p2, p1, p3)

  let normed_to_string m =
    m |> faces_of_mesh
      |> List.map (normed_string_of_face m)
      |> String.concat "\n; "
      |> Printf.sprintf "[ %s\n]"

  let threejs_of_face nm f =
    f |> tri_of_face
      |> G.threejs_of_tri nm

  let normed_threejs_of_face nm m f =
    match norm m f with
    | R -> f |> tri_of_face
             |> G.threejs_of_tri nm
    | L -> f |> tri_of_face
             |> (fun t -> G.mktri t.G.b t.G.a t.G.c)
             |> G.threejs_of_tri nm

  let normed_faces_to_threejs nm m fs =
    fs |> List.map (normed_threejs_of_face nm m)
       |> String.concat "\n\n"

  let to_threejs nm m =
    m |> faces_of_mesh
      |> normed_faces_to_threejs nm m

  let stl_of_face m f =
    let n =
      G.unit_vec (abs_norm m f) in
    let (p1, p2, p3) =
      G.pts_of_tri (tri_of_face f) in
    let (a, b, c) =
      (* STL demands right hand rule *)
      match norm m f with
      | L -> (p3, p2, p1)
      | R -> (p1, p2, p3)
    in
    Printf.sprintf
"facet normal %s %s %s
  outer loop
    vertex %s %s %s
    vertex %s %s %s
    vertex %s %s %s
  endloop
endfacet"
      (N.to_string n.G.x) (N.to_string n.G.y) (N.to_string n.G.z)
      (N.to_string a.G.x) (N.to_string a.G.y) (N.to_string a.G.z)
      (N.to_string b.G.x) (N.to_string b.G.y) (N.to_string b.G.z)
      (N.to_string c.G.x) (N.to_string c.G.y) (N.to_string c.G.z)

  let to_stl m =
    m |> faces_of_mesh
      |> List.map (stl_of_face m)
      |> String.concat "\n"
      |> Printf.sprintf
"solid REINCARNATE
%s
endsolid REINCARNATE"

  let binop ?name:(name = "(anonymous)") keepA keepB mA mB =
    log (Printf.sprintf "binop %s: %d left faces, %d right faces"
      name (nfaces mA) (nfaces mB));
    let (smA, smB) = splits (mA, mB) in
    let mA' =
      List.filter
        (keepA smA smB)
        (faces_of_mesh smA)
    in
    let mB' =
      List.filter
        (keepB smA smB)
        (faces_of_mesh smB)
    in
    mkmesh (mA' @ mB')
    >> (fun m ->
      log (Printf.sprintf "binop %s: %d result faces"
        name (nfaces m)))

  let same_direction v1 v2 =
    let d = G.dot_prod v1 v2 in
    match N.cmp d N.n0 with
    | EQ ->
      failwith (Printf.sprintf "same_direction ambiguous %s %s"
                               (G.string_of_pt v1)
                               (G.string_of_pt v2))
    | LT -> false
    | GT -> true

  let union =
    let keepA smA smB fA =
      match face_mem smB fA with
      | Out -> true
      | In  -> false
      | On [fB] ->
          let na = norm smA fA in
          let keep =
            if equiv_face fB fA then
               na = (norm smB fB)
            else
              failwith "union.keepA: non equiv faces"
          in
         keep
      | On fs ->
          failwith
          (Printf.sprintf
            "union.keepA: %s not on exactly one face\n%s"
            (string_of_face fA)
            (string_of_faces fs))
    in
    let keepB smA smB fB =
      face_mem smA fB = Out
    in
    binop ~name:"union" keepA keepB

  let diff =
    let keepA smA smB fA =
      match face_mem smB fA with
      | Out -> true
      | In  -> false
      | On [fB] ->
          let na = norm smA fA in
          let keep =
            if equiv_face fB fA then
              na <> (norm smB fB)
            else
              failwith "diff.keepA: non equiv faces"
          in
          keep
      | On fs ->
          failwith
            (Printf.sprintf "diff.keepA: %s not on exactly one face\n%s"
              (string_of_face fA)
              (string_of_faces fs))
    in
    let keepB smA smB fB =
      face_mem smA fB = In
    in
    binop ~name:"diff" keepA keepB

  let inter =
    let keepA smA smB fA =
      match face_mem smB fA with
      | Out -> false
      | In  -> true
      | On [fB] ->
          let na = norm smA fA in
          let keep =
            if equiv_face fB fA then
              na = norm smB fB
            else
              failwith "inter.keepA: non equiv faces"
          in
          keep
      | On fs ->
          failwith
            (Printf.sprintf "inter.keepA: %s not on exactly one face\n%s"
              (string_of_face fA)
              (string_of_faces fs))
    in
    let keepB smA smB fB =
      face_mem smA fB = In
    in
    binop ~name:"inter" keepA keepB

  let unit_cube = mesh
    [ ((N.n0, N.n0, N.n0), (N.n1, N.n0, N.n0), (N.n1, N.n1, N.n0))
    ; ((N.n0, N.n0, N.n0), (N.n1, N.n1, N.n0), (N.n0, N.n1, N.n0))
    ; ((N.n0, N.n0, N.n1), (N.n1, N.n0, N.n1), (N.n1, N.n1, N.n1))
    ; ((N.n0, N.n0, N.n1), (N.n1, N.n1, N.n1), (N.n0, N.n1, N.n1))
    ; ((N.n0, N.n0, N.n0), (N.n1, N.n0, N.n0), (N.n1, N.n0, N.n1))
    ; ((N.n0, N.n0, N.n0), (N.n1, N.n0, N.n1), (N.n0, N.n0, N.n1))
    ; ((N.n0, N.n1, N.n0), (N.n1, N.n1, N.n0), (N.n1, N.n1, N.n1))
    ; ((N.n0, N.n1, N.n0), (N.n1, N.n1, N.n1), (N.n0, N.n1, N.n1))
    ; ((N.n0, N.n0, N.n0), (N.n0, N.n1, N.n0), (N.n0, N.n1, N.n1))
    ; ((N.n0, N.n0, N.n0), (N.n0, N.n1, N.n1), (N.n0, N.n0, N.n1))
    ; ((N.n1, N.n0, N.n0), (N.n1, N.n1, N.n0), (N.n1, N.n1, N.n1))
    ; ((N.n1, N.n0, N.n0), (N.n1, N.n1, N.n1), (N.n1, N.n0, N.n1)) ]

  let euler (n1x, n1y, n1z) (n2x, n2y, n2z) (n3x, n3y, n3z) =
    let nx = G.mkpt n1x n1y n1z in
    let ny = G.mkpt n2x n2y n2z in
    let nz = G.mkpt n3x n3y n3z in
    let ry = N.asin (N.neg (nx.G.z))
              |> N.mul (N.div (N.of_string "180") N.pi)
    in
    if not (N.equiv (N.abs ry) (N.of_string "90.0"))
    then
      let rx = N.atan2 ny.G.z nz.G.z
                |> N.mul (N.div (N.of_string "180") N.pi)
      in
      let rz = N.atan2 nx.G.y nx.G.x
                |> N.mul (N.div (N.of_string "180") N.pi)
      in
      (rx, ry, rz)
    else
      let rx =
        N.sub (N.of_string "180")
              (N.acos ny.G.y
                |> N.mul (N.div (N.of_string "180") N.pi))
      in
      let rz = N.n0 in
      (rx, ry, rz)

  (* helper for types *)
  let translate pt m =
    trans (pt.G.x, pt.G.y, pt.G.z) m

  (* the base masking cube has center at `a` *)
  let mkmask n1 n2 n3 dm a =
    print_endline (G.string_of_pt n1);
    print_endline (G.string_of_pt n2);
    print_endline (G.string_of_pt n3);
    print_endline (N.to_string dm);
    print_endline (G.string_of_pt a);
    let half = N.div N.n1 N.n2 in
    let (rx, ry, rz) = euler (n1.G.x, n1.G.y, n1.G.z)
                             (n2.G.x, n2.G.y, n2.G.z)
                             (n3.G.x, n3.G.y, n3.G.z)
    in
    unit_cube
      |> home (half, half, half)
      |> scale (dm, dm, dm)
      |> rotateX rx
      |> rotateY ry
      |> rotateZ rz
      |> translate a

  let plane_split (a, b, c) m =
    print_endline "a b c: ";
    print_endline (G.string_of_pt a);
    print_endline (G.string_of_pt b);
    print_endline (G.string_of_pt c);
    (* the plane normal and two vectors in the plane normal to it *)
    let n1 = G.plane_norm (a, b, c) in
    let n2 = G.sub b a in
    let n3 = G.cross_prod n1 n2 in

    (* dimension of masking cube *)
    let bb = G.bbox (verts_of_mesh m) in
    let dx = N.sub bb.G.xmax bb.G.xmin in
    let dy = N.sub bb.G.ymax bb.G.ymin in
    let dz = N.sub bb.G.zmax bb.G.zmin in
    let dm = N.mul N.n180 (maxl N.cmp [dx; dy; dz]) in

    let mask = mkmask n1 n2 n3 dm a in

    (* top and bottom masks *)
    let dm2 = N.div dm N.n2 in
    let top_mask =
      translate
        (G.mul n1 (G.mkpt dm2 dm2 dm2))
        mask
    in
    let neg_dm2 = N.neg dm2 in
    let bot_mask =
      translate
        (G.mul n1 (G.mkpt neg_dm2 neg_dm2 neg_dm2))
        mask
    in
    Util.to_file "topmask.stl" (to_stl top_mask);
    Util.to_file "botmask.stl" (to_stl bot_mask);

    (* the two halves "above" and "below" the plane *)
    ( diff m top_mask
    , diff m bot_mask )

  (* copy/paste some from FactorManifolds and Geom 3D Hull *)
  module ConvexSplit : sig
    val split : t -> (t * t) option
  end = struct

    let rhrule_tri m f =
      let (a, b, c) =
        G.pts_of_tri (tri_of_face f) in
      match norm m f with
      | R -> (a, b, c)
      | L -> (c, b, a)

    let rhrule_norm (a, b, c) =
      G.cross_prod (G.sub b a) (G.sub c a)

    type edge =
      | AB
      | BC
      | CA

    let get_edge (a, b, c) = function
      | AB -> (a, b)
      | BC -> (b, c)
      | CA -> (c, a)

    let get_other (a, b, c) = function
      | AB -> c
      | BC -> a
      | CA -> b

    let share_pt (a, b) (p, q) =
         G.equiv_pt a p
      || G.equiv_pt a q
      || G.equiv_pt b p
      || G.equiv_pt b q

    (* b/c of RHR, order flipped on neighbors *)
    let neighbor_edge (a, b) (p, q) =
      (G.equiv_pt a q && G.equiv_pt b p)

    let shared_edge_other t1 t2 =
      let es = [AB; BC; CA] in
      let aux (e1, e2) =
        neighbor_edge (get_edge t1 e1) (get_edge t2 e2) in
      try
        let (e1, e2) =
          List.find aux (xprod es es) in
        Some (get_edge t1 e1, get_other t2 e2)
      with Not_found ->
        None

    let pinchedge (t1, t2) =
      match shared_edge_other t1 t2 with
      | None -> []
      | Some (e, x) ->
          let gt1 = Util.uncurry3 G.mktri t1 in
          if G.side_plane gt1 x = G.POS
          then [e]
          else []

    let rec all_coplanar = function
      | [] -> true
      | [_] -> true
      | (m, n) :: (p, q) :: es ->
          G.coplanar m n p q &&
          all_coplanar ((p, q) :: es)

    let rec plane_pts_of_edge_chain = function
      | (m, n) :: (p, q) :: ec ->
          if not (G.collinear m n p) then
            [(m, n, p)]
          else if not (G.collinear m n q) then
            [(m, n, q)]
          else
            plane_pts_of_edge_chain ((m, n) :: ec)
      | _ ->
          []

    let pinchplanes es =
      let aux edge_chains e =
        let rec loop = function
          | [] -> [[e]]
          | ec :: ecs ->
              if List.exists (share_pt e) ec
              then (e :: ec) :: ecs
              else ec :: loop ecs
        in
        loop edge_chains
      in
      es |> List.fold_left aux []
         |> List.sort (fun a b -> List.length b - List.length a)
         (* TODO handle intersecting pinchplanes *)
         |> List.filter all_coplanar
         |> Util.flatmap plane_pts_of_edge_chain

    let split m =
      let pps =
        m |> faces_of_mesh
          |> List.map (rhrule_tri m)
          |> Util.choose2
          |> Util.flatmap pinchedge
          |> pinchplanes
      in
      match pps with
      | [] -> None
      (* TODO for now just taking split with most edges *)
      | pp :: _ -> Some (plane_split pp m)
  end

  let convex_split m =
    ConvexSplit.split m
end

module Mesh3 (N : NUM) (G : GEOM3 with type num = N.t)
  : (MESH3 with type num = N.t
            and type pt  = N.t * N.t * N.t)
  = RawMesh3(N)(G)
