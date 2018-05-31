(** Synthesize CAD from Mesh *)

open Util
open NumSys

open LinAlg
open Geom
open Mesh
open CAD
open SynthBench
open RPriorityQueue

module type SYNTH = sig
  type mesh
  type cad
  type bench

  exception InitialMesh

  val synth   : (string * cad) list -> int -> mesh -> cad
  val synth_b : bench -> cad

end

module RawSynth1
  (N   : NUM)
  (Q   : PRIORITY_QUEUE
         with type priority = N.t)
  (G1  : GEOM1
         with type num = N.t)
  (M1  : MESH1
         with type num = N.t
          and type pt  = N.t)
  (C1  : CAD1
         with type num  = N.t
          and type mesh = M1.t)
  (SB1 : SYNTHBENCH
          with type mesh = M1.t
           and type prim = C1.t)
= struct

  type mesh  = M1.t
  type cad   = C1.t
  type bench = SB1.t

  exception InitialMesh

  let prims : (string * cad) list ref = ref []

  (** returns the CAD with fewer number of primitives *)
  (* TODO: other ways to compare CADs? *)
  let rank_edit c1 c2 =
    Pervasives.compare (C1.num_prims c1)
                       (C1.num_prims c2)

  (* pick the CAD with least number of prims *)
  let best cads =
    if cads <> [] then
      List.sort rank_edit cads
        |> List.hd
    else
      C1.Empty

  let mesh_verts m =
    m |> M1.faces
      |> List.map (fun x -> [x])
      |> List.flatten

  let bbox m =
    let bb =
      m |> mesh_verts
        |> List.map G1.mkpt
        |> G1.bbox
    in
    let span =
      N.sub bb.G1.xmax bb.G1.xmin
    in
    C1.Unop ( C1.Trans bb.G1.xmin
            , C1.Unop ( C1.Scale span
                       , C1.Unit))

  (* splits the mesh at the mid point *)
  let mid_split m =
    let m = mesh_verts m in
    let rec range i1 i2 l =
      if i2 < i1 then
        []
      else
        (match l with
         | [] -> failwith "empty list"
         | _  -> List.nth l i1 :: range (i1 + 1) i2 l)
    in
    let len  = List.length m in
    let mid = len / 2 in
    if mid < 1 then
      failwith "illegal mesh"
    else
      if mid mod 2 = 0 then
        let m1 = range 0 (mid - 1) m in
        let m2 = range mid (len - 1) m in
        (M1.mesh m1, M1.mesh m2)
       else
        let m1 = range 0 mid m in
        let m2 = range (mid + 1) (len - 1) m in
        (M1.mesh m1, M1.mesh m2)


  (* TODO: other splits *)
  let best_split m =
    mid_split m

  let rec infer m =
    if m = M1.mesh [] then
      [C1.Empty]
    else
      let bb   = bbox m in
      let c_bb = C1.compile bb in
      if  c_bb = m then
        [bb]
      else
        let a_cdts = additive m in
        let s_cdts = subtractive bb m in
        a_cdts @ s_cdts
  and subtractive bb m =
    let bb_m = C1.compile bb in
    let d    = M1.diff bb_m m in
    let ds   = infer d in
    let subs = List.map
                (fun x ->
                  C1.Binop ( C1.Diff
                           , bb
                           , x
                           )) ds in
    subs
  and additive m =
    let (m1, m2) = best_split m in
    let c1s      = infer m1 in
    let c2s      = infer m2 in
    let adds = List.map
                (fun x ->
                  List.map
                    (fun y ->
                      C1.Binop ( C1.Union
                               , x
                               , y
                               )
                    ) c2s
                ) c1s in
     List.flatten adds

  (* TODO use prims and fuel *)
  let synth ps f m =
    prims := ps;
(*  let m = Util.sort M1.cmp m in*)
(*  TODO: we need the above line *)
    let cs = infer m in
    C1.simplify (best cs)

  let synth_b bench =
    match bench.SB1.prims with
    | None ->
        (* TODO: somehow use default prim list *)
        synth
          []
          bench.SB1.fuel
          bench.SB1.input
    | Some l ->
        synth
          []
          bench.SB1.fuel
          bench.SB1.input
end

module Synth1
  (N   : NUM)
  (Q   : PRIORITY_QUEUE
         with type priority = N.t)
  (G1  : GEOM1
         with type num = N.t)
  (M1  : MESH1
         with type num = N.t
          and type pt  = N.t)
  (C1  : CAD1
         with type num  = N.t
          and type mesh = M1.t)
  (SB1 : SYNTHBENCH
          with type mesh = M1.t
           and type prim = C1.t)
  : (SYNTH
     with type mesh  = M1.t
      and type cad   = C1.t
      and type bench = SB1.t)
= RawSynth1(N)(Q)(G1)(M1)(C1)(SB1)

module RawSynth2
  (N   : NUM)
  (Q   : PRIORITY_QUEUE
         with type priority = N.t)
  (G2  : GEOM2
         with type num = N.t)
  (M2  : MESH2
         with type num = N.t
          and type pt  = N.t * N.t)
  (C2  : CAD2
         with type num  = N.t
          and type mesh = M2.t)
  (SB2 : SYNTHBENCH
          with type mesh = M2.t
           and type prim = C2.t)
= struct

  type mesh  = M2.t
  type cad   = C2.t
  type bench = SB2.t

  exception InitialMesh

(*

           -----------------------
          < Worklist based synth. >
           -----------------------
                  \   ^__^
                   \  (oo)\_______
                      (__)\       )\/\
                          ||----w |
                          ||     ||

*)

  (** returns the CAD with fewer number of primitives *)
  (* TODO: other ways to compare CADs? *)
  let rank_edit c1 c2 =
    Pervasives.compare (C2.num_prims c1)
                       (C2.num_prims c2)

  let rec hole_count c =
    match c with
    | C2.Empty  -> 0
    | C2.Unit   -> 0
    | C2.Mesh m -> 1
    | C2.Unop (_, c1) ->
        hole_count c1
    | C2.Binop (op, c1, c2) ->
        hole_count c1 + hole_count c2

  (** find point in m farthest from origin
    and put it on the x-axis *)
  let canonicalize m =
    let fs = M2.faces m in
    let ps = List.map (fun (x, y) -> G2.mkpt x y)
                      (M2.verts m)
    in
    let lstr = string_of_int (List.length ps) in
    let orig = G2.mkpt N.n0 N.n0 in
    let tran =
      ps |> List.fold_left G2.add orig
         |> Util.flip G2.div
              (G2.mkpt (N.of_string lstr)
                       (N.of_string lstr))
         |> Util.flip G2.sub orig
    in
    let tx  = tran.G2.x in
    let ty  = tran.G2.y in
    let fs' =
      List.map
        (fun ((ax, ay), (bx, by)) ->
           ( (N.sub ax tx, N.sub ay ty)
           , (N.sub bx tx, N.sub by ty)))
        fs
    in
    let m'  = M2.mesh fs' in
    let ps' =
      List.map
        (fun (x, y) -> G2.mkpt x y)
        (M2.verts m')
    in
    let bb  = G2.bbox ps' in
    let dx  = N.sub bb.G2.xmax bb.G2.xmin in
    let dy  = N.sub bb.G2.ymax bb.G2.ymin in
    let fit = G2.mkpt dx dy in
    let m'' = M2.scale (N.div N.n1 dx, N.div N.n1 dy) m' in
    let ps'' =
      List.map
        (fun (x, y) -> G2.mkpt x y)
        (M2.verts m'')
    in
    let ufar =
      ps'' |> List.map (fun pt -> (pt, G2.dist_pt pt orig))
           |> Util.maxl (fun (p1, d1) (p2, d2) -> N.cmp d1 d2)
           |> fst
           |> G2.unit_vec
    in
    let deg = N.atan2 (N.neg ufar.G2.y) ufar.G2.x
               |> N.mul (N.div (N.of_string "180") N.pi)
    in
    (* TODO: also rotate to align along y axis? *)
    let cm = M2.rotate deg m'' in
    (cm, tran, deg, fit)

  let prims : (string * cad) list ref = ref []
  let __bprims = ref []

  (* This is list of canonicalized primitives *)
  (* careful to delay eval (refs) *)
  let canon_prims () =
    List.map
      (fun x -> pair x (canonicalize (C2.compile x)))
      (List.map (fun (x, y) -> y) !prims)

  let rec union_diff t =
    match t with
    | CTree.Node xs ->
        let diff (v, ns) =
          C2.mkbinop C2.Diff (C2.Mesh v) (union_diff ns)
        in
        let diffs = List.map diff xs in
        List.fold_left (C2.mkbinop C2.Union) C2.Empty diffs

  let factor_manifolds m =
    let cs = M2.cycles m in
    let ms = List.map (M2.path) cs in
    let t  = CTree.mkctree M2.contains ms
    in union_diff t

  let hole_depth c =
    let rec loop = function
      | C2.Empty  -> None
      | C2.Unit   -> None
      | C2.Mesh _ -> Some 0
      | C2.Unop (_, c1) ->
          begin match loop c1 with
          | None   -> None
          | Some i -> Some (i + 1)
          end
      | C2.Binop (_, c1, c2) ->
          begin match loop c1, loop c2 with
          | None, None ->
              None
          | None, Some i
          | Some i, None ->
              Some (i + 1)
          | Some i, Some j ->
              Some ((Pervasives.min i j) + 1)
          end
    in loop c

  let disj_step c =
    let rec go = function
      | C2.Empty -> C2.Empty
      | C2.Unit  -> C2.Unit
      | C2.Mesh m ->
          factor_manifolds m
      | C2.Unop (op, c1) ->
          C2.Unop (op, go c1)
      | C2.Binop (op, c1, c2) ->
          (* TODO : develop better prioritization heuristic
           * --OR-- find way to add both options, but ensure
           *        that we avoid repeating equivalent work
           *        (e.g., perhaps using a cache?)
           *)
          begin match hole_depth c1, hole_depth c2 with
           | None,   None   -> C2.Binop (op, c1, c2)
           | None,   Some i -> C2.Binop (op, c1, go c2)
           | Some i, None   -> C2.Binop (op, go c1, c2)
           | Some i, Some j ->
               if i < j
               then C2.Binop (op, go c1, c2)
               else C2.Binop (op, c1, go c2)
          end
    in
    go c

  let splits m =
    (* TODO: splits *)
    [m]

  let add_step c =
    let rec go = function
      | C2.Empty -> C2.Empty
      | C2.Unit  -> C2.Unit
      | C2.Mesh m ->
          let ms = splits m in
          if List.length ms = 1 then
            C2.Mesh m
          else
            ( List.fold_left
                (C2.mkbinop C2.Union)
                C2.Empty (List.map C2.mkhole ms))
      | C2.Unop (op, c1) ->
          C2.Unop (op, go c1)
      | C2.Binop (op, c1, c2) ->
          begin match hole_depth c1, hole_depth c2 with
           | None,   None   -> C2.Binop (op, c1, c2)
           | None,   Some i -> C2.Binop (op, c1, go c2)
           | Some i, None   -> C2.Binop (op, go c1, c2)
           | Some i, Some j ->
               if i < j
               then C2.Binop (op, go c1, c2)
               else C2.Binop (op, c1, go c2)
          end
    in
    go c

  let b_prim m =
    try
      List.assoc m !__bprims
    with
      Not_found ->
      let (cm, t, d, s) = canonicalize m in
      let same_mesh (m1, t1, d1, s1) (c, (m2, t2, d2, s2)) =
        (m1, m2) |> M2.splits
                 |> Util.uncurry M2.equiv
      in
      let min_diff_area m1 cps =
        let cp' =
          List.filter
            (fun (p, (m, t, d, s)) ->
              M2.faces (M2.diff m1 m) = [])
            cps
        in
        cp' |> List.map (fun (p, (m, t, d, s)) ->
                ((p, (m, t, d, s)), M2.diff m m1))
            |> List.map (fun ((p, (m, t, d, s)), df) ->
                ((p, (m, t, d, s)), M2.area df))
            |> List.sort
                (fun (x1, y1) (x2, y2) ->
                  Pervasives.compare y1 y2)
      in
      let tune_prim (p, (m', t', d', s')) =
        p |> C2.mkunop
              (C2.Trans (N.neg t'.G2.x, N.neg t'.G2.y))
          |> C2.mkunop
              (C2.Rotate d')
          |> C2.mkunop
              (C2.Scale (s'.G2.x, s'.G2.y))
          |> C2.mkunop
              (C2.Rotate (N.neg d))
          |> C2.mkunop
              (C2.Scale (s.G2.x, s.G2.y))
          |> C2.mkunop
              (C2.Trans (t.G2.x, t.G2.y))
      in
      match
        List.find_opt
          (same_mesh (cm, t, d, s))
          (canon_prims ())
      with
      | Some (p, (m', t', d', s')) ->
          let tp = tune_prim (p, (m', t', d', s')) in
          __bprims := (m, tp) :: !__bprims;
          tp
      | None ->
          let prms = min_diff_area cm (canon_prims ()) in
          if List.length prms > 0
          then
            ( let tp = tune_prim (fst (List.hd prms)) in
              __bprims := (m, tp) :: !__bprims;
              tp )
          else
            ( __bprims := (m, C2.Mesh m) :: !__bprims;
              C2.Mesh m )

  let sub_step c =
    let rec go = function
      | C2.Empty -> C2.Empty
      | C2.Unit  -> C2.Unit
      | C2.Mesh m ->
          if M2.faces m = [] then
            C2.Empty
          else
            let bb = b_prim m in
            if bb = C2.Mesh m then
              C2.Mesh m
            else
              let d = M2.diff (C2.compile bb) m in
              C2.mkbinop C2.Diff bb (C2.Mesh d)
      | C2.Unop (op, c1) ->
          C2.Unop (op, go c1)
      | C2.Binop (op, c1, c2) ->
          begin match hole_depth c1, hole_depth c2 with
           | None,   None   -> C2.Binop (op, c1, c2)
           | None,   Some i -> C2.Binop (op, c1, go c2)
           | Some i, None   -> C2.Binop (op, go c1, c2)
           | Some i, Some j ->
               if i < j
               then C2.Binop (op, go c1, c2)
               else C2.Binop (op, c1, go c2)
          end
    in
    go c

  let step c =
    [ disj_step c
    ; add_step  c
    ; sub_step  c
    ]

  let best1 cs =
    cs |> Util.sort C2.cmp
       |> List.rev
       |> List.hd
       |> C2.simplify

  let best2 cs =
    cs |> List.map (fun x -> (x, hole_count x))
       |> List.sort
            (fun (c1, h1) (c2, h2) ->
              Pervasives.compare h1 h2)
       |> List.hd
       |> fst
       |> C2.simplify

  let best3 cs =
    cs |> List.sort rank_edit
       |> List.hd
       |> C2.simplify

  (* TODO: use priority queue *)
  let synth_c limit c =
    let rec loop fin wl =
      match Q.pop wl with
      | None ->
          fin
      | Some (p, (i, c), rest) ->
          if i < limit then
            c |> step
              |> List.map (pair (i + 1))
              |> List.map (pair N.n1) (* TODO: dummy priority for now *)
              |> Q.pushl rest
              |> loop fin
          else
            loop (c :: fin) rest
    in
    c |> pair 0
      |> pair N.n1 (* TODO: dummy priority for now *)
      |> Q.single
      |> loop []
      |> best2

  let synth_aux f m =
    if M2.faces m = [] then
      C2.Empty
    else
      let simplest m1 p =
        let cp = C2.compile p in
        (m1, cp) |> M2.splits
                 |> Util.uncurry M2.equiv
      in
      match List.find_opt (simplest m)
                          (List.map (fun (x, y) -> y) !prims)
      with
      | Some p -> p
      | None   -> synth_c f (C2.Mesh m)

  let synth ps f m =
    prims := ps;
    let c = synth_aux f m in
    match c with
    | C2.Mesh _ ->
        raise InitialMesh
    | _ ->
        print_endline (C2.to_string c);
        c

  let synth_b bench =
    match bench.SB2.prims with
    | None ->
        (* TODO: somehow use default prim list *)
        synth
          []
          bench.SB2.fuel
          bench.SB2.input
    | Some l ->
        synth
          []
          bench.SB2.fuel
          bench.SB2.input
end

module Synth2
  (N   : NUM)
  (Q   : PRIORITY_QUEUE
         with type priority = N.t)
  (G2  : GEOM2
         with type num = N.t)
  (M2  : MESH2
         with type num = N.t
          and type pt  = N.t * N.t)
  (C2  : CAD2
         with type num  = N.t
          and type mesh = M2.t)
  (SB2 : SYNTHBENCH
          with type mesh = M2.t
           and type prim = C2.t)
  : (SYNTH
     with type mesh  = M2.t
      and type cad   = C2.t
      and type bench = SB2.t)
= RawSynth2(N)(Q)(G2)(M2)(C2)(SB2)

module RawSynth3
  (N   : NUM)
  (Q   : PRIORITY_QUEUE
         with type priority = N.t)
  (G3  : GEOM3
         with type num = N.t)
  (M3  : MESH3
         with type num = N.t
          and type pt  = N.t * N.t * N.t)
  (C3  : CAD3
         with type num  = N.t
          and type mesh = M3.t)
  (SB3 : SYNTHBENCH
          with type mesh = M3.t
           and type prim = C3.t)
= struct

  type mesh  = M3.t
  type cad   = C3.t
  type bench = SB3.t

  type trans = G3.pt
  type rot   = N.t * N.t * N.t
  type scale = G3.pt

  type prim =
    { pcad      : cad
    ; recognize : mesh -> (trans * rot * scale) option
    }

  exception InitialMesh

  let prims : (string * cad) list ref = ref []

  (** returns the CAD with fewer number of primitives *)
  (* TODO: other ways to compare CADs? *)
  let rank_edit c1 c2 =
    compare (C3.num_prims c1)
            (C3.num_prims c2)

  let rec hole_count c =
    match c with
    | C3.Empty    -> 0
    | C3.Unit     -> 0
    | C3.Sphere   -> 0
    | C3.Cylinder -> 0
    | C3.Hexagon  -> 0
    | C3.Pentagon -> 0
    | C3.Mesh m -> 1
    | C3.Unop (_, c1) ->
        hole_count c1
    | C3.Binop (op, c1, c2) ->
        hole_count c1 + hole_count c2

  let rec union_diff t =
    match t with
    | CTree.Node xs ->
        let diff (v, ns) =
          C3.mkbinop C3.Diff (C3.Mesh v) (union_diff ns)
        in
        let diffs = List.map diff xs in
        List.fold_left (C3.mkbinop C3.Union) C3.Empty diffs

  let factor_manifolds m =
    let cs = M3.cycles m in
    if List.length cs = 1 then
      C3.Mesh m
    else
      let t = CTree.mkctree M3.contains cs
      in union_diff t

  let hole_depth c =
    let rec loop = function
      | C3.Empty    -> None
      | C3.Unit     -> None
      | C3.Sphere   -> None
      | C3.Cylinder -> None
      | C3.Hexagon  -> None
      | C3.Pentagon -> None
      | C3.Mesh _ -> Some 0
      | C3.Unop (_, c1) ->
          begin match loop c1 with
          | None   -> None
          | Some i -> Some (i + 1)
          end
      | C3.Binop (_, c1, c2) ->
          begin match loop c1, loop c2 with
          | None, None ->
              None
          | None, Some i
          | Some i, None ->
              Some (i + 1)
          | Some i, Some j ->
              Some ((Pervasives.min i j) + 1)
          end
    in loop c

  let disj_step c =
    (*print_endline "disjoint step";*)
    let rec go = function
      | C3.Empty    -> C3.Empty
      | C3.Unit     -> C3.Unit
      | C3.Sphere   -> C3.Sphere
      | C3.Cylinder -> C3.Cylinder
      | C3.Hexagon  -> C3.Hexagon
      | C3.Pentagon -> C3.Pentagon
      | C3.Mesh m ->
          factor_manifolds m
      | C3.Unop (op, c1) ->
          C3.Unop (op, go c1)
      | C3.Binop (op, c1, c2) ->
          (* TODO : develop better prioritization heuristic
           * --OR-- find way to add both options, but ensure
           *        that we avoid repeating equivalent work
           *        (e.g., perhaps using a cache?)
           *)
          begin match hole_depth c1, hole_depth c2 with
           | None,   None   -> C3.Binop (op, c1, c2)
           | None,   Some i -> C3.Binop (op, c1, go c2)
           | Some i, None   -> C3.Binop (op, go c1, c2)
           | Some i, Some j ->
               if i < j
               then C3.Binop (op, go c1, c2)
               else C3.Binop (op, c1, go c2)
          end
    in
    go c

  let splits m =
    match M3.convex_split m with
    | None -> [m]
    | Some (m1, m2) -> [m1; m2]

  let add_step c =
    let rec go = function
      | C3.Empty    -> C3.Empty
      | C3.Unit     -> C3.Unit
      | C3.Sphere   -> C3.Sphere
      | C3.Cylinder -> C3.Cylinder
      | C3.Hexagon  -> C3.Hexagon
      | C3.Pentagon -> C3.Pentagon
      | C3.Mesh m ->
          let ms = splits m in
          if List.length ms = 1 then
            C3.Mesh m
          else
            ( List.fold_left
                (C3.mkbinop C3.Union)
                C3.Empty (List.map C3.mkhole ms))
      | C3.Unop (op, c1) ->
          C3.Unop (op, go c1)
      | C3.Binop (op, c1, c2) ->
          begin match hole_depth c1, hole_depth c2 with
           | None,   None   -> C3.Binop (op, c1, c2)
           | None,   Some i -> C3.Binop (op, c1, go c2)
           | Some i, None   -> C3.Binop (op, go c1, c2)
           | Some i, Some j ->
               if i < j
               then C3.Binop (op, go c1, c2)
               else C3.Binop (op, c1, go c2)
          end
    in
    go c

  let canon m =
    let fs = M3.faces m in
    let ts =
      List.map
        (fun (p1, p2, p3) ->
          (p1, p2, p3)
            |> Util.triple_map (Util.uncurry3 G3.mkpt)
            |> Util.uncurry3 G3.mktri) fs
    in
    let fans =
      List.map
        (fun x ->
          ( x
          , G3.area_tri x
          , G3.unit_vec (G3.plane_norm (G3.pts_of_tri x)))) ts
    in
    let same_norms =
      let rec aux acc (f, a, n) =
        match acc with
        | [] -> [(n, [(f, a)])]
        | (n', fas) :: nfas' ->
            if G3.equiv_pt n n'
            then (n', (f, a) :: fas) :: nfas'
            else (n', fas) :: aux nfas' (f, a, n)
      in
      List.fold_left aux [] fans
    in
    let area_sums =
      List.map
        (fun (n, fas) ->
          match fas with
          | [] -> failwith "bad normal"
          | (f, a) :: fas' ->
              (n, (fas, List.fold_left N.add N.n0 (List.map snd fas))))
        same_norms
    in

    let sorted =
      Util.sort (fun (n1, (fas1, a1)) (n2, (fas2, a2)) -> N.cmp a1 a2)
      area_sums
    in
    let nz =
      sorted |> List.rev
             |> List.hd
             |> fst
             |> G3.unit_vec
    in
    let ny =
      List.find
        (fun (n, (fas, a)) ->
          N.equiv (G3.dot_prod (G3.unit_vec nz) (G3.unit_vec n)) N.n0)
        sorted
          |> fst
          |> G3.unit_vec
    in
    let nx = nz |> G3.cross_prod ny
                |> G3.unit_vec
    in
    let (ax, ay, az) =
      M3.euler (nx.G3.x, nx.G3.y, nx.G3.z)
               (ny.G3.x, ny.G3.y, ny.G3.z)
               (nz.G3.x, nz.G3.y, nz.G3.z)
    in
    let rotm =
      m |> M3.rotateZ(N.sub az az)
        |> M3.rotateY(N.sub ay ay)
        |> M3.rotateX(N.sub ax ax)
(*
      m |> M3.rotateZ(N.neg az)
        |> M3.rotateY(N.neg ay)
        |> M3.rotateX(N.neg ax)
*)
    in
    let rps =
      List.map (fun (x, y, z) -> G3.mkpt x y z)
               (M3.verts rotm)
    in
    let bbr   = G3.bbox rps in
    let d1    = N.sub bbr.G3.xmax bbr.G3.xmin in
    let d2    = N.sub bbr.G3.ymax bbr.G3.ymin in
    let d3    = N.sub bbr.G3.zmax bbr.G3.zmin in
    let scale = G3.mkpt d1 d2 d3 in
    let sclm  = rotm |> M3.scale ( N.div N.n1 scale.G3.x
                                 , N.div N.n1 scale.G3.y
                                 , N.div N.n1 scale.G3.z)
    in
    let sps =
      List.map (fun (x, y, z) -> G3.mkpt x y z)
               (M3.verts sclm)
    in
    let bbs  = G3.bbox sps in
    let s1   = N.add bbs.G3.xmax bbs.G3.xmin in
    let s2   = N.add bbs.G3.ymax bbs.G3.ymin in
    let s3   = N.add bbs.G3.zmax bbs.G3.zmin in
    let tran = G3.mkpt (N.div s1 N.n2)
                       (N.div s2 N.n2)
                       (N.div s3 N.n2)
    in
    let canon_m = sclm |> M3.trans ( N.neg tran.G3.x
                                   , N.neg tran.G3.y
                                   , N.neg tran.G3.z)
    in
    (* NOTE: to get back original m, you need to
      first scale (canon m) by scale, then translate
      it back to where it was. For that last step, you need
      to recompute the correct translation. We do this by
      multiplying tran and scale. *)
(*
    (canon_m, tran, (ax, ay, az), scale)
*)
    (canon_m, tran, (N.n0, N.n0, N.n0), scale)

  let sphere_recog m =
    let (cm, t, (rx, ry, rz), s) = canon m in
    let fs = M3.faces cm in
    (* TODO: this is bad, won't work with face splits *)
    if List.length fs < 200 then
      None
    else
      let ps =
        List.map (fun (x, y, z) -> G3.mkpt x y z)
                 (M3.verts m)
      in
      let lstr = string_of_int (List.length ps) in
      let orig = G3.mkpt N.n0 N.n0 N.n0 in
      let cntr = List.fold_left G3.add orig ps
                  |> Util.flip G3.div
                      (G3.mkpt (N.of_string lstr)
                               (N.of_string lstr)
                               (N.of_string lstr))
      in
      let radius = G3.dist_pt (List.hd ps) cntr in
      match List.for_all
              (fun pt ->
                N.cmp radius (G3.dist_pt pt cntr) = EQ) ps with
      | true ->
          Some (t, (rx, ry, rz), s)
      | false ->
          None

  let cube_recog m =
    let (cm, t, (rx, ry, rz), s) = canon m in
    let fs = M3.faces cm in
    let ts =
      List.map
        (fun (p1, p2, p3) ->
          (p1, p2, p3)
            |> Util.triple_map (Util.uncurry3 G3.mkpt)
            |> Util.uncurry3 G3.mktri
        )
        fs
    in
    (* all face areas should be same *)
    let a1 = G3.area_tri (List.hd ts) in
    if List.exists
      (fun t -> N.cmp a1 (G3.area_tri t) <> EQ)
      (List.tl ts)
    then
      None
    else
      let pls = List.map G3.pts_of_tri ts in
      let ns  =
        pls |> List.map
                (fun x -> G3.unit_vec (G3.plane_norm x))
            |> Util.dedup G3.cmp_pt
      in
      if List.length ns <> 6 then
        None
      else
        let prs =
          ns |> Util.choose2
             |> List.map (fun (x, y) -> G3.dot_prod x y)
             |> Util.dedup N.cmp
        in
        (* TODO: there is a better way to do this *)
        if List.length prs = 2
        && N.equiv (N.add (List.nth prs 0) (List.nth prs 1)) (N.neg N.n1)
        then
          Some (t, (rx, ry, rz), s)
        else
          None

  let cuboid_recog m =
    let (cm, t, (rx, ry, rz), s) = canon m in
    let fs = M3.faces cm in
    let ts =
      List.map
        (fun (p1, p2, p3) ->
          (p1, p2, p3)
            |> Util.triple_map (Util.uncurry3 G3.mkpt)
            |> Util.uncurry3 G3.mktri
        ) fs
    in
    let pls = List.map G3.pts_of_tri ts in
    let ns  =
      pls |> List.map (fun x -> G3.unit_vec (G3.plane_norm x))
          |> Util.dedup G3.cmp_pt
    in
    if List.length ns <> 6 then
      None
    else
      let prs =
        ns |> Util.choose2
           |> List.map (fun (x, y) -> G3.dot_prod x y)
           |> Util.dedup N.cmp
      in
      (* TODO: there is a better way to do this *)
      if List.length prs = 2
      && N.equiv (N.add (List.nth prs 0) (List.nth prs 1)) (N.neg N.n1)
      then
        let areas = List.map
                      (fun x ->
                        ( x
                        , G3.area_tri x
                        , G3.unit_vec (G3.plane_norm (G3.pts_of_tri x))))
                      ts
        in
        let arprs = Util.choose2 areas in
        if List.exists
            (fun ((f1, a1, n1), (f2, a2, n2)) ->
              (N.cmp (G3.dot_prod n1 n2) N.n1 = EQ ||
               N.cmp (G3.dot_prod n1 n2) (N.neg (N.n1)) = EQ) &&
               not (N.equiv a1 a2))
            arprs
        then
          None
        else
         Some (t, (rx, ry, rz), s)
      else
        None

  let hexagon_recog m =
    let (cm, t, (rx, ry, rz), s) = canon m in
    let fs = M3.faces cm in
    if List.length fs <> 20 then
      None
    else
      let ts =
        List.map
          (fun (p1, p2, p3) ->
            (p1, p2, p3)
              |> Util.triple_map (Util.uncurry3 G3.mkpt)
              |> Util.uncurry3 G3.mktri
          ) fs
      in
      let fans =
        List.map
          (fun x ->
            ( x
            , G3.area_tri x
            , G3.unit_vec (G3.plane_norm (G3.pts_of_tri x))))
          ts
      in
      let same_norms =
        let rec aux acc (f, a, n) =
          match acc with
          | [] -> [(n, [(f, a)])]
          | (n', fas) :: nfas' ->
              if G3.equiv_pt n n'
              then (n', (f, a) :: fas) :: nfas'
              else (n', fas) :: aux nfas' (f, a, n)
        in
        List.fold_left aux [] fans
      in
      let area_sums =
        List.map
          (fun (n, fas) ->
            match fas with
            | [] -> failwith "bad normal"
            | (f, a) :: fas' ->
                (n, (fas, List.fold_left N.add N.n0 (List.map snd fas))))
          same_norms
      in
      let sorted =
        Util.sort
          (fun (n1, (fas1, a1)) (n2, (fas2, a2)) -> N.cmp a1 a2)
          area_sums |> List.rev

      in
      let (n1, (fas1, a1)) = List.hd sorted in
      let (n2, (fas2, a2)) = List.hd (List.tl sorted) in
      let same_side_areas =
        let rest = List.tl (List.tl sorted) in
        let ar1  = snd (snd (List.hd rest)) in
        (* VERY VERY BAD DEADLINE HACK TO AVOID ROUNDING ERROR *)
        List.for_all (fun x -> N.equiv ar1 (snd (snd x)) ||
                               N.equiv (N.of_string "0.5") (snd (snd x)))
                     rest
      in
      if N.equiv (G3.magn (G3.unit_vec n1))
                 (G3.magn (G3.unit_vec n2))
      && N.equiv (G3.dot_prod (G3.unit_vec n1) (G3.unit_vec n2))
                 (N.neg N.n1)
      && same_side_areas
      then
        Some (t, (rx, ry, rz), s)
      else
        None

  let pentagon_recog m =
    None

  let cyl_recog m =
    let (cm, t, (rx, ry, rz), s) = canon m in
    let fs = M3.faces cm in
    (* TODO: this is BAD. Definitely temporary. *)
    if List.length fs <> 196 then
      None
    else
      let ts =
        List.map
          (fun (p1, p2, p3) ->
            (p1, p2, p3)
              |> Util.triple_map (Util.uncurry3 G3.mkpt)
              |> Util.uncurry3 G3.mktri
          )
          fs
      in
      let fans =
        List.map
          (fun x ->
            ( x
            , G3.area_tri x
            , G3.unit_vec (G3.plane_norm (G3.pts_of_tri x))))
          ts
      in
      let same_norms =
        let rec aux acc (f, a, n) =
          match acc with
          | [] -> [(n, [(f, a)])]
          | (n', fas) :: nfas' ->
              if G3.equiv_pt n n'
              then (n', (f, a) :: fas) :: nfas'
              else (n', fas) :: aux nfas' (f, a, n)
        in
        List.fold_left aux [] fans
      in
      let area_sums =
        List.map
          (fun (n, fas) ->
            match fas with
            | [] -> failwith "bad normal"
            | (f, a) :: fas' ->
                (n, (fas, List.fold_left N.add N.n0 (List.map snd fas))))
          same_norms
      in
      let sorted =
        Util.sort (fun (n1, (fas1, a1)) (n2, (fas2, a2)) -> N.cmp a1 a2)
        area_sums
      in
      let fstn =
        sorted |> List.rev
               |> List.hd
      in
      let sndn =
        sorted |> List.rev
               |> List.tl
               |> List.hd
      in
      if N.equiv (snd (snd fstn)) (snd (snd sndn))
      && N.equiv (G3.dot_prod (fst fstn) (fst sndn)) (N.neg N.n1)
      then
        Some (t, (rx, ry, rz), s)
      else
        None

  let rec try_recogs recogs m =
    let vs = List.map (fun x -> uncurry3 G3.mkpt x) (M3.verts m)
    in
    let vs_len = N.of_string (string_of_int (List.length vs)) in
    let cent =
      G3.div (List.fold_left G3.add (G3.mkpt N.n0 N.n0 N.n0) vs)
             (G3.mkpt vs_len vs_len vs_len)
    in
    match recogs with
    | [] -> None
    | (r, (pm, ptr, (prx, pry, prz), psc)) :: rs ->
        match r.recognize m with
        | Some (t, (rx, ry, rz), s) ->
            let m' =
              r.pcad
              |> C3.mkunop (C3.RotateZ (N.neg prz))
              |> C3.mkunop (C3.RotateY (N.neg pry))
              |> C3.mkunop (C3.RotateX (N.neg prx))
              |> C3.mkunop (C3.Scale ( N.div N.n1 psc.G3.x
                                     , N.div N.n1 psc.G3.y
                                     , N.div N.n1 psc.G3.z))
              |> C3.mkunop (C3.Trans ( N.neg ptr.G3.x
                                     , N.neg ptr.G3.y
                                     , N.neg ptr.G3.z))
              (* then obtain the actual object *)
              |> C3.mkunop (C3.Scale (s.G3.x, s.G3.y, s.G3.z))
              |> C3.mkunop (C3.RotateX rx)
              |> C3.mkunop (C3.RotateY ry)
              |> C3.mkunop (C3.RotateZ rz)
              |> C3.compile
            in
            let vs' = List.map (fun x -> uncurry3 G3.mkpt x) (M3.verts m')
            in
            let vs_len' = N.of_string (string_of_int (List.length vs')) in
            let cent' =
              G3.div (List.fold_left G3.add (G3.mkpt N.n0 N.n0 N.n0) vs')
                     (G3.mkpt vs_len' vs_len' vs_len')
            in
            Some (r.pcad
              (* first canonicalize the prim *)
              |> C3.mkunop (C3.RotateZ (N.neg prz))
              |> C3.mkunop (C3.RotateY (N.neg pry))
              |> C3.mkunop (C3.RotateX (N.neg prx))
              |> C3.mkunop (C3.Scale ( N.div N.n1 psc.G3.x
                                     , N.div N.n1 psc.G3.y
                                     , N.div N.n1 psc.G3.z))
              |> C3.mkunop (C3.Trans ( N.neg ptr.G3.x
                                     , N.neg ptr.G3.y
                                     , N.neg ptr.G3.z))
              (* then obtain the actual object *)
              |> C3.mkunop (C3.Scale (s.G3.x, s.G3.y, s.G3.z))
              |> C3.mkunop (C3.RotateX rx)
              |> C3.mkunop (C3.RotateY ry)
              |> C3.mkunop (C3.RotateZ rz)
              |> C3.mkunop (C3.Trans ( N.sub cent.G3.x cent'.G3.x
                                     , N.sub cent.G3.y cent'.G3.y
                                     , N.sub cent.G3.z cent'.G3.z)))
        | None ->
            try_recogs rs m

  let bound_prim prims m =
    let disjoint m = List.length (M3.cycles m) > 1 in
    if disjoint m then
      (*(print_endline "disjoint mesh";*)
      C3.Mesh m
      (*)*)
    else
      let (cm, t, (rx, ry, rz), s) = canon m in
      let potentials =
        List.filter
          (fun (p, (pm, ptr, (prx, pry, prz), psc)) ->
            M3.faces ( M3.diff cm pm) = [])
          prims
      in
      let bprim =
        List.map
          (fun (p, (pm, ptr, (prx, pry, prz), psc)) ->
            ( (p.pcad, (ptr, (prx, pry, prz), psc))
            , pm |> Util.flip M3.diff cm
                 |> M3.vol
            ))
          potentials
            |> Util.sort
                (fun ((p1, (t1, (rx1, ry1, rz1), s1)), v1)
                     ((p2, (t2, (rx2, ry2, rz2), s2)), v2) ->
                N.cmp v1 v2)
            |> List.hd
      in
      let bp = bprim |> fst |> fst in
      let (t1, (rx1, ry1, rz1), s1) =
        bprim |> fst |> snd
      in
      bp |> C3.mkunop (C3.RotateZ (N.neg rz1))
         |> C3.mkunop (C3.RotateY (N.neg ry1))
         |> C3.mkunop (C3.RotateX (N.neg rx1))
         |> C3.mkunop (C3.Scale ( N.div N.n1 s1.G3.x
                                , N.div N.n1 s1.G3.y
                                , N.div N.n1 s1.G3.z))
         |> C3.mkunop (C3.Trans ( N.neg t1.G3.x
                                , N.neg t1.G3.y
                                , N.neg t1.G3.z))
         |> C3.mkunop (C3.Scale (s.G3.x, s.G3.y, s.G3.z))
         |> C3.mkunop (C3.RotateX rx)
         |> C3.mkunop (C3.RotateY ry)
         |> C3.mkunop (C3.RotateZ rz)
         |> C3.mkunop (C3.Trans ( N.mul s.G3.x t.G3.x
                                , N.mul s.G3.y t.G3.y
                                , N.mul s.G3.z t.G3.z))

  let canon_prim_meshes = ref []

  let sub_step c =
    (*print_endline "subtractive step";*)
    let rec go = function
      | C3.Empty    -> C3.Empty
      | C3.Unit     -> C3.Unit
      | C3.Sphere   -> C3.Sphere
      | C3.Cylinder -> C3.Cylinder
      | C3.Hexagon  -> C3.Hexagon
      | C3.Pentagon -> C3.Pentagon
      | C3.Mesh m ->
          if M3.faces m = [] then
            C3.Empty
          else begin
            match try_recogs !canon_prim_meshes m with
            | None ->
                (*print_endline "finding bounding prim";*)
                let bb = bound_prim !canon_prim_meshes m in
                (*print_endline  "bounding primitive found";*)
                let d  = M3.diff (C3.compile bb) m in
                C3.mkbinop C3.Diff bb (C3.Mesh d)
            | Some p -> p
          end
      | C3.Unop (op, c1) ->
          C3.Unop (op, go c1)
      | C3.Binop (op, c1, c2) ->
          begin match hole_depth c1, hole_depth c2 with
           | None,   None   -> C3.Binop (op, c1, c2)
           | None,   Some i -> C3.Binop (op, c1, go c2)
           | Some i, None   -> C3.Binop (op, go c1, c2)
           | Some i, Some j ->
               if i < j
               then C3.Binop (op, go c1, c2)
               else C3.Binop (op, c1, go c2)
          end
    in
    go c

  let best1 cs =
    cs |> Util.sort C3.cmp
       |> List.rev
       |> List.hd
       |> C3.simplify

  let best2 cs =
    let f = "cads.txt" in
    List.iter
      (fun x -> Util.append_file f (C3.to_string x ^ "\n-----------\n\n"))
      cs;
    cs |> List.map (fun x -> (x, hole_count x))
       |> List.sort
            (fun (c1, h1) (c2, h2) ->
              Pervasives.compare h1 h2)
       |> List.hd
       |> fst
       |> C3.simplify

  let best3 cs =
    cs |> List.sort rank_edit
       |> List.hd
       |> C3.simplify

  let best4 c cs =
    let vols =
      List.map (fun x ->
        (x, M3.vol (M3.diff (C3.compile x) (C3.compile c)))) cs
    in
    vols |> Util.sort (fun (x1, v1) (x2, v2) -> N.cmp v1 v2)
         |> List.hd
         |> fst
         |> C3.simplify

  let step c =
   [ disj_step c
   ; sub_step c
(* ; add_step c*)
   ]

  let init () =
    let sphere_prim =
      { pcad      = List.assoc "sphere" !prims
      ; recognize = sphere_recog
      }
    in
    let cube_prim =
      { pcad      = List.assoc "cube" !prims
      ; recognize = cube_recog
      }
    in
    let cuboid_prim =
      { pcad      = List.assoc "cube" !prims
      ; recognize = cuboid_recog
      }
    in
    let cyl_prim =
      { pcad      = List.assoc "cylinder" !prims
      ; recognize = cyl_recog
      }
    in
    let pentagon_prim =
      { pcad      = List.assoc "pentagon" !prims
      ; recognize = pentagon_recog
      }
    in
    let hexagon_prim =
      { pcad      = List.assoc "hexagon" !prims
      ; recognize = hexagon_recog
      }
    in
    let primitives =
      [ cube_prim
      ; cuboid_prim
      ; hexagon_prim
      ; sphere_prim
      ; pentagon_prim
      ; cyl_prim
      ]
    in
    canon_prim_meshes :=
      List.map
        (fun p -> (p, canon (C3.compile p.pcad)))
        primitives

  let synth_c limit c =
    init();
    let none_done wl =
      let l = Q.to_list wl in
      List.for_all (fun (p, (i, c)) -> hole_count c > 0) l
    in
    let rec loop fin wl =
      match Q.pop wl with
      | None ->
          fin
      | Some (p, (i, c), rest) ->
          if i < limit && none_done wl then
            c |> step
              |> Util.dedup C3.cmp
              |> List.map (pair (i + 1))
              |> List.map (pair N.n1)
              |> Q.pushl rest
              |> loop fin
          else
            loop (c :: fin) rest
    in
    c |> pair 0
      |> pair N.n1
      |> Q.single
      |> loop []
      |> best4 c

  let synth ps f m =
    prims := ps;
    let c = synth_c f (C3.Mesh m) in
    c
(*
    match c with
    | C3.Mesh _ ->
        raise InitialMesh
    | _ -> c
*)


  let synth_b bench =
    match bench.SB3.prims with
    | None ->
        (* TODO: somehow use default prim list *)
        synth
          []
          bench.SB3.fuel
          bench.SB3.input
    | Some l ->
        synth
          []
          bench.SB3.fuel
          bench.SB3.input
end

module Synth3
  (N   : NUM)
  (Q   : PRIORITY_QUEUE
         with type priority = N.t)
  (G3  : GEOM3
         with type num = N.t)
  (M3  : MESH3
         with type num = N.t
          and type pt  = N.t * N.t * N.t)
  (C3  : CAD3
         with type num  = N.t
          and type mesh = M3.t)
  (SB3 : SYNTHBENCH
         with type mesh = M3.t
          and type prim = C3.t)
  : (SYNTH
     with type mesh  = M3.t
      and type cad   = C3.t
      and type bench = SB3.t)
= RawSynth3(N)(Q)(G3)(M3)(C3)(SB3)
