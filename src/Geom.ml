open Util
open NumSys
open LinAlg
open RStream

(** Geometry Modules *)

(* we treat points as vectors whenever it's convenient *)
module type GEOM1 = sig

  type num

  type pt =
    { x : num }

  val mkpt         : num  -> pt
  val rand         : unit -> pt
  val string_of_pt : pt -> string
  val equiv_pt     : pt -> pt -> bool
  val order_pt     : pt -> pt -> cmp
  val cmp_pt       : pt -> pt -> cmp
  val dist_pt      : pt -> pt -> num

  (** component-wise ops on points *)
  val add : pt -> pt -> pt
  val sub : pt -> pt -> pt
  val mul : pt -> pt -> pt
  val div : pt -> pt -> pt

  (** bounding boxes *)
  type bbox =
    { xmin : num
    ; xmax : num
    }

  val bbox_of_pt     : pt -> bbox
  val updt_bbox      : bbox -> pt -> bbox
  val bbox           : pt list -> bbox
  val pts_of_bbox    : bbox -> pt * pt
  val string_of_bbox : bbox -> string
  val equiv_bbox     : bbox -> bbox -> bool
  val in_bbox        : bbox -> pt -> bool

end

module Geom1 (N : NUM) : (GEOM1 with type num = N.t) = struct

  type num = N.t

  type pt =
    { x : num }

  let mkpt x =
    { x = x }

  let rand () =
    mkpt (N.rand ())

  let string_of_pt pt =
    Printf.sprintf "{x = %s}"
      (N.to_string pt.x)

  let order_pt a b =
    match N.order a.x b.x with
    | LT -> LT
    | EQ -> EQ
    | GT -> GT

  let cmp_pt a b =
    match N.cmp a.x b.x with
    | LT -> LT
    | EQ -> EQ
    | GT -> GT

  let equiv_pt a b =
    cmp_pt a b = EQ

  let dist_pt a b =
    N.abs (N.sub a.x b.x)

  let add a b =
    { x = N.add a.x b.x }

  let sub a b =
    { x = N.sub a.x b.x }

  let mul a b =
    { x = N.mul a.x b.x }

  let div a b =
    { x = N.div a.x b.x }

  (** bounding boxes *)

  type bbox =
    { xmin : num
    ; xmax : num
    }

  let bbox_of_pt pt =
    { xmin = pt.x
    ; xmax = pt.x
    }

  let updt_bbox bb pt =
    { xmin = min N.cmp bb.xmin pt.x
    ; xmax = max N.cmp bb.xmax pt.x
    }

  let bbox points =
    match points with
    | [] ->
        failwith "Geom1.bbox: empty"
    | pt :: pts ->
        List.fold_left updt_bbox (bbox_of_pt pt) pts

  let pts_of_bbox bb =
    ( mkpt bb.xmin
    , mkpt bb.xmax )

  let string_of_bbox bb =
    Printf.sprintf "{xmin = %s; xmax = %s}"
      (N.to_string bb.xmin)
      (N.to_string bb.xmax)

  let equiv_bbox bbA bbB =
    N.equiv bbA.xmin bbB.xmin &&
    N.equiv bbA.xmax bbB.xmax

  let in_bbox bb pt =
    N.cmp bb.xmin pt.x <> GT &&
    N.cmp pt.x bb.xmax <> GT

end

module type GEOM2 = sig

  type num

  (** points *)
  type pt =
    { x : num
    ; y : num
    }

  val mkpt         : num  -> num -> pt
  val rand         : unit -> pt
  val string_of_pt : pt -> string
  val equiv_pt     : pt -> pt -> bool
  val order_pt     : pt -> pt -> cmp
  val cmp_pt       : pt -> pt -> cmp
  val dist2_pt      : pt -> pt -> num
  val dist_pt      : pt -> pt -> num
  val rotate       : num  -> pt -> pt
  val collinear    : pt -> pt -> pt -> bool

  (** component-wise ops on points *)
  val add : pt -> pt -> pt
  val sub : pt -> pt -> pt
  val mul : pt -> pt -> pt
  val div : pt -> pt -> pt

  (** vector ops *)
  val dot_prod   : pt -> pt -> num
  val magn       : pt -> num
  val unit_vec   : pt -> pt

  (** directed line segments *)

  type dseg =
    { head : pt
    ; tail : pt
    }

  type turn =
    | CCW
    | COL
    | CLK

  type isect_dseg =
    | SNone
    | SCross of pt
    | SOver  of dseg

  val mkdseg         : pt -> pt -> dseg
  val string_of_dseg : dseg -> string
  val equiv_dseg     : dseg -> dseg -> bool
  val order_dseg     : dseg -> dseg -> cmp
  val cmp_dseg       : dseg -> dseg -> cmp
  val empty_dseg     : dseg -> bool
  val pts_of_dseg    : dseg -> pt * pt
  val pts_of_dsegs   : dseg list -> pt list
  val midpt          : dseg -> pt
  val len_dseg       : dseg -> num
  val on_dseg        : dseg -> pt -> bool
  val lift_ptop_dseg : (pt -> pt) -> dseg -> dseg
  val turn           : dseg -> pt -> turn
  val isect_ss       : dseg -> dseg -> isect_dseg
  val dseg_circuit   : pt list -> dseg list

  val equiv_dseg_endpoints : dseg -> dseg -> bool

  (* partitiong and joining dsegs *)
  val part_dseg  : dseg -> dseg -> dseg list
  val part_dsegs : dseg list -> dseg -> dseg list
  val join_dseg  : dseg -> dseg -> dseg option
  val join_dsegs : dseg list -> dseg option

  (** lines and rays *)

  type line
  type ray
  exception SamePoint of pt * pt

  type isect_line =
    | LNone
    | LCross of pt
    | LOver

  val mkline         : pt -> pt -> line
  val line_of_dseg   : dseg -> line
  val line_of_ray    : ray -> line
  val string_of_line : line -> string
  val equiv_line     : line -> line -> bool
  val on_line        : line -> pt -> bool
  val line_pt_dist   : line -> pt -> num
  val line_part      : line -> pt list -> (pt list * pt list * pt list)
  val isect_ll       : line -> line -> isect_line
  val isect_ls       : line -> dseg -> isect_dseg

  val mkray          : dseg -> ray
  val string_of_ray  : ray -> string
  val equiv_ray      : ray -> ray -> bool
  val on_ray         : ray -> pt -> bool
  val isect_rs       : ray -> dseg -> isect_dseg

  (** bounding boxes *)

  type bbox =
    { xmin : num
    ; ymin : num
    ; xmax : num
    ; ymax : num
    }

  val bbox_of_pt     : pt -> bbox
  val updt_bbox      : bbox -> pt -> bbox
  val bbox           : pt list -> bbox
  val pts_of_bbox    : bbox -> pt * pt
  val string_of_bbox : bbox -> string
  val equiv_bbox     : bbox -> bbox -> bool
  val in_bbox        : bbox -> pt -> bool
  val around_bbox    : bbox -> pt Stream.t

  (** polygons *)

  type pgon
  exception SelfIsect
  exception RepeatPt

  val mkpgon         : pt list -> pgon (** checks pts are a polygon *)
  val pts_of_pgon    : pgon -> pt list
  val dsegs_of_pgon  : pgon -> dseg list
  val string_of_pgon : pgon -> string
  val equiv_pgon     : pgon -> pgon -> bool
  val area           : pgon -> num

  (** hulls *)

  type hull
  exception WrongTurn

  val mkhull         : pt list -> hull (** only checks pts are a hull *)
  val hull           : pt list -> hull (** computes hull of pts *)
  val pgon_of_hull   : hull -> pgon
  val string_of_hull : hull -> string
  val equiv_hull     : hull -> hull -> bool

end

module Geom2 (N : NUM) : (GEOM2 with type num = N.t)
= struct

  type num = N.t

  (** points *)
  type pt =
    { x : num
    ; y : num
    }

  let mkpt x y =
    { x = x
    ; y = y
    }

  let rand () =
    mkpt (N.rand ()) (N.rand ())

  let string_of_pt pt =
    Printf.sprintf "{x = %s; y = %s}"
      (N.to_string pt.x)
      (N.to_string pt.y)

  let order_pt a b =
    match N.order a.x b.x with
    | LT -> LT
    | EQ -> N.order a.y b.y
    | GT -> GT

  let cmp_pt a b =
    match N.cmp a.x b.x with
    | LT -> LT
    | EQ -> N.cmp a.y b.y
    | GT -> GT

  let equiv_pt a b =
    cmp_pt a b = EQ

  (* NOTE possible numerical issues *)
  let dist2_pt a b =
    let dx  = N.sub a.x b.x in
    let dy  = N.sub a.y b.y in
    let dx2 = N.mul dx  dx  in
    let dy2 = N.mul dy  dy  in
    N.add dx2 dy2

  let dist_pt a b =
    N.sqrt (dist2_pt a b)

  (** rotate about origin *)
  let rotate deg pt =
    let r = N.rad_of_deg deg in
    let x =
      N.sub (N.mul pt.x (N.cos r))
            (N.mul pt.y (N.sin r))
    in
    let y =
      N.add (N.mul pt.x (N.sin r))
            (N.mul pt.y (N.cos r))
    in
    mkpt x y

  let add a b =
    { x = N.add a.x b.x
    ; y = N.add a.y b.y }

  let sub a b =
    { x = N.sub a.x b.x
    ; y = N.sub a.y b.y }

  let mul a b =
    { x = N.mul a.x b.x
    ; y = N.mul a.y b.y }

  let div a b =
    { x = N.div a.x b.x
    ; y = N.div a.y b.y }

  (* imagine 0 z-coords *)
  let cross_prod a b =
    N.sub (N.mul a.x b.y)
          (N.mul a.y b.x)

  let dot_prod a b =
    N.add (N.mul a.x b.x)
          (N.mul a.y b.y)

  let magn a =
    N.sqrt (dot_prod a a)

  let unit_vec a =
    if not (N.equiv N.n0 (magn a))
    then
      mkpt (N.div a.x (magn a))
           (N.div a.y (magn a))
    else
      mkpt N.n0 N.n0

  (** bounding boxes *)

  type bbox =
    { xmin : num
    ; ymin : num
    ; xmax : num
    ; ymax : num
    }

  let bbox_of_pt pt =
    { xmin = pt.x
    ; ymin = pt.y
    ; xmax = pt.x
    ; ymax = pt.y
    }

  let updt_bbox bb pt =
    { xmin = min N.cmp bb.xmin pt.x
    ; ymin = min N.cmp bb.ymin pt.y
    ; xmax = max N.cmp bb.xmax pt.x
    ; ymax = max N.cmp bb.ymax pt.y
    }

  let bbox points =
    match points with
    | [] ->
        failwith "Geom2.bbox: empty"
    | pt :: pts ->
        List.fold_left updt_bbox (bbox_of_pt pt) pts

  let pts_of_bbox bb =
    ( mkpt bb.xmin bb.ymin
    , mkpt bb.xmax bb.ymax )

  let string_of_bbox bb =
    Printf.sprintf "{%s}" @@
      String.concat "; "
        [ Printf.sprintf "xmin = %s" (N.to_string bb.xmin)
        ; Printf.sprintf "ymin = %s" (N.to_string bb.ymin)
        ; Printf.sprintf "xmax = %s" (N.to_string bb.xmax)
        ; Printf.sprintf "ymax = %s" (N.to_string bb.ymax)
        ]

  let equiv_bbox bbA bbB =
    N.equiv bbA.xmin bbB.xmin &&
    N.equiv bbA.ymin bbB.ymin &&
    N.equiv bbA.xmax bbB.xmax &&
    N.equiv bbA.ymax bbB.ymax

  let in_bbox bb pt =
    N.cmp bb.xmin pt.x <> GT &&
    N.cmp pt.x bb.xmax <> GT &&
    N.cmp bb.ymin pt.y <> GT &&
    N.cmp pt.y bb.ymax <> GT

  let stream_angles =
    (* See "Enumeration of the Rational Points
       Between 0 and 1", Edwin L. Godfrey, 1938 *)
    Stream.map (fun n ->
        let g = 0.5 +. Pervasives.sqrt (float_of_int (2 * n)) |> int_of_float
        in
        let i = n - (g * (g - 1) / 2) in
        let j = g - i + 1 in
        N.mul N.n360 (N.div (N.of_int i) (N.of_int j)))
      Stream.nats

  let around_bbox bb =
    let max_abs =
      Util.maxl N.cmp @@
        List.map N.abs
          [ bb.xmin; bb.ymin
          ; bb.xmax; bb.ymax
          ]
    in
    let outside =
      let bumped = N.add max_abs N.n2 in
      mkpt bumped bumped
    in
    Stream.map (fun d -> rotate d outside) stream_angles

  (** line segments *)

  type dseg =
    { head : pt
    ; tail : pt
    }

  type turn =
    | CCW
    | COL
    | CLK

  type isect_dseg =
    | SNone
    | SCross of pt
    | SOver  of dseg

  let mkdseg head tail =
    { head = head
    ; tail = tail
    }

  let rev_dseg s =
    { head = s.tail
    ; tail = s.head
    }

  let string_of_dseg s =
    Printf.sprintf "{head = %s; tail = %s}"
      (string_of_pt s.head)
      (string_of_pt s.tail)

  let order_dseg a b =
    match order_pt a.head b.head with
    | LT -> LT
    | EQ -> order_pt a.tail b.tail
    | GT -> GT

  let cmp_dseg a b =
    match cmp_pt a.head b.head with
    | LT -> LT
    | EQ -> cmp_pt a.tail b.tail
    | GT -> GT

  let equiv_dseg a b =
    cmp_dseg a b = EQ

  let equiv_dseg_endpoints a b =
    equiv_dseg a b ||
    equiv_dseg a (rev_dseg b)

  let empty_dseg s =
    equiv_pt s.head s.tail

  let pts_of_dseg s =
    (s.head, s.tail)

  let pts_of_dsegs ss =
    ss |> Util.flatmap (fun s -> [s.head; s.tail])
       |> Util.dedup cmp_pt

  let midpt s =
    let d =
        { x = N.n2
        ; y = N.n2 }
    in
    div (add s.head s.tail) d

  let len_dseg s =
    dist_pt s.head s.tail

  (* we use ax + by = c as the equation of a 2D line *)
  let line_eqn (pA, pB) =
    let a = N.sub pB.y pA.y in
    let b = N.sub pA.x pB.x in
    let c = N.add (N.mul a pA.x)
                  (N.mul b pA.y) in
    (a, b, c)

  (* equation and in_bbox based on_dseg *)
  let on_dseg s pt =
    let (a, b, c) =
      line_eqn (pts_of_dseg s)
    in
    let c' =
      N.add (N.mul a pt.x)
            (N.mul b pt.y)
    in
    if N.equiv c c' then
      [s] |> pts_of_dsegs
          |> bbox
          |> Util.flip in_bbox pt
    else
      false

  (* triangle inequality based on_dseg *)
  let on_dseg' s pt =
    let d1 = dist_pt s.head pt in
    let d2 = dist_pt s.tail pt in
    N.equiv (len_dseg  s)
            (N.add d1 d2)

  let lift_ptop_dseg op s =
    { head = op s.head
    ; tail = op s.tail }

  let turn seg pt =
    let dir =
      cross_prod
        (sub seg.tail seg.head)
        (sub pt       seg.head)
    in
    match N.cmp dir N.n0 with
    | LT -> CLK
    | EQ -> COL
    | GT -> CCW

  let collinear pA pB pC =
    turn (mkdseg pA pB) pC = COL

  let dseg_circuit pts =
    pts |> Util.rotateL
        |> List.combine pts
        |> List.map (Util.uncurry mkdseg)

  (** lines *)

  type line =
    Line of pt * pt

  type ray =
    Ray of dseg

  exception SamePoint of pt * pt

  type isect_line =
    | LNone
    | LCross of pt
    | LOver

  let mkline pA pB =
    if equiv_pt pA pB then
      raise (SamePoint (pA, pB))
    else
      Line (pA, pB)

  let mkray s =
    if equiv_pt s.head s.tail then
      raise (SamePoint (s.head, s.tail))
    else
      Ray s

  let line_pts = function Line (pA, pB) ->
    (pA, pB)

  let ray_dseg = function Ray s ->
    s

  let line_of_dseg s =
    mkline s.head s.tail

  let line_of_ray r =
    line_of_dseg (ray_dseg r)

  let string_of_line l =
    let (pA, pB) = line_pts l in
    Printf.sprintf "Line(%s, %s)"
      (string_of_pt pA)
      (string_of_pt pB)

  let string_of_ray r =
    let s = ray_dseg r in
    Printf.sprintf "Ray(%s)"
      (string_of_dseg s)

  (* TODO add symmetry tests *)
  (* TODO add collinear "symmetry" test *)
  let equiv_line lA lB =
    let (pA1, pA2) = line_pts lA in
    let (pB1, pB2) = line_pts lB in
    collinear pA1 pA2 pB1 &&
    collinear pA1 pA2 pB2

  let on_line l pt =
    let (pA, pB) = line_pts l in
    collinear pA pB pt

  let line_pt_dist l pt =
    let (a, b) = line_pts l in
    let dy = N.sub b.y a.y in
    let dx = N.sub b.x a.x in
    let n =
      N.add (N.sub (N.mul dy pt.x)
                   (N.mul dx pt.y))
            (N.sub (N.mul b.x a.y)
                   (N.mul b.y a.x))
    in
    let d =
      N.add (N.mul dy dy)
            (N.mul dx dx)
    in
    N.div (N.abs n)
          (N.sqrt d)

  (* NOTE this (sort of) breaks the line abstraction
   * the notion of turn here depends on the order of pts in the line *)
  let line_part l points =
    let s =
      l |> line_pts
        |> Util.uncurry mkdseg
    in
    let rec loop (ccws, cols, clks) = function
      | [] -> (ccws, cols, clks)
      | pt :: pts ->
          begin match turn s pt with
          | CCW -> loop (pt :: ccws, cols, clks) pts
          | COL -> loop (ccws, pt :: cols, clks) pts
          | CLK -> loop (ccws, cols, pt :: clks) pts
          end
    in
    loop ([], [], []) points

  (* TODO: should we reimplement this like Geom3? *)
  let on_ray r pt =
    (* on underlying line *)
    on_line (line_of_ray r) pt &&
    (* on ray side *)
    let s = ray_dseg r in
    not (on_dseg (mkdseg s.tail pt) s.head)

  (* TODO add symmetry tests *)
  let equiv_ray rA rB =
    let sA = ray_dseg rA in
    let sB = ray_dseg rB in
    (* same start *)
    equiv_pt sA.head sB.head &&
    (* same direction *)
    on_ray rA sB.tail

  (* TODO test symmetry *)
  let isect_ll l1 l2 =
    if equiv_line l1 l2 then
      LOver
    else
      let (a1, b1, c1) =
        line_eqn (line_pts l1) in
      let (a2, b2, c2) =
        line_eqn (line_pts l2) in
      let det = N.sub (N.mul a1 b2)
                      (N.mul a2 b1) in
      if N.equiv det N.n0 then
        (* parallel but not equivalent *)
        LNone
      else
        let x = N.div (N.sub (N.mul b2 c1)
                             (N.mul b1 c2))
                      det in
        let y = N.div (N.sub (N.mul a1 c2)
                             (N.mul a2 c1))
                      det in
        LCross (mkpt x y)

  let isect_ls l s =
    match isect_ll l (line_of_dseg s) with
    | LNone ->
        SNone
    | LCross pt ->
        if on_dseg s pt
        then SCross pt
        else SNone
    | LOver ->
        SOver s

  (* NOTE: segment direction correctly preserved! *)
  let isect_rs r s =
    match isect_ls (line_of_ray r) s with
    | SNone ->
        SNone
    | SCross pt ->
        if on_ray r pt
        then SCross pt
        else SNone
    | SOver {head = h; tail = t} ->
        let rH = (ray_dseg r).head in
        begin match on_ray r h, on_ray r t with
        | true,  true  -> SOver (mkdseg h  t)
        | true,  false -> SOver (mkdseg h rH)
        | false, true  -> SOver (mkdseg rH t)
        | false, false -> SNone
        end

  let isect_ss a b =
    match isect_ll (line_of_dseg a) (line_of_dseg b) with
    | LNone ->
        SNone
    | LCross pt ->
        if on_dseg a pt
        && on_dseg b pt
        then SCross pt
        else SNone
    | LOver ->
        (* NOTE segs collinear! *)
        let overlap =
          on_dseg a b.head ||
          on_dseg a b.tail ||
          on_dseg b a.head ||
          on_dseg b a.tail
        in
        if overlap then
          begin match
            sort cmp_pt
              [ a.head ; a.tail
              ; b.head ; b.tail
              ]
          with
          | [p1; p2; p3; p4] ->
              (* TODO may not preserve direction! *)
              SOver (mkdseg p2 p3)
          | _ ->
              failwith "Geom2.isect_ss: bogus"
          end
        else
          SNone

  (* give the parts of b as partitioned by a *)
  let part_dseg a b =
    let parts =
      match isect_ss a b with
      | SNone ->
          [b]
      | SCross pt ->
          [ mkdseg b.head pt
          ; mkdseg pt b.tail ]
      | SOver o ->
          let dhh = dist_pt b.head o.head in
          let dht = dist_pt b.head o.tail in
          if N.cmp dhh dht <> GT then
            [ mkdseg b.head o.head
            ; mkdseg o.head o.tail
            ; mkdseg o.tail b.tail ]
          else
            [ mkdseg b.head o.tail
            ; mkdseg o.tail o.head
            ; mkdseg o.head b.tail ]
    in
    match List.filter
            (Util.notp empty_dseg)
            parts with
    | [] -> [b]
    | ps -> ps

  let rec part_dsegs segs b =
    match segs with
    | [] -> [b]
    | s :: ss ->
        begin match
          b |> part_dseg s
            |> Util.flatmap (part_dsegs ss)
            |> List.filter (Util.notp empty_dseg)
        with
        | [] -> [b]
        | ps -> ps
        end

  let join_dseg a b =
    if equiv_pt a.tail b.head
    && collinear a.head a.tail b.tail
    && not (on_dseg a b.tail)
    && not (on_dseg b a.head) then
      Some (mkdseg a.head b.tail)
    else
      None

  let join_dsegs segs =
    let aux om s =
      match om with
      | None -> None
      | Some m -> join_dseg m s
    in
    match segs with
    | [] ->
        failwith "Geom2.join_dsegs: empty"
    | s :: ss ->
        List.fold_left aux (Some s) ss

  (* TODO test:
   *   forall segs b,
   *      ps = part_dsegs segs b ->
   *      exists b',
   *        Some b' = join_dsegs ps /\
   *        equiv_dseg b b'
   *)


  (** polygons *)

  let only_vertex_isects ss =
    let no_split (sa, sb) =
      match
        part_dseg sb sa,
        part_dseg sa sb
      with
      | [_], [_] -> true
      |  _ ,  _  -> false
    in
    ss |> Util.choose2
       |> List.for_all no_split

  type pgon =
    Pgon of pt list

  exception SelfIsect
  exception RepeatPt

  let mkpgon pts =
    let c = dseg_circuit pts in
    if only_vertex_isects c then
      let n_uniq =
        (* TODO what about equiv points? *)
        pts |> Util.dedup cmp_pt
            |> List.length
      in
      if List.length pts = n_uniq then
        (* canonicalize up to reverse *)
        (* NOTE: depends on no repeats *)
        let (_, i) = Util.minli cmp_pt pts in
        let (a, b) = Util.split i pts in
        Pgon (b @ a)
      else
        raise RepeatPt
    else
      raise SelfIsect

  let pts_of_pgon = function Pgon pts ->
    pts

  let dsegs_of_pgon p =
    p |> pts_of_pgon
      |> dseg_circuit

  let string_of_pgon p =
    p |> pts_of_pgon
      |> List.map string_of_pt
      |> String.concat "; "
      |> Printf.sprintf "Pgon([%s])"

  let equiv_pgon a b =
    let ptsA = pts_of_pgon a in
    let ptsB = pts_of_pgon b in
    try
      let eqF =
        List.for_all
          (Util.uncurry equiv_pt)
          (List.combine ptsA ptsB)
      in
      let eqR =
        List.for_all
          (Util.uncurry equiv_pt)
          (List.combine ptsA (List.rev ptsB))
      in
      eqF || eqR
    with Invalid_argument _ ->
      false

  let area pgon =
    let dsegs = dsegs_of_pgon pgon in
    let pts   = List.map pts_of_dseg dsegs in
    let pt1s  =
      Util.flatmap
        (fun (p1, p2) -> [p1; p2])
        pts
    in
    let pt2s  = Util.rotateL pt1s in
    let cir_pts =
      List.combine pt1s pt2s in
    let add_part =
      List.fold_left
        (fun acc (p1, p2) ->
          N.add acc (N.mul p1.x p2.y))
      N.n0
      cir_pts
    in
    let sub_part =
      List.fold_left
        (fun acc (p1, p2) ->
          N.add acc (N.mul p1.y p2.x))
      N.n0
      cir_pts
    in
    sub_part
      |> N.sub add_part
      |> N.abs
      |> Util.flip N.div N.n2

  (** hulls *)

  let allCLK a =
    let isCLK (m, n, p) =
      turn (mkdseg m n) p = CLK
    in
    let b = Util.rotateL a in
    let c = Util.rotateL b in
    List.for_all isCLK
      (Util.triples a b c)

  type hull =
    Hull of pgon

  exception WrongTurn

  let mkhull pts =
    if allCLK pts then
      Hull (mkpgon pts)
    else
      let rpts = List.rev pts in
      if allCLK rpts then
        Hull (mkpgon rpts)
      else
        raise WrongTurn

  let in_tri (v1, v2, v3) pt =
    let t1 = turn (mkdseg v1 v2) pt in
    let t2 = turn (mkdseg v2 v3) pt in
    let t3 = turn (mkdseg v3 v1) pt in
    (t1 = t2 && t2 = t3)
    || (t1 = COL && in_bbox (bbox [v1; v2]) pt)
    || (t2 = COL && in_bbox (bbox [v2; v3]) pt)
    || (t3 = COL && in_bbox (bbox [v3; v1]) pt)

  let hull pts =
    let ldst_cmp l p q =
      N.cmp (line_pt_dist l p)
            (line_pt_dist l q)
    in
    let rec loop (a, b) = function
      | [] -> []
      | cs ->
          let (piv, cs) =
            Util.extract_max
              (ldst_cmp (mkline a b))
              cs
          in
          let cs =
            List.filter
              (Util.notp (in_tri (a, piv, b)))
              cs
          in
          let (ccws, _, clks) =
            line_part (mkline a piv) cs
          in
          loop (a, piv) ccws
          @ piv ::
          loop (piv, b) clks
    in
    let (l, pts) = Util.extract_min cmp_pt pts in
    let (r, pts) = Util.extract_max cmp_pt pts in
    let (ccws, _, clks) =
      line_part (mkline l r) pts
    in
    mkhull @@
    l :: loop (l, r) ccws @
    r :: loop (r, l) clks

  let pgon_of_hull = function Hull p ->
    p

  let string_of_hull h =
    h |> pgon_of_hull
      |> string_of_pgon
      |> Printf.sprintf "Hull(%s)"

  let equiv_hull a b =
    equiv_pgon
      (pgon_of_hull a)
      (pgon_of_hull b)

end

module type RAWGEOM3 = sig

  type num

  (** points *)
  type pt =
    { x : num
    ; y : num
    ; z : num
    }

  val mkpt         : num  -> num -> num -> pt
  val rand         : unit -> pt
  val string_of_pt : pt  -> string
  val equiv_pt     : pt  -> pt -> bool
  val order_pt     : pt  -> pt -> cmp
  val cmp_pt       : pt  -> pt -> cmp
  val dist2_pt     : pt  -> pt -> num
  val dist_pt      : pt  -> pt -> num
  val rotateX      : num -> pt -> pt
  val rotateY      : num -> pt -> pt
  val rotateZ      : num -> pt -> pt
  val collinear    : pt ->  pt -> pt -> bool
  val coplanar     : pt ->  pt -> pt -> pt -> bool

  (** component-wise ops on points *)
  val add : pt -> pt -> pt
  val sub : pt -> pt -> pt
  val mul : pt -> pt -> pt
  val div : pt -> pt -> pt

  val scale : num -> pt -> pt

  (** vector ops *)
  val cross_prod : pt -> pt -> pt
  val dot_prod   : pt -> pt -> num
  val magn       : pt -> num
  val unit_vec   : pt -> pt

  val best_effort_unit_vec : pt -> pt

  (** bounding boxes *)

  type bbox =
    { xmin : num
    ; ymin : num
    ; zmin : num
    ; xmax : num
    ; ymax : num
    ; zmax : num
    }

  (** Raised by bbox if given list is empty *)
  exception BBoxOfEmpty

  val bbox_of_pt     : pt -> bbox
  val updt_bbox      : bbox -> pt -> bbox
  val bbox           : pt list -> bbox
  val pts_of_bbox    : bbox -> pt * pt
  val string_of_bbox : bbox -> string
  val equiv_bbox     : bbox -> bbox -> bool
  val in_bbox        : bbox -> pt -> bool
  val around_bbox    : bbox -> pt Stream.t

  (** directed line segments *)

  type dseg =
    { head : pt
    ; tail : pt
    }

  type isect_dseg =
    (* parallel and skew are both SNone *)
    | SNone
    | SCross of pt
    | SOver  of dseg

  val mkdseg          : pt -> pt -> dseg
  val string_of_dseg  : dseg -> string
  val threejs_of_dseg : string -> dseg -> string
  val equiv_dseg      : dseg -> dseg -> bool
  val order_dseg      : dseg -> dseg -> cmp
  val cmp_dseg        : dseg -> dseg -> cmp
  val empty_dseg      : dseg -> bool
  val pts_of_dseg     : dseg -> pt * pt
  val pts_of_dsegs    : dseg list -> pt list
  val midpt           : dseg -> pt
  val len_dseg        : dseg -> num
  val on_dseg         : dseg -> pt -> bool
  val lift_ptop_dseg  : (pt -> pt) -> dseg -> dseg
  val isect_ss        : dseg -> dseg -> isect_dseg

  val equiv_dseg_endpoints : dseg -> dseg -> bool

  (** lines and rays *)

  type line
  type ray
  exception SamePoint of pt * pt

  type isect_line =
    (* parallel and skew are both LNone *)
    | LNone
    | LCross of pt
    | LOver

  val mkline         : pt -> pt -> line
  val line_of_dseg   : dseg -> line
  val line_of_ray    : ray -> line
  val string_of_line : line -> string
  val threejs_of_line : string -> line -> string
  val equiv_line     : line -> line -> bool
  val on_line        : line -> pt -> bool
  val line_pt_dist   : line -> pt -> num
  val isect_ll       : line -> line -> isect_line
  val isect_ls       : line -> dseg -> isect_dseg

  val isect_line_equiv : isect_line -> isect_line -> bool

  (* TODO: for testing *)
  val string_of_isect_ls : isect_dseg -> string
  val string_of_isect_ln : isect_line -> string

  val mkray          : dseg -> ray
  val string_of_ray  : ray -> string
  val equiv_ray      : ray -> ray -> bool
  val on_ray         : ray -> pt -> bool
  val isect_rs       : ray -> dseg -> isect_dseg

  (** triangles *)
  (* TODO: convention is to use A B C (caps) *)
  type tri =
    { a : pt
    ; b : pt
    ; c : pt
    }

  type side =
    | POS
    | NEG
    | COP

  type isect_tln =
    | TLThru of pt
    | TLCut  of dseg
    | TLNone

  type isect_tsg =
    | TSThru of pt
    | TSCut  of dseg
    | TSNone

  val mktri          : pt -> pt -> pt -> tri
  val string_of_tri  : tri -> string
  val threejs_of_tri : string -> tri -> string
  val equiv_tri      : tri -> tri -> bool
  val order_tri      : tri -> tri -> cmp
  val cmp_tri        : tri -> tri -> cmp
  val empty_tri      : tri -> bool
  val pts_of_tri     : tri -> pt * pt * pt
  val pts_of_tris    : tri list -> pt list
  val dsegs_of_tri   : tri -> dseg * dseg * dseg
  val centroid       : tri -> pt
  val area_tri       : tri -> num
  val on_tri         : tri -> pt   -> bool
  val lift_ptop_tri  : (pt -> pt)  -> tri -> tri
  val side_plane     : tri -> pt   -> side
  val isect_tl       : tri -> line -> isect_tln
  val isect_ts       : tri -> dseg -> isect_tsg
  val isect_tr       : tri -> ray  -> isect_tln

  val equiv_isect_tl : isect_tln -> isect_tln -> bool
  val equiv_isect_ts : isect_tsg -> isect_tsg -> bool

  (* TODO: for testing *)
  val string_of_isect_ts  : isect_tsg -> string
  val string_of_isect_tlr : isect_tln -> string

  (* partitioning and joining tris *)

  val part_ts : tri -> dseg -> tri list

  (** split second argument with respect to first argument *)
  val part_tri  : tri -> tri -> tri list
  val part_tris : tri list -> tri -> tri list

  (** tetrahedrons *)
  type tdron
  exception TooManyUniquePoints of pt list

  val mktdron         : tri -> tri -> tri -> tri -> tdron
  val tris_of_tdron   : tdron -> tri * tri * tri * tri
  val pts_of_tdron    : tdron -> pt * pt * pt * pt
  val pts_of_tdrons   : tdron list  -> pt list
  val string_of_tdron : tdron -> string
  val in_tdron        : tdron -> pt -> bool
  val tetraVol        : pt -> pt -> pt -> pt -> num

  (** planes *)

  type plane
  exception CollinearPlanePoints of pt * pt * pt

  type isect_plane =
    | PNone
    | POver
    | PCross of line

  type isect_pln =
    | PLThru of pt
    | PLNone
    | PLOn

  val mkplane          : pt -> pt -> pt -> plane
  val plane_of_tri     : tri -> plane
  val string_of_plane  : plane -> string
  val equiv_plane      : plane -> plane -> bool
  val on_plane         : plane -> pt -> bool
  val plane_pt_dist    : plane -> pt -> num
  val isect_pl         : plane -> line  -> isect_pln
  val plane_angle      : plane -> plane -> num
  val plane_eqn        : pt * pt * pt -> num * num * num * num
  val plane_norm       : pt * pt * pt -> pt

  (* TODO: for testing *)
  val string_of_isect_pl  : isect_pln -> string
  val equiv_isect_pln : isect_pln -> isect_pln -> bool

  (** polyhedrons *)

  type pdon
  exception SelfIsect
  exception RepeatTri

  val mkpdon         : tri list -> pdon (** checks tris are a polyhedron *)
  val pts_of_pdon    : pdon -> pt list
  val tris_of_pdon   : pdon -> tri list
  val string_of_pdon : pdon -> string
  val equiv_pdon     : pdon -> pdon -> bool

  (** hulls *)

  type hull
  exception BadHull

  val mkhull         : tri list -> hull (** only checks tris are a hull *)
  val hull           : pt  list -> hull (** computes hull of pts *)
  val pdon_of_hull   : hull -> pdon
  val string_of_hull : hull -> string
  val equiv_hull     : hull -> hull -> bool

  val forget_pt : pt -> pt
  val forget_tri : tri -> tri
end

module RawGeom3
  (N : NUM)
  (M : MATRIX with type num = N.t) : (RAWGEOM3 with type num = N.t)
= struct
  include MakeTaggedLogging(struct let tag = "Geom3" end)

  module Mat = Matrix (N)

  type num = N.t

  (** points *)

  type pt =
    { x : num
    ; y : num
    ; z : num
    }

  let mkpt x y z =
    { x = x
    ; y = y
    ; z = z
    }

  let rand () =
    mkpt (N.rand ())
         (N.rand ())
         (N.rand ())

  let string_of_pt pt =
    Printf.sprintf "{x = %s, y = %s, z = %s}"
      (N.to_string pt.x)
      (N.to_string pt.y)
      (N.to_string pt.z)

  let order_pt a b =
    match N.order a.x b.x with
    | LT -> LT
    | EQ -> ( match N.order a.y b.y with
              | LT -> LT
              | EQ -> N.order a.z b.z
              | GT -> GT)
    | GT -> GT

  let cmp_pt a b =
    match N.cmp a.x b.x with
    | LT -> LT
    | EQ -> ( match N.cmp a.y b.y with
              | LT -> LT
              | EQ -> N.cmp a.z b.z
              | GT -> GT)
    | GT -> GT

  let equiv_pt a b =
    cmp_pt a b = EQ

  let dist2_pt a b =
    let dx  = N.sub a.x b.x in
    let dy  = N.sub a.y b.y in
    let dz  = N.sub a.z b.z in
    let dx2 = N.mul dx dx   in
    let dy2 = N.mul dy dy   in
    let dz2 = N.mul dz dz   in
    N.add dx2 (N.add dy2 dz2)

  let dist_pt a b =
    N.sqrt (dist2_pt a b)

  let rotateX deg pt =
    let r = N.rad_of_deg deg in
    let y =
      N.sub (N.mul pt.y (N.cos r))
            (N.mul pt.z (N.sin r))
    in
    let z =
      N.add (N.mul pt.z (N.cos r))
            (N.mul pt.y (N.sin r))
    in
    mkpt pt.x y z

  let rotateY deg pt =
    let r = N.rad_of_deg deg in
    let x =
      N.add (N.mul pt.x (N.cos r))
            (N.mul pt.z (N.sin r))
    in
    let z =
      N.sub (N.mul pt.z (N.cos r))
            (N.mul pt.x (N.sin r))
    in
    mkpt x pt.y z

  let rotateZ deg pt =
    let r = N.rad_of_deg deg in
    let x =
      N.sub (N.mul pt.x (N.cos r))
            (N.mul pt.y (N.sin r))
    in
    let y =
      N.add (N.mul pt.x (N.sin r))
            (N.mul pt.y (N.cos r))
    in
    mkpt x y pt.z

  let add a b =
    { x = N.add a.x b.x
    ; y = N.add a.y b.y
    ; z = N.add a.z b.z }

  let sub a b =
    { x = N.sub a.x b.x
    ; y = N.sub a.y b.y
    ; z = N.sub a.z b.z }

  let mul a b =
    { x = N.mul a.x b.x
    ; y = N.mul a.y b.y
    ; z = N.mul a.z b.z }

  let div a b =
    { x = N.div a.x b.x
    ; y = N.div a.y b.y
    ; z = N.div a.z b.z }

  let scale a v =
    { x = N.mul a v.x
    ; y = N.mul a v.y
    ; z = N.mul a v.z }

  let dot_prod a b =
    let p = mul a b in
    N.add p.x (N.add p.y p.z)

  let magn a =
    N.sqrt (dot_prod a a)

  let cross_prod a b =
    let cx = N.sub (N.mul a.y b.z)
                   (N.mul a.z b.y)
    in
    let cy = N.sub (N.mul a.z b.x)
                   (N.mul a.x b.z)
    in
    let cz = N.sub (N.mul a.x b.y)
                   (N.mul a.y b.x)
    in
    mkpt cx cy cz

  let origin =
    mkpt N.n0 N.n0 N.n0

  let is_origin p =
    N.equiv N.n0 p.x &&
    N.equiv N.n0 p.y &&
    N.equiv N.n0 p.z

  let unit_vec v =
    let mv = magn v in
    if N.equiv N.n0 mv
    then origin
    else mkpt (N.div v.x mv)
              (N.div v.y mv)
              (N.div v.z mv)

  (** Returns a scalar multiple of [v] with magnitude between 1/10 and 10,
      without calling N.sqrt.

      This is useful for when a vector of approximately unit length is
      numerically desirable, but the number system does not support sqrt
      (eg, our exact arithmetic library).
  *)
  let best_effort_unit_vec v =
    let d =
      [v.x; v.y; v.z]
        |> List.map N.abs
        |> maxl N.order
    in
    if N.equiv N.n0 d
    then origin
    else mkpt (N.div v.x d)
              (N.div v.y d)
              (N.div v.z d)

  (** bounding boxes *)

  type bbox =
    { xmin : num
    ; ymin : num
    ; zmin : num
    ; xmax : num
    ; ymax : num
    ; zmax : num
    }

  let bbox_of_pt pt =
    { xmin = pt.x
    ; ymin = pt.y
    ; zmin = pt.z
    ; xmax = pt.x
    ; ymax = pt.y
    ; zmax = pt.z
    }

  let updt_bbox bb pt =
    { xmin = min N.cmp bb.xmin pt.x
    ; ymin = min N.cmp bb.ymin pt.y
    ; zmin = min N.cmp bb.zmin pt.z
    ; xmax = max N.cmp bb.xmax pt.x
    ; ymax = max N.cmp bb.ymax pt.y
    ; zmax = max N.cmp bb.zmax pt.z
    }

  exception BBoxOfEmpty

  let bbox points =
    match points with
    | [] ->
       raise BBoxOfEmpty
    | pt :: pts ->
        List.fold_left updt_bbox (bbox_of_pt pt) pts

  let pts_of_bbox bb =
    ( mkpt bb.xmin bb.ymin bb.zmin
    , mkpt bb.xmax bb.ymax bb.zmax )

  let string_of_bbox bb =
    Printf.sprintf "{%s}" @@
      String.concat "; "
        [ Printf.sprintf "xmin = %s" (N.to_string bb.xmin)
        ; Printf.sprintf "ymin = %s" (N.to_string bb.ymin)
        ; Printf.sprintf "zmin = %s" (N.to_string bb.zmin)
        ; Printf.sprintf "xmax = %s" (N.to_string bb.xmax)
        ; Printf.sprintf "ymax = %s" (N.to_string bb.ymax)
        ; Printf.sprintf "zmax = %s" (N.to_string bb.zmax)
        ]

  let equiv_bbox bbA bbB =
    N.equiv bbA.xmin bbB.xmin &&
    N.equiv bbA.ymin bbB.ymin &&
    N.equiv bbA.zmin bbB.zmin &&
    N.equiv bbA.xmax bbB.xmax &&
    N.equiv bbA.ymax bbB.ymax &&
    N.equiv bbA.zmax bbB.zmax

  let in_bbox bb pt =
    N.cmp bb.xmin pt.x <> GT &&
    N.cmp pt.x bb.xmax <> GT &&
    N.cmp bb.ymin pt.y <> GT &&
    N.cmp pt.y bb.ymax <> GT &&
    N.cmp bb.zmin pt.z <> GT &&
    N.cmp pt.z bb.zmax <> GT

  let stream_angles =
    (* See "Enumeration of the Rational Points Between
       0 and 1", Edwin L. Godfrey, 1938 *)
    Stream.map (fun n ->
      let g =
        0.5 +. Pervasives.sqrt (float_of_int (2 * n))
        |> int_of_float
      in
      let i = n - (g * (g - 1) / 2) in
      let j = g - i + 1 in
      N.mul N.n360 (N.div (N.of_int i) (N.of_int j))) Stream.nats

  let around_bbox bb =
    let max_abs =
      Util.maxl N.cmp @@
        List.map N.abs
          [ bb.xmin; bb.ymin; bb.zmin
          ; bb.xmax; bb.ymax; bb.zmax
          ]
    in
    let outside =
      let bumped = N.add max_abs N.n2 in
      mkpt bumped bumped bumped
    in
    Stream.map
      (fun d ->
        rotateZ d (rotateY d (rotateX d outside)))
      stream_angles

  (** line segments *)

  type dseg =
    { head : pt
    ; tail : pt
    }

  type isect_dseg =
    | SNone
    | SCross of pt
    | SOver  of dseg

  let mkdseg head tail =
    { head = head
    ; tail = tail
    }

  let rev_dseg s =
    { head = s.tail
    ; tail = s.head
    }

  module ThreeJS =
  struct
    let string_of_point (x, y, z) =
      Printf.sprintf "(%s, %s, %s)"
        (N.to_string x)
        (N.to_string y)
        (N.to_string z)

    let of_point p =
      Printf.sprintf "new THREE.Vector3%s"
                     (string_of_point p)

    let add_point nm p =
      Printf.sprintf "addPoint(\"%s\", %s);" nm
                     (of_point p)

    let add_line nm p1 p2 =
      Printf.sprintf "addLine(\"%s\", %s, %s);" nm
                     (of_point p1)
                     (of_point p2)

    let add_face nm p1 p2 p3 =
      Printf.sprintf "addFace(\"%s\", %s, %s, %s);" nm
                     (of_point p1)
                     (of_point p2)
                     (of_point p3)
  end

  let string_of_dseg s =
    Printf.sprintf "{head = %s; tail = %s}"
      (string_of_pt s.head)
      (string_of_pt s.tail)

  let threejs_of_dseg nm s =
    let open ThreeJS in
    let (a, b) =
      (s.head, s.tail)
        |> Util.pair_map (fun p -> (p.x, p.y, p.z))
    in
    [ add_point nm a
    ; add_point nm b
    ; ""
    ; add_line nm a b
    ] |> String.concat "\n"


  let order_dseg a b =
    match order_pt a.head b.head with
    | LT -> LT
    | EQ -> order_pt a.tail b.tail
    | GT -> GT

  let cmp_dseg a b =
    match cmp_pt a.head b.head with
    | LT -> LT
    | EQ -> cmp_pt a.tail b.tail
    | GT -> GT

  let equiv_dseg a b =
    cmp_dseg a b = EQ

  let equiv_dseg_endpoints a b =
    equiv_dseg a b ||
    equiv_dseg a (rev_dseg b)

  let empty_dseg s =
    equiv_pt s.head s.tail

  let pts_of_dseg s =
    (s.head, s.tail)

  let pts_of_dsegs ss =
    ss |> Util.flatmap (fun s -> [s.head; s.tail])
       |> Util.dedup cmp_pt

  let midpt s =
    let d =
        { x = N.n2
        ; y = N.n2
        ; z = N.n2
        }
    in
    div (add s.head s.tail) d

  let len_dseg s =
    dist_pt s.head s.tail

  let coords_of_pt p =
    (p.x, p.y, p.z)

  (* we use vector equation of a 3D line *)
  let line_eqn (pA, pB) =
    ( coords_of_pt pA
    , coords_of_pt (best_effort_unit_vec (sub pB pA)))

  let lift_ptop_dseg op s =
    { head = op s.head
    ; tail = op s.tail }

  let collinear pA pB pC =
    (* best_effort_unit_vec ensures the code is scale-independent;
       otherwise the cross product would be equivalent to zero for
       nearby-enough points, no matter their relative position. *)
    let vAB = best_effort_unit_vec (sub pB pA) in
    let vAC = best_effort_unit_vec (sub pC pA) in
    is_origin (cross_prod vAB vAC)

  let on_dseg s pt =
    collinear s.head pt s.tail &&
    let l = dist2_pt s.head s.tail in
    N.cmp (dist2_pt s.head pt) l <> GT &&
    N.cmp (dist2_pt s.tail pt) l <> GT

  (* triangle inequality based on_dseg *)
  let on_dseg' s pt =
    let d1 = dist_pt s.head pt in
    let d2 = dist_pt s.tail pt in
    N.equiv (len_dseg  s)
            (N.add d1 d2)

  (** lines *)

  type line =
    Line of pt * pt

  type isect_line =
    | LNone
    | LCross of pt
    | LOver

  type ray =
    Ray of dseg

  exception SamePoint of pt * pt

  let mkline pA pB =
    if equiv_pt pA pB then
      raise (SamePoint (pA, pB))
    else
      Line (pA, pB)

  let mkray s =
    if equiv_pt s.head s.tail then
      raise (SamePoint (s.head, s.tail))
    else
      Ray s

  let line_pts = function Line (pA, pB) ->
    (pA, pB)

  let ray_dseg = function Ray s ->
    s

  let string_of_isect_ln i =
    match i with
    | LNone     -> "LNone"
    | LCross _  -> "LCross"
    | LOver     -> "LOver"

  let line_of_dseg s =
    mkline s.head s.tail

  let line_of_ray r =
    line_of_dseg (ray_dseg r)

  let string_of_line l =
    let (pA, pB) = line_pts l in
    Printf.sprintf "Line(%s, %s)"
      (string_of_pt pA)
      (string_of_pt pB)

  let threejs_of_line nm l =
    let open ThreeJS in
    let (a, b) =
      line_pts l |> Util.pair_map (fun p -> (p.x, p.y, p.z))
    in
    [ add_point nm a
    ; add_point nm b
    ; ""
    ; add_line nm a b
    ] |> String.concat "\n"


  let string_of_ray r =
    let s = ray_dseg r in
    Printf.sprintf "Ray(%s)"
      (string_of_dseg s)

  (* TODO add symmetry tests *)
  (* TODO add collinear "symmetry" test *)
  let equiv_line lA lB =
    let (pA1, pA2) = line_pts lA in
    let (pB1, pB2) = line_pts lB in
    collinear pA1 pA2 pB1 &&
    collinear pA1 pA2 pB2

  let on_line l pt =
    let (pA, pB) = line_pts l in
    collinear pA pB pt

  let line_pt_dist l pt =
    let (p1, p2) = line_pts l in
    N.div (magn (cross_prod (sub pt p1) (sub pt p2)))
          (magn (sub p2 p1))

    (*
    let (p1, p2) = line_pts l in
    let numer =
      N.sub
        (N.mul
          (N.mul (magn (sub p1 pt)) (magn (sub p1 pt)))
          (N.mul (magn (sub p2 p1)) (magn (sub p2 p1))))
        (N.mul
          (dot_prod (sub p1 pt) (sub p2 p1))
          (dot_prod (sub p1 pt) (sub p2 p1)))
    in
    let denom =
      N.mul
        (magn (sub p2 p1))
        (magn (sub p2 p1))
    in
    N.div numer denom
    *)

  let on_ray r pt =
    let s = ray_dseg r in
    if equiv_pt pt s.tail
    || equiv_pt pt s.head then
      true
    else
      on_line (line_of_ray r) pt &&
      not (on_dseg (mkdseg s.tail pt) s.head)

  (* TODO add symmetry tests *)
  let equiv_ray rA rB =
    let sA = ray_dseg rA in
    let sB = ray_dseg rB in
    equiv_pt sA.head sB.head &&
    on_ray rA sB.tail

  (* using Gaussian Elimination *)
  let isect_ll_gauss l1 l2 =
    let (p11, p12) = line_pts l1 in
    let (p21, p22) = line_pts l2 in
    let v1 = sub p12 p11 in
    let v2 = sub p22 p21 in
    let p  = sub p11 p21 in
    let point_list {x = x; y = y; z = z} = [x; y; z] in
    let m = Mat.of_list (transpose (List.map point_list [v1; v2; p])) in
    Mat.gaussian_elim m;
    let is_zero x = N.equiv x N.n0 in
    if is_zero (Mat.get m 0 0) then
      (* Not possible since v1 is nonzero (since all lines are
         represented w/ two distinct points) *)
      failwith "LinAlg.isect_ll: impossible"
    else ();
    if not (is_zero (Mat.get m 2 2)) then
      LNone
    else if is_zero (Mat.get m 1 1) then
      if not (is_zero (Mat.get m 1 2)) then
        LNone
      else
        (* GE has reduced it to a 1d equation in the first row *)
        LOver
    else
      let t2 = N.div (Mat.get m 1 2) (Mat.get m 1 1) in
      let ans = add p21 (scale t2 v2) in
      LCross ans


  let skew_inter_z pt1 v1 pt2 v2 =
    let (x1, y1, z1) = pt1 in
    let (a1, b1, c1) =  v1 in
    let (x2, y2, z2) = pt2 in
    let (a2, b2, c2) =  v2 in

    (* TODO what does this correspond to geometrically? *)
    let t2 =
      let ntor = N.sub (N.sub (N.mul b1 x1)
                              (N.mul a1 y1))
                       (N.sub (N.mul b1 x2)
                              (N.mul a1 y2)) in
      let dtor = N.sub (N.mul a2 b1)
                       (N.mul a1 b2) in
      N.div ntor dtor
    in
    let t1 =
      if N.equiv N.n0 a1 then
        N.div (N.add (N.mul b2 t2)
                     (N.sub y2 y1))
              b1
      else
        N.div (N.add (N.mul a2 t2)
                     (N.sub x2 x1))
              a1
    in
    let z_l = N.add (N.mul c1 t1) z1 in
    let z_r = N.add (N.mul c2 t2) z2 in
    if N.equiv z_l z_r then
      let x = N.add (N.mul a1 t1) x1 in
      let y = N.add (N.mul b1 t1) y1 in
      LCross (mkpt x y z_l)
    else
      LNone


  let skew_inter_x pt1 v1 pt2 v2 =
    let (x1, y1, z1) = pt1 in
    let (a1, b1, c1) =  v1 in
    let (x2, y2, z2) = pt2 in
    let (a2, b2, c2) =  v2 in

    let t2 =
      let ntor = N.sub (N.sub (N.mul c1 y1)
                              (N.mul b1 z1))
                       (N.sub (N.mul c1 y2)
                              (N.mul b1 z2)) in
      let dtor = N.sub (N.mul b2 c1)
                       (N.mul b1 c2) in
      N.div ntor dtor
    in
    let t1 =
      if N.equiv N.n0 b1 then
        N.div (N.add (N.mul c2 t2)
                     (N.sub z2 z1))
              c1
      else
        N.div (N.add (N.mul b2 t2)
                     (N.sub y2 y1))
              b1
    in
    let x_l = N.add (N.mul a1 t1) x1 in
    let x_r = N.add (N.mul a2 t2) x2 in
    if N.equiv x_l x_r then
      let y = N.add (N.mul b1 t1) y1 in
      let z = N.add (N.mul c1 t1) z1 in
      LCross (mkpt x_l y z)
    else
      LNone


  let skew_inter_y pt1 v1 pt2 v2 =
    let (x1, y1, z1) = pt1 in
    let (a1, b1, c1) =  v1 in
    let (x2, y2, z2) = pt2 in
    let (a2, b2, c2) =  v2 in

    let t2 =
      let ntor = N.sub (N.sub (N.mul c1 x1)
                              (N.mul a1 z1))
                       (N.sub (N.mul c1 x2)
                              (N.mul a1 z2)) in
      let dtor = N.sub (N.mul c1 a2)
                       (N.mul c2 a1) in
      N.div ntor dtor
    in
    let t1 =
      if N.equiv N.n0 c1 then
        N.div (N.add (N.mul a2 t2)
                     (N.sub x2 x1))
              a1
      else
        N.div (N.add (N.mul c2 t2)
                     (N.sub z2 z1))
              c1

    in
    let y_l = N.add (N.mul b1 t1) y1 in
    let y_r = N.add (N.mul b2 t2) y2 in
    if N.equiv y_l y_r then
      let x = N.add (N.mul a1 t1) x1 in
      let z = N.add (N.mul c1 t1) z1 in
      LCross (mkpt x y_l z)
    else
      LNone


  let shared_pt_l l1 l2 =
    let (a, b) = line_pts l1 in
    let (c, d) = line_pts l2 in
    match sort cmp_pt [a; b; c; d] with
    | [m; n; p; q] ->
        if equiv_pt m n then
          Some m
        else if equiv_pt n p then
          Some n
        else if equiv_pt p q then
          Some p
        else
          None
    | _ -> failwith "Geom3.shared_pt_l: bogus sort"

  let isect_ll_direct l1 l2 =
    if equiv_line l1 l2 then
      (* overlap *)
      LOver
    else
      match shared_pt_l l1 l2 with
      | Some pt ->
          LCross pt
      | None -> begin
          let (pt1, ((a1, b1, c1) as v1)) =
            line_eqn (line_pts l1) in
          let (pt2, ((a2, b2, c2) as v2)) =
            line_eqn (line_pts l2) in
          (* TODO maybe unit vec the a b c *)
          if not (N.equiv (N.mul a1 b2) (N.mul a2 b1)) then
            (* xy not parallel *)
            skew_inter_z pt1 v1 pt2 v2
          else if not (N.equiv (N.mul b1 c2) (N.mul b2 c1)) then
            (* yz not parallel *)
            skew_inter_x pt1 v1 pt2 v2
          else if not (N.equiv (N.mul c1 a2) (N.mul c2 a1)) then
            (* zx not parallel *)
            skew_inter_y pt1 v1 pt2 v2
          else
            (* all parallel (and no overlap -- above) *)
            LNone
        end

  let verbose_string_of_isect_ln i =
    match i with
    | LNone    -> "LNone"
    | LCross p -> Printf.sprintf "LCross(%s)" (string_of_pt p)
    | LOver    -> "LOver"

  let isect_line_equiv il1 il2 =
    match il1, il2 with
    | LNone, LNone -> true
    | LCross p1, LCross p2 -> true
    | LOver, LOver -> true
    | _, _ -> false

  let isect_ll l1 l2 =
    let ans_d = isect_ll_direct l1 l2 in
(*    let ans_g = isect_ll_gauss  l1 l2 in
    if not (isect_line_equiv ans_g ans_d) then begin
      Printf.printf "Inputs: %s %s\n"
        (string_of_line l1)
        (string_of_line l2);
      Printf.printf "Gauss: %s\n"
        (verbose_string_of_isect_ln ans_g);
      Printf.printf "Direct: %s\n"
        (verbose_string_of_isect_ln ans_d);
      failwith "isect_ll: different answers!"
      end
    else (); *)
    ans_d

  let isect_ls l s =
    match isect_ll l (line_of_dseg s) with
    | LNone ->
        SNone
    | LCross pt ->
        if on_dseg s pt
        then SCross pt
        else SNone
    | LOver ->
        SOver s

  let string_of_isect_ls i =
    match i with
    | SNone    -> "SNone"
    | SCross _ -> "SCross"
    | SOver  _ -> "SOver"

  (* NOTE: segment direction correctly preserved! *)
  let isect_rs r s =
    match isect_ls (line_of_ray r) s with
    | SNone ->
        SNone
    | SCross pt ->
        if on_ray r pt
        then SCross pt
        else SNone
    | SOver {head = h; tail = t} ->
        let rH = (ray_dseg r).head in
        begin match on_ray r h, on_ray r t with
        | true,  true  -> SOver (mkdseg h  t)
        | true,  false -> SOver (mkdseg h rH)
        | false, true  -> SOver (mkdseg rH t)
        | false, false -> SNone
        end

  let isect_ss a b =
    match isect_ll (line_of_dseg a) (line_of_dseg b) with
    | LNone ->
        SNone
    | LCross pt ->
        if on_dseg a pt
        && on_dseg b pt
        then SCross pt
        else SNone
    | LOver ->
        (* NOTE segs collinear! *)
        let overlap =
          on_dseg a b.head ||
          on_dseg a b.tail ||
          on_dseg b a.head ||
          on_dseg b a.tail
        in
        if overlap then
          begin match
            sort cmp_pt
              [ a.head ; a.tail
              ; b.head ; b.tail
              ]
          with
          | [p1; p2; p3; p4] ->
              (* TODO may not preserve direction! *)
              SOver (mkdseg p2 p3)
          | _ ->
              failwith "Geom3.isect_ss: bogus"
          end
        else
          SNone

  (** triangles *)
  type tri =
    { a : pt
    ; b : pt
    ; c : pt
    }

  type side =
    | POS
    | NEG
    | COP

  type isect_tln =
    | TLThru of pt
    | TLCut  of dseg
    | TLNone

  type isect_tsg =
    | TSThru of pt
    | TSCut  of dseg
    | TSNone

  let equiv_isect_tl itl1 itl2 =
    match itl1, itl2 with
    | TLThru pt1, TLThru pt2 ->
        equiv_pt pt1 pt2
    | TLCut s1, TLCut s2 ->
        equiv_dseg s1 s2
    | TLNone, TLNone ->
        true
    | _, _ ->
        false

  let equiv_isect_ts its1 its2 =
    match its1, its2 with
    | TSThru pt1, TSThru pt2 ->
        equiv_pt pt1 pt2
    | TSCut s1, TSCut s2 ->
        equiv_dseg s1 s2
    | TSNone, TSNone ->
        true
    | _, _ ->
        false

  (* TODO: allows zero area triangles *)
  let mktri a b c =
    { a = a
    ; b = b
    ; c = c
    }

  let string_of_tri t =
    [ Printf.sprintf "a = %s" (string_of_pt t.a)
    ; Printf.sprintf "b = %s" (string_of_pt t.b)
    ; Printf.sprintf "c = %s" (string_of_pt t.c)
    ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"

  let pts_of_tri t =
    (t.a, t.b, t.c)

  let threejs_of_tri nm t =
    let open ThreeJS in
    let (a, b, c) =
      t |> pts_of_tri
        |> Util.triple_map (fun p -> (p.x, p.y, p.z))
    in
    [ add_point nm a
    ; add_point nm b
    ; add_point nm c
    ; ""
    ; add_line nm a b
    ; add_line nm b c
    ; add_line nm c a
    ; ""
    ; add_face nm a b c
    ] |> String.concat "\n"


  let pts_of_tris ts =
    ts |> Util.flatmap (fun t -> [t.a; t.b; t.c])
       |> Util.dedup cmp_pt

  let dsegs_of_tri t =
    ( mkdseg t.a t.b
    , mkdseg t.b t.c
    , mkdseg t.c t.a
    )

  let order_tri t1 t2 =
    match order_pt t1.a t2.a with
    | LT -> LT
    | EQ -> (match order_pt t1.b t2.b with
            | LT -> LT
            | EQ -> (match order_pt t1.c t2.c with
                    | LT -> LT
                    | EQ -> EQ
                    | GT -> GT)
            | GT -> GT)
    | GT -> GT

  let cmp_tri t1 t2 =
    match cmp_pt t1.a t2.a with
    | LT -> LT
    | EQ -> (match cmp_pt t1.b t2.b with
            | LT -> LT
            | EQ -> (match cmp_pt t1.c t2.c with
                    | LT -> LT
                    | EQ -> EQ
                    | GT -> GT)
            | GT -> GT)
    | GT -> GT

  let equiv_tri t1 t2 =
    let (a2, b2, c2) = pts_of_tri t2 in
    cmp_tri t1 t2  = EQ ||
    cmp_tri t1 (mktri b2 c2 a2) = EQ ||
    cmp_tri t1 (mktri c2 b2 a2) = EQ

  let centroid t =
    let d =
      { x = N.n3
      ; y = N.n3
      ; z = N.n3
      }
    in
    div (add t.a (add t.b t.c)) d

  (* W. Kahan recommends this *)
  let area_tri' pA pB pC =
    let d_ab = dist_pt pA pB in
    let d_bc = dist_pt pB pC in
    let d_ac = dist_pt pA pC in
    let sides = List.rev
                  ( Util.sort N.cmp
                      [d_ab; d_bc; d_ac])
    in
    let a  = List.nth sides 0 in
    let b  = List.nth sides 1 in
    let c  = List.nth sides 2 in
    let s  = N.add a (N.add b c) in
    let s1 = N.sub c (N.sub a b) in
    let s2 = N.add c (N.sub a b) in
    let s3 = N.add a (N.sub b c) in
    N.div (N.sqrt (N.mul s (N.mul s1 (N.mul s2 s3)))) N.n4
(*
  let area_tri' pA pB pC =
    let a = dist_pt pA pB in
    let b = dist_pt pB pC in
    let c = dist_pt pA pC in
    let s = N.div (N.add a (N.add b c)) N.n2 in
    let p1 = N.sub s a in
    let p2 = N.sub s b in
    let p3 = N.sub s c in
    let p4 = N.mul p1 p2 in
    let p5 = N.mul p3 p4 in
    N.sqrt(N.mul s p5)
*)

  let area_tri t =
    area_tri' t.a t.b t.c

  let empty_tri t =
    collinear t.a t.b t.c ||
    N.equiv N.n0 (area_tri t)

  let plane_norm (pA, pB, pC) =
    let vAB = best_effort_unit_vec (sub pB pA) in
    let vAC = best_effort_unit_vec (sub pC pA) in
    best_effort_unit_vec (cross_prod vAB vAC)

  (* plane equation: ax + by + cz = d *)
  (* TODO how to select which point for computing d?
   *   OR ensure it doesn't matter numerically *)
  let plane_eqn ((pA, _, _) as pts) =
    let n = plane_norm pts in
    let d = dot_prod n pA in
    (n.x, n.y, n.z, d)

  let vecs_orthogonal v1 v2 =
    N.equiv N.n0 (dot_prod v1 v2)

  (* unique plane through the points *)
  let coplanar pA pB pC pD =
    let pn = plane_norm (pA, pB, pC) in
    let vAD = best_effort_unit_vec (sub pD pA) in
    vecs_orthogonal pn vAD

  (* either inside or on t *)
  let on_tri t pt =
    if not (coplanar t.a t.b t.c pt) then
      false
    else
      (* are [p1] and [p2] are on the same side of [ab] *)
      let same_side p1 p2 a b =
        let tri_norm p q r =
          best_effort_unit_vec (cross_prod
            (best_effort_unit_vec (sub q p))
            (best_effort_unit_vec (sub r p)))
        in
        let cp1 = tri_norm a b p1 in
        let cp2 = tri_norm a b p2 in
        N.cmp N.n0 (dot_prod cp1 cp2) <> GT
      in
      let (a, b, c) = pts_of_tri t in
      same_side pt a b c &&
      same_side pt b a c &&
      same_side pt c a b

  let lift_ptop_tri op t =
    { a = op t.a
    ; b = op t.b
    ; c = op t.c
    }

  (** planes *)

  type plane =
    Plane of pt * pt * pt

  exception CollinearPlanePoints of pt * pt * pt

  let () =
    Printexc.register_printer (function
      | CollinearPlanePoints (a, b, c) -> Some (
          Printf.sprintf "CollinearPlanePoints:\n%s\n%s\n%s"
            (string_of_pt a)
            (string_of_pt b)
            (string_of_pt c))
      | _ -> None)

  type isect_plane =
    | PNone
    | POver
    | PCross of line

  type isect_pln =
    | PLThru of pt
    | PLNone
    | PLOn

  let mkplane pA pB pC =
    if collinear pA pB pC
    then raise (CollinearPlanePoints (pA, pB, pC))
    else Plane (pA, pB, pC)

  let plane_of_tri t =
    mkplane t.a t.b t.c

  let plane_pts = function Plane (pA, pB, pC) ->
    (pA, pB, pC)

  let string_of_plane p =
    let (pA, pB, pC) = plane_pts p in
    Printf.sprintf "Plane (%s, %s, %s)"
      (string_of_pt pA)
      (string_of_pt pB)
      (string_of_pt pC)

  let equiv_plane p1 p2 =
    let (p1A, p1B, p1C) = plane_pts p1 in
    let (p2A, p2B, p2C) = plane_pts p2 in
    coplanar p1A p1B p1C p2A &&
    coplanar p1A p1B p1C p2B &&
    coplanar p1A p1B p1C p2C

  let on_plane p pt =
    let (pA, pB, pC) = plane_pts p in
    coplanar pA pB pC pt

  (* signed distance (by right hand rule) from plane to point *)
  let plane_pt_dist' p pt =
    let (pA, pB, pC) = plane_pts p in
    let n = plane_norm (pA, pB, pC) in
    let v = best_effort_unit_vec (sub pt pA) in
    N.div (dot_prod n v) (magn n)

  let plane_pt_dist p pt =
    N.abs (plane_pt_dist' p pt)

  let side_plane' p pt =
    let d = plane_pt_dist' p pt in
    match N.cmp N.n0 d with
    | GT -> POS
    | LT -> NEG
    | EQ -> begin
        let (a, b, c) = plane_pts p in
        assert (coplanar a b c pt);
        COP
      end

  (** checks if [pt] is on the plane of triangle [t], or above it, or below it *)
  let side_plane t pt =
    side_plane' (plane_of_tri t) pt
    (*
    let (a, b, c, d) =
      plane_eqn (t.a, t.b, t.c) in
    let d' =
      dot_prod (mkpt a b c) pt in
    match N.cmp d' d with
    | GT -> POS
    | LT -> NEG
    | EQ -> COP
    *)

  (* returns the acute angle between vectors [v1] and [v2] *)
  let vector_angle v1 v2 =
    let cosA =
      N.div (dot_prod v1 v2)
            (N.mul (magn v1) (magn v2))
    in
    assert (Util.between N.cmp (N.neg N.n1) cosA N.n1);
    N.acos cosA

  (* returns the acute angle between planes [p1] and [p2] *)
  let plane_angle p1 p2 =
    let n1 = plane_norm (plane_pts p1) in
    let n2 = plane_norm (plane_pts p2) in
    vector_angle n1 n2

  let isect_pl p l =
    let (lA, lB) = line_pts l in
    if on_plane p lA
    && on_plane p lB then
      PLOn
    else
      let pn = plane_norm (plane_pts p) in
      let lv = best_effort_unit_vec (sub lB lA) in
      if vecs_orthogonal pn lv then
        (* line is parallel to plane, but not coplanar *)
        PLNone
      else
        let (_, _, _, d) =
          plane_eqn (plane_pts p) in
        let t =
          N.div (N.sub d (dot_prod pn lA))
                (dot_prod pn lv) in
        PLThru (add lA (scale t lv))

  (** tetrahedrons *)
  type tdron =
    { ta : tri
    ; tb : tri
    ; tc : tri
    ; td : tri
    }

  exception TooManyUniquePoints of pt list

  let mktdron a b c d =
    let pts = pts_of_tris [a; b; c; d] in
    let uniques = Util.dedup cmp_pt pts in
    if List.length uniques <> 4 then
      raise (TooManyUniquePoints uniques)
    else
      { ta = a
      ; tb = b
      ; tc = c
      ; td = d
      }

  let tris_of_tdron t =
    (t.ta, t.tb, t.tc, t.td)

  let pts_of_tdron t =
    let (a, b, c, d) = tris_of_tdron t in
    let pts =
      pts_of_tris [a; b; c; d]
        |> Util.dedup cmp_pt
    in
    ( List.nth pts 0
    , List.nth pts 1
    , List.nth pts 2
    , List.nth pts 3)

  let pts_of_tdrons ts =
    let aux t =
      let (a, b, c, d) = pts_of_tdron t in
      [a; b; c; d]
    in
    ts |> Util.flatmap aux
       |> Util.dedup cmp_pt

  (* checks if [pt] is on the same side
    of [a], [b], [c] as [d] *)
  let side_tdron a b c d pt =
    let norm =
      cross_prod (sub b a) (sub c a)
    in
    let dot_d  = dot_prod norm (sub d  a) in
    let dot_pt = dot_prod norm (sub pt a) in
    N.cmp (N.mul dot_d dot_pt) N.n0 <> LT
    (*
    N.cmp (N.mul dot_d dot_pt) N.n0 = GT
    *)

  let string_of_tdron t =
    [ Printf.sprintf "a = %s" (string_of_tri t.ta)
    ; Printf.sprintf "b = %s" (string_of_tri t.tb)
    ; Printf.sprintf "c = %s" (string_of_tri t.tc)
    ; Printf.sprintf "d = %s" (string_of_tri t.td)
    ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"

  let in_tdron t pt =
    let (a, b, c, d) = pts_of_tdron t in
    side_tdron a b c d pt &&
    side_tdron b c d a pt &&
    side_tdron c d a b pt &&
    side_tdron d a b c pt

  let tetraVol v1 v2 v3 v4 =
    let (x1, y1, z1) = (v1.x, v1.y, v1.z) in
    let (x2, y2, z2) = (v2.x, v2.y, v2.z) in
    let (x3, y3, z3) = (v3.x, v3.y, v3.z) in
    let (x4, y4, z4) = (v4.x, v4.y, v4.z) in
    let m  =
      [ [x1; x2; x3; x4]
      ; [y1; y2; y3; y4]
      ; [z1; z2; z3; z4]
      ; [N.n1; N.n1; N.n1; N.n1]
      ]
    in
    N.div (Mat.det m) N.n6

  let equiv_isect_pln ipl1 ipl2 =
    match ipl1, ipl2 with
    | PLNone, PLNone -> true
    | PLThru pt1, PLThru pt2 -> equiv_pt pt1 pt2
    | PLOn, PLOn -> true
    | _, _ -> false

  let string_of_isect_pl i =
    match i with
    | PLNone   -> "PLNone"
    | PLThru _ -> "PLThru"
    | PLOn     -> "PLOn"

  let isect_tl t l =
    match isect_pl (plane_of_tri t) l with
    | PLNone ->
        TLNone
    | PLThru pt ->
        if on_tri t pt
        then TLThru pt
        else TLNone
    | PLOn ->
        match isect_ls l (mkdseg t.a t.b)
            , isect_ls l (mkdseg t.b t.c)
            , isect_ls l (mkdseg t.c t.a) with
        | SNone, SNone, SNone ->
            TLNone
        (* NOTE: expect crossings to be at vertices in the following 3 cases *)
        | SOver s1, SCross _, SCross _ ->
            TLCut s1
        | SCross _, SOver s2, SCross _ ->
            TLCut s2
        | SCross _, SCross _, SOver s3 ->
            TLCut s3
        | SCross pt1, SCross pt2, SNone ->
            TLCut (mkdseg pt1 pt2)
        | SCross pt1, SNone, SCross pt3 ->
            TLCut (mkdseg pt1 pt3)
        | SNone, SCross pt2, SCross pt3 ->
            TLCut (mkdseg pt2 pt3)
        | SCross pt1, SCross pt2, SCross pt3 ->
            (* NOTE: two of them should be the same vertex of the triangle *)
            if equiv_pt pt1 pt2 then
              TLCut (mkdseg pt1 pt3)
            else if equiv_pt pt2 pt3 then
              TLCut (mkdseg pt1 pt2)
            else if equiv_pt pt1 pt3 then
              TLCut (mkdseg pt2 pt3)
            else
              (* TODO: THIS IS VERY VERY BAD *)
              TLNone
              (*
              failwith (Printf.sprintf
                          "Geom3.isect_tl: bogus t = %s; l = %s"
                          (string_of_tri t)
                          (string_of_line l))
              *)
        | _, _, _ ->
            (* TODO: THIS IS VERY VERY BAD *)
            TLNone
            (*
            failwith (Printf.sprintf
              "Geom3.isect_tl: no case matched!\nt = %s; l = %s"
              (string_of_tri t)
              (string_of_line l))
            *)

  (* NOTE cut may not be on triangle edges! *)
  let isect_tr t r =
    match isect_tl t (line_of_ray r) with
    | TLNone ->
        TLNone
    | TLThru pt ->
        if on_ray r pt
        then TLThru pt
        else TLNone
    | TLCut {head = h; tail = t} ->
        let rH = (ray_dseg r).head in
        begin match on_ray r h, on_ray r t with
        | true , true  -> TLCut (mkdseg h  t)
        | true , false -> TLCut (mkdseg h rH)
        | false, true  -> TLCut (mkdseg rH t)
        | false, false -> TLNone
        end

  let isect_ts t s =
    match isect_tl t (line_of_dseg s) with
    | TLNone  ->
        TSNone
    | TLThru pt ->
        if on_dseg s pt then
          TSThru pt
        else
          TSNone
    | TLCut s' ->
        let (p1, p2) = pts_of_dseg s' in
        if equiv_pt p1 p2 then
          TSNone
        else
          match isect_ss s s' with
          | SNone ->
              TSNone
          | SOver s'' ->
              TSCut s''
          | SCross pt ->
              (* NOTE: VERY BAD *)
              TSNone
              (*
              print_endline (string_of_dseg s);
              print_endline (string_of_dseg s');
              failwith "s s' are on same line"
              *)

  let string_of_isect_ts i =
    match i with
    | TSNone   -> "TSNone"
    | TSThru _ -> "TSThru"
    | TSCut  _ -> "TSCut"

  let string_of_isect_tlr i =
    match i with
    | TLNone   -> "TLNone"
    | TLThru _ -> "TLThru"
    | TLCut  s -> "TLCut" ^ (string_of_dseg s)

  let part_tpt t pt =
    if not (on_tri t pt) then
      [t]
    else
      let (a, b, c) = pts_of_tri t in
      List.filter (Util.notp empty_tri)
        [ mktri a b pt
        ; mktri b pt c
        ; mktri c a pt
        ]

  let part_ts t s =
    match isect_ts t s with
    | TSNone ->
        [t]
    | TSThru pt ->
        part_tpt t pt
    | TSCut s' ->
        t |> flip part_tpt s'.head
          |> Util.flatmap (flip part_tpt s'.tail)

  let part_ts_other t1 t2 s2 =
    match isect_ts t1 s2 with
    | TSNone ->
        [t2]
    | TSThru pt ->
        part_tpt t2 pt
    | TSCut s' ->
        [t2]

  let sides_of_tri t =
    let (a, b, c) = pts_of_tri t in
    ( mkdseg a b
    , mkdseg b c
    , mkdseg c a)

  let part_tri_other t1 t2 =
    if equiv_tri t1 t2 then
      [t2]
    else
      let (s2a, s2b, s2c) = sides_of_tri t2 in
      [t2] |> Util.flatmap (flip (part_ts_other t1) s2a)
           |> Util.flatmap (flip (part_ts_other t1) s2b)
           |> Util.flatmap (flip (part_ts_other t1) s2c)
           |> List.filter (Util.notp empty_tri)

  (* partition [t2] with respect to [t1] *)
  let part_tri t1 t2 =
    if equiv_tri t1 t2 then
      [t2]
    else
      let (s1a, s1b, s1c) = sides_of_tri t1 in
      [t2] |> Util.flatmap (flip part_ts s1a)
           |> Util.flatmap (flip part_ts s1b)
           |> Util.flatmap (flip part_ts s1c)
           |> Util.flatmap (part_tri_other t1)

  (* give the parts of [b] as partitioned by tris *)
  let part_tris tris b = begin
    assert (List.for_all (Util.notp empty_tri) tris);
    let rec loop tris acc =
      match tris with
      | [] -> List.filter (Util.notp empty_tri) acc
      | t :: ts -> loop ts (Util.flatmap (part_tri t) acc)
    in
    loop tris [b]
    >> (fun bs ->
      if List.length bs > 10 then begin
        log "WARNING";
        log (Printf.sprintf
          "part_tris splitting by %d triangles, got %d triangles for\n%s"
          (List.length tris)
          (List.length bs)
          (string_of_tri b))
      end)
  end
(*
    match tris with
    | [] -> [b]
    | t :: ts ->
        begin match
          b |> part_tri t
            |> Util.flatmap (part_tris ts)
            |> List.filter (Util.notp empty_tri)
        with
        | [] -> [b]
        | trs -> trs
        end
*)

  (** polyhedrons *)

  let only_boundary_isects tris =
    let no_split (ta, tb) =
      match
        part_tri tb ta,
        part_tri ta tb
      with
      | [_], [_] -> true
      |  _ ,  _  -> false
    in
    tris |> Util.choose2
         |> List.for_all no_split

  type pdon =
    Pdon of tri list

  exception SelfIsect
  exception RepeatTri

  let mkpdon tris =
    if only_boundary_isects tris then
      let n_uniq =
        (* TODO what about equiv tris? *)
        tris |> Util.dedup cmp_tri
             |> List.length
      in
      if List.length tris = n_uniq then
        (* canonicalize *)
        Pdon (Util.sort cmp_tri tris)
      else
        raise RepeatTri
    else
      raise SelfIsect

  let tris_of_pdon = function Pdon tris ->
    tris

  let pts_of_pdon p =
    p |> tris_of_pdon
      |> List.map pts_of_tri
      |> List.map (function (a, b, c) -> [a; b; c])
      |> List.fold_left (@) []
      |> Util.dedup cmp_pt

  let string_of_pdon p =
    p |> tris_of_pdon
      |> List.map string_of_tri
      |> String.concat "; "
      |> Printf.sprintf "Pdon(%s)"

  let equiv_pdon a b =
    (* pdons canonicalized by sorting *)
    (a, b)
      |> pair_map tris_of_pdon
      |> uncurry List.combine
      |> List.for_all (uncurry equiv_tri)

  (** hulls *)

  let ckhull p =
    (* TODO need hull check for 3D *)
    true

  type hull =
    Hull of pdon

  exception BadHull

  let mkhull tris =
    if ckhull tris then
      Hull (mkpdon tris)
    else
      raise BadHull

  module H : sig
    type t

    val init      : tri list -> t
    val extend    : t -> pt -> unit
    val extract   : t -> tri list
    val valid     : t -> bool
    val to_string : t -> string
  end = struct
    type htri =
      { a : pt
      ; b : pt
      ; c : pt
      ; mutable border_ab : htri option
      ; mutable border_ac : htri option
      ; mutable border_bc : htri option
      ; mutable under     : bool
      }

    let mkhtri p q r =
      (* canonicalize *)
      match Util.sort order_pt [p; q; r] with
      | [a; b; c] ->
          { a = a
          ; b = b
          ; c = c
          ; border_ab = None
          ; border_ac = None
          ; border_bc = None
          ; under     = false
          }
      | _ ->
          failwith "mkhtri: bogus"

    let string_of_htri' = function
      | None -> "None"
      | Some p ->
          Printf.sprintf "Some { %s\n                   }" @@
            String.concat "\n                   ; " @@
              [ Printf.sprintf "a = %s" (string_of_pt p.a)
              ; Printf.sprintf "b = %s" (string_of_pt p.b)
              ; Printf.sprintf "c = %s" (string_of_pt p.c)
              ]

    let string_of_htri p =
      Printf.sprintf "{ %s\n}" @@
        String.concat "\n; " @@
          [ Printf.sprintf "a = %s" (string_of_pt p.a)
          ; Printf.sprintf "b = %s" (string_of_pt p.b)
          ; Printf.sprintf "c = %s" (string_of_pt p.c)
          ; Printf.sprintf "border_ab = %s" (string_of_htri' p.border_ab)
          ; Printf.sprintf "border_ac = %s" (string_of_htri' p.border_ac)
          ; Printf.sprintf "border_bc = %s" (string_of_htri' p.border_bc)
          ; Printf.sprintf "under = %B" p.under
          ]

    let htri_of_tri t =
      (* ugly, field name clash with tri *)
      uncurry3 mkhtri (pts_of_tri t)

    let tri_of_htri p =
      mktri p.a p.b p.c

    let equiv_htri p q =
      equiv_pt p.a q.a &&
      equiv_pt p.b q.b &&
      equiv_pt p.c q.c

    type edge =
      | AB
      | AC
      | BC

    let string_of_edge = function
      | AB -> "AB"
      | AC -> "AC"
      | BC -> "BC"

    let get_edge p = function
      | AB -> (p.a, p.b)
      | AC -> (p.a, p.c)
      | BC -> (p.b, p.c)

    let get_other p = function
      | AB -> p.c
      | AC -> p.b
      | BC -> p.a

    let get_border p = function
      | AB -> p.border_ab
      | AC -> p.border_ac
      | BC -> p.border_bc

    let get_border' p b =
      valOf (get_border p b)

    let ck_unset p b =
      if get_border p b <> None then begin
        failwith @@ Printf.sprintf "ck_unset: %s already set"
          (string_of_edge b)
      end

    let set_border p b q = begin
      ck_unset p b;
      match b with
      | AB -> p.border_ab <- Some q
      | AC -> p.border_ac <- Some q
      | BC -> p.border_bc <- Some q
    end

    let ck_set p b q =
      match get_border p b with
      | None ->
          failwith @@
          Printf.sprintf "ck_set: %s was None in\n%s\n\nExpected:\n%s\n"
            (string_of_edge b)
            (string_of_htri p)
            (string_of_htri q)
      | Some x ->
          if not (equiv_htri x q) then begin
            failwith @@
            Printf.sprintf "ck_set: %s unexpected in:\n%s\n\nExpected:\n%s\n"
              (string_of_edge b)
              (string_of_htri p)
              (string_of_htri q)
          end

    let unset_border p b q = begin
      ck_set p b q;
      match b with
      | AB -> p.border_ab <- None
      | AC -> p.border_ac <- None
      | BC -> p.border_bc <- None
    end

    let equiv_edge (a, b) (p, q) =
      equiv_pt a p &&
      equiv_pt b q

    let shared_border p q =
      let bs = [AB; AC; BC] in
      let aux (pb, qb) =
        equiv_edge (get_edge p pb) (get_edge q qb) in
      List.find_opt aux (xprod bs bs)

    (* assumes p valid *)
    let neighbor_other p =
      let q = get_border' p AB in
      let b = snd (valOf (shared_border p q)) in
      get_other q b

    let get_shared_edge p q =
      let b = snd (valOf (shared_border p q)) in
      get_edge q b

    type t =
      { mutable htris : htri list }

    let mkhull () =
      { htris = [] }

    let add h p = begin
      let aux q =
        match shared_border p q with
        | Some (pb, qb) ->
            set_border p pb q;
            set_border q qb p
        | None -> ()
      in
      List.iter aux h.htris;
      h.htris <- p :: h.htris
    end

    let rem h p = begin
      (* remove from htris first, avoid self border unset *)
      h.htris <- List.filter (notp (equiv_htri p)) h.htris;
      let aux q =
        match shared_border p q with
        | Some (pb, qb) ->
            unset_border p pb q;
            unset_border q qb p
        | None -> ()
      in
      List.iter aux h.htris
    end

    let init ts = begin
      let h = mkhull () in
      ts |> List.map htri_of_tri
         |> List.iter (add h);
      h
    end

    let extend h pt = begin
      (* for each triangle, save whether it is "under" pt *)
      let set_under p =
        let d = neighbor_other p in
        p.under <- not (side_tdron p.a p.b p.c d pt)
      in
      List.iter set_under h.htris;

      (* determine triangles to remove and add due to extension *)
      let to_rem = ref [] in
      let to_add = ref [] in
      let aux p =
        if p.under then begin
          to_rem := p :: !to_rem;
          let aux b =
            let q = get_border' p b in
            if not q.under then begin
              let (a, b) = get_shared_edge p q in
              to_add := mkhtri a b pt :: !to_add
            end
          in
          List.iter aux [AB; AC; BC]
        end
      in
      List.iter aux h.htris;

      (* perform surgery *)
      List.iter (rem h) !to_rem;
      List.iter (add h) !to_add
    end

    let valid_htri p =
      (* TODO more invariants *)
      cmp_pt p.a p.b = LT &&
      cmp_pt p.b p.c = LT &&
      List.for_all
        (fun b -> get_border p b <> None)
        [AB; AC; BC]

    let valid h =
      (* TODO more invariants *)
      List.for_all valid_htri h.htris &&
      List.for_all
        (fun p ->
          List.for_all
            (fun b ->
              let q = get_border' p b in
              List.exists (equiv_htri q) h.htris)
            [AB; AC; BC])
        h.htris

    let to_string h =
      h.htris
        |> List.map string_of_htri
        |> List.map (Util.indents 2)
        |> String.concat "\n; "
        |> Printf.sprintf "[ %s\n]"

    let extract h =
      List.map tri_of_htri h.htris
  end

  (* slow, possibly correct *)
  let hull pts = begin
    let ldst_cmp l a b =
      N.cmp (line_pt_dist l a)
            (line_pt_dist l b)
    in
    let pdst_cmp p a b =
      N.cmp (plane_pt_dist p a)
            (plane_pt_dist p b)
    in

    (* set up initial tetrahedron *)
    let pts =
      Util.dedup cmp_pt pts in
    let (a, pts) =
      Util.extract_min cmp_pt pts in
    let (b, pts) =
      Util.extract_max cmp_pt pts in
    let (c, pts) =
      let l = mkline a b in
      Util.extract_max (ldst_cmp l) pts in
    let (d, pts) =
      let p = plane_of_tri (mktri a b c) in
      Util.extract_max (pdst_cmp p) pts in

    let h =
      H.init [ mktri a b c
             ; mktri b c d
             ; mktri c d a
             ; mktri d a b ]
    in
    List.iter (H.extend h) pts;
    mkhull (H.extract h)
  end

  let pdon_of_hull = function Hull p ->
    p

  let string_of_hull h =
    h |> pdon_of_hull
      |> string_of_pdon
      |> Printf.sprintf "Hull(%s)"

  let equiv_hull a b =
    equiv_pdon
      (pdon_of_hull a)
      (pdon_of_hull b)

  let forget_pt {x; y; z} =
    {x=N.forget x; y=N.forget y; z=N.forget z}

  let forget_tri {a; b; c} =
    {a=forget_pt a; b=forget_pt b; c=forget_pt c}

end

module type GEOM3 = sig
  include RAWGEOM3
end

module Geom3
  (N : NUM)
  (M : MATRIX with type num = N.t)
  : (GEOM3 with type num = N.t) = RawGeom3 (N) (M)
