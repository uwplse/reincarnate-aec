open NumSys
open LinAlg
open Geom
open Mesh
open CAD
open Synth
open SynthBench
open RPriorityQueue

module type GLUE = sig

  type sbench1
  type sbench2
  type sbench3
  type bench1
  type bench2
  type bench3
  type mesh1
  type mesh2
  type mesh3
  type cad1
  type cad2
  type cad3
  type lc

  (** number system controls  *)
  val set_eps_abs : string -> unit
  val set_eps_rel : string -> unit
  val set_oprec   : string -> unit

  (** set invariant flag for compilers *)
  val set_invariants : bool -> unit

  (** parsers *)

  val mesh1_of_file    : string -> mesh1
  val mesh1_of_string  : string -> mesh1
  val mesh2_of_file    : string -> mesh2
  val mesh2_of_string  : string -> mesh2
  val mesh3_of_file    : string -> mesh3
  val mesh3_of_string  : string -> mesh3
  val stl_of_file      : string -> mesh3
  val stl_of_string    : string -> mesh3
  val cad1_of_file     : string -> cad1
  val cad1_of_string   : string -> cad1
  val cad2_of_file     : string -> cad2
  val cad2_of_string   : string -> cad2
  val cad3_of_file     : string -> cad3
  val cad3_of_string   : string -> cad3
  val lc_of_file       : string -> lc
  val lc_of_string     : string -> lc
  val bench1_of_file   : string -> bench1
  val bench1_of_string : string -> bench1
  val bench2_of_file   : string -> bench2
  val bench2_of_string : string -> bench2
  val bench3_of_file   : string -> bench3
  val bench3_of_string : string -> bench3

  (** pretty printers *)

  val string_of_mesh1   : mesh1   -> string
  val string_of_mesh2   : mesh2   -> string
  val tikz2_of_mesh2    : mesh2   -> string
  val tikz3_of_mesh3    : mesh3   -> string
  val string_of_mesh3   : mesh3   -> string
  val stl_of_mesh3      : mesh3   -> string
  val threejs_of_mesh3  : string  -> mesh3   -> string
  val string_of_cad1    : cad1    -> string
  val string_of_cad2    : cad2    -> string
  val string_of_cad3    : cad3    -> string
  val scad_of_cad3      : cad3    -> string
  val string_of_lc      : lc      -> string
  val string_of_bench1  : bench1  -> string
  val string_of_bench2  : bench2  -> string
  val string_of_bench3  : bench3  -> string
  val string_of_sbench1 : sbench1 -> string
  val string_of_sbench2 : sbench2 -> string
  val string_of_sbench3 : sbench3 -> string

  (** string to CAD for synth bench *)
  val sb1_of_sb1string : sbench1 -> bench1
  val sb2_of_sb2string : sbench2 -> bench2
  val sb3_of_sb3string : sbench3 -> bench3

  (** set primitives for synthesis *)

  val set_prims1 : (string * cad1) list -> unit
  val set_prims2 : (string * cad2) list -> unit
  val set_prims3 : (string * cad3) list -> unit

  (** set default flue for synthesis *)
  val set_fuel : int -> unit

  (** conversions between IRs *)

  val eval_lc        : lc     -> lc
  val cad1_of_lc     : lc     -> cad1
  val cad2_of_lc     : lc     -> cad2
  val cad3_of_lc     : lc     -> cad3
  val mesh1_of_cad1  : cad1   -> mesh1
  val mesh2_of_cad2  : cad2   -> mesh2
  val mesh3_of_cad3  : cad3   -> mesh3
  val cad1_of_mesh1  : mesh1  -> cad1
  val cad2_of_mesh2  : mesh2  -> cad2
  val cad3_of_mesh3  : mesh3  -> cad3
  val cad1_of_bench1 : bench1 -> cad1
  val cad2_of_bench2 : bench2 -> cad2
  val cad3_of_bench3 : bench3 -> cad3

  val get_numsys_stats : unit -> int
  val print_numsys_table : unit -> unit
end

module MakeGlue
  (N   : NUM)
  (M1  : MESH1
         with type num  = N.t
          and type pt   = N.t)
  (C1  : CAD1
         with type num  = N.t
          and type mesh = M1.t)
  (SS1 : SYNTHBENCH
         with type mesh = M1.t)
  (SB1 : SYNTHBENCH
          with type mesh = M1.t
           and type prim = C1.t)
  (S1  : SYNTH
         with type mesh  = M1.t
          and type cad   = C1.t
          and type bench = SB1.t)
  (M2  : MESH2
         with type num  = N.t
          and type pt   = N.t * N.t)
  (C2  : CAD2
         with type num  = N.t
          and type mesh = M2.t)
  (SS2 : SYNTHBENCH
         with type mesh = M2.t)
  (SB2 : SYNTHBENCH
         with type mesh = M2.t
          and type prim = C2.t)
  (S2  : SYNTH
         with type mesh  = M2.t
          and type cad   = C2.t
          and type bench = SB2.t)
  (M3  : MESH3
         with type num  = N.t
          and type pt   = N.t * N.t * N.t)
  (C3  : CAD3
         with type num  = N.t
          and type mesh = M3.t)
  (SS3 : SYNTHBENCH
         with type mesh = M3.t)
  (SB3 : SYNTHBENCH
         with type mesh = M3.t
          and type prim = C3.t)
  (S3  : SYNTH
         with type mesh  = M3.t
          and type cad   = C3.t
          and type bench = SB3.t)
  : GLUE
= struct

  module LC = LambdaCAD.Make(N)(C1)(C2)(C3)

  module M1Parser  = RParse.MakeMesh1Parser(N)(M1)
  module M2Parser  = RParse.MakeMesh2Parser(N)(M2)
  module M3Parser  = RParse.MakeMesh3Parser(N)(M3)
  module STLParser = RParse.MakeSTLParser(N)(M3)
  module C1Parser  = RParse.MakeCAD1Parser(N)(M1)(C1)
  module C2Parser  = RParse.MakeCAD2Parser(N)(M2)(C2)
  module C3Parser  = RParse.MakeCAD3Parser(N)(M3)(C3)
  module LCParser  = RParse.MakeLCParser(N)(LC)
  module SB1Parser = RParse.MakeSB1Parser(N)(M1)(SS1)
  module SB2Parser = RParse.MakeSB2Parser(N)(M2)(SS2)
  module SB3Parser = RParse.MakeSB3Parser(N)(M3)(SS3)

  type sbench1 = SS1.t
  type sbench2 = SS2.t
  type sbench3 = SS3.t
  type bench1  = SB1.t
  type bench2  = SB2.t
  type bench3  = SB3.t
  type mesh1   = M1.t
  type mesh2   = M2.t
  type mesh3   = M3.t
  type cad1    = C1.t
  type cad2    = C2.t
  type cad3    = C3.t
  type lc      = LC.expr

  let set_eps_abs s = N.set_eps_abs (N.forget (N.of_string s))
  let set_eps_rel s = N.set_eps_rel (N.forget (N.of_string s))
  let set_oprec   s = N.set_oprec   (int_of_string s)

  let set_invariants b = M3.set_invariants b

  let mesh1_of_file    = M1Parser.of_file
  let mesh1_of_string  = M1Parser.of_string
  let mesh2_of_file    = M2Parser.of_file
  let mesh2_of_string  = M2Parser.of_string
  let mesh3_of_file    = M3Parser.of_file
  let mesh3_of_string  = M3Parser.of_string
  let stl_of_file      = STLParser.of_file
  let stl_of_string    = STLParser.of_string
  let cad1_of_file     = C1Parser.of_file
  let cad1_of_string   = C1Parser.of_string
  let cad2_of_file     = C2Parser.of_file
  let cad2_of_string   = C2Parser.of_string
  let cad3_of_file     = C3Parser.of_file
  let cad3_of_string   = C3Parser.of_string
  let lc_of_file       = LCParser.of_file
  let lc_of_string     = LCParser.of_string

  let string_of_mesh1   = M1.to_string
  let string_of_mesh2   = M2.to_string
  let tikz2_of_mesh2     = M2.to_tikz
  let tikz3_of_mesh3     = M3.to_tikz
  let string_of_mesh3   = M3.to_string
  let stl_of_mesh3      = M3.to_stl
  let threejs_of_mesh3  = M3.to_threejs
  let string_of_cad1    = C1.to_string
  let string_of_cad2    = C2.to_string
  let string_of_cad3    = C3.to_string
  let scad_of_cad3      = C3.to_scad
  let string_of_lc      = LC.to_string
  let string_of_bench1  = SB1.to_string
  let string_of_bench2  = SB2.to_string
  let string_of_bench3  = SB3.to_string
  let string_of_sbench1 = SS1.to_string
  let string_of_sbench2 = SS2.to_string
  let string_of_sbench3 = SS3.to_string

  let eval_lc       = LC.eval
  let cad1_of_lc    = LC.to_cad1
  let cad2_of_lc    = LC.to_cad2
  let cad3_of_lc    = LC.to_cad3
  let mesh1_of_cad1 = C1.compile
  let mesh2_of_cad2 = C2.compile
  let mesh3_of_cad3 = C3.compile

  let prims1 : (string * cad1) list ref = ref []
  let prims2 : (string * cad2) list ref = ref []
  let prims3 : (string * cad3) list ref = ref []

  let fuel = ref 0

  let set_prims1 ps = prims1 := ps
  let set_prims2 ps = prims2 := ps
  let set_prims3 ps = prims3 := ps

  let set_fuel f = fuel := f

  let sb1_of_sb1string b =
    let ps =
      match b.SS1.prims with
      | None -> None
      | Some l ->
          let l' =
          l |> List.map SS1.prim_to_string
            |> List.map lc_of_string
            |> List.map cad1_of_lc
          in Some l'
    in
    SB1.mkbench
      b.SS1.bench
      b.SS1.input
      b.SS1.fuel
      ps

  let sb2_of_sb2string b =
    let ps =
      match b.SS2.prims with
      | None -> None
      | Some l ->
          let l' =
          l |> List.map SS2.prim_to_string
            |> List.map lc_of_string
            |> List.map cad2_of_lc
          in Some l'
    in
    SB2.mkbench
      b.SS2.bench
      b.SS2.input
      b.SS2.fuel
      ps

  let sb3_of_sb3string b =
    let ps =
      match b.SS3.prims with
      | None -> None
      | Some l ->
          let l' =
          l |> List.map SS3.prim_to_string
            |> List.map lc_of_string
            |> List.map cad3_of_lc
          in Some l'
    in
    SB3.mkbench
      b.SS3.bench
      b.SS3.input
      b.SS3.fuel
      ps

  let bench1_of_file f =
    let sb1 = SB1Parser.of_file f in
    sb1_of_sb1string sb1

  let bench1_of_string f =
    let sb1 = SB1Parser.of_string f in
    sb1_of_sb1string sb1

  let bench2_of_file f =
    let sb2 = SB2Parser.of_file f in
    sb2_of_sb2string sb2

  let bench2_of_string f =
    let sb2 = SB2Parser.of_string f in
    sb2_of_sb2string sb2

  let bench3_of_file f =
    let sb3 = SB3Parser.of_file f in
    sb3_of_sb3string sb3

  let bench3_of_string f =
    let sb3 = SB3Parser.of_string f in
    sb3_of_sb3string sb3

  (* careful to delay evaluation (refs) *)
  let cad1_of_mesh1 m = S1.synth !prims1 !fuel m
  let cad2_of_mesh2 m = S2.synth !prims2 !fuel m
  let cad3_of_mesh3 m = S3.synth !prims3 !fuel m

  let cad1_of_bench1 b = S1.synth_b b
  let cad2_of_bench2 b = S2.synth_b b
  let cad3_of_bench3 b = S3.synth_b b

  let get_numsys_stats   = N.get_stats
  let print_numsys_table = N.print_table

end

module DefaultGlue (N : NUM) : GLUE = struct

  module LA  = Matrix(N)

  module Q   = PriorityQueue(N)

  module G1  = Geom1(N)
  module M1  = Mesh1(N)(G1)
  module C1  = CAD1(N)(M1)

  module G2  = Geom2(N)
  module M2  = Mesh2(N)(G2)
  module C2  = CAD2(N)(M2)

  module G3  = Geom3(N)(LA)
  module M3  = Mesh3(N)(G3)
  module C3  = CAD3(N)(M3)

  module SS1 = SBString1(M1)
  module SS2 = SBString2(M2)
  module SS3 = SBString3(M3)

  module SB1 = SynthBench1(M1)(C1)
  module SB2 = SynthBench2(M2)(C2)
  module SB3 = SynthBench3(M3)(C3)

  module S1  = Synth1(N)(Q)(G1)(M1)(C1)(SB1)
  module S2  = Synth2(N)(Q)(G2)(M2)(C2)(SB2)
  module S3  = Synth3(N)(Q)(G3)(M3)(C3)(SB3)

  module X = MakeGlue(N)(M1)(C1)(SS1)(SB1)(S1)
                        (M2)(C2)(SS2)(SB2)(S2)
                        (M3)(C3)(SS3)(SB3)(S3)
  include X

end

module OpenSCADMeshGlue (N : NUM) : GLUE = struct

  module LA  = Matrix(N)

  module Q   = PriorityQueue(N)

  module G1  = Geom1(N)
  module M1  = Mesh1(N)(G1)
  module C1  = CAD1(N)(M1)

  module G2  = Geom2(N)
  module M2  = Mesh2(N)(G2)
  module C2  = CAD2(N)(M2)

  module G3  = Geom3(N)(LA)
  module M3  = OpenSCAD.Mesh(N)(Mesh3(N)(G3))
  module C3  = CAD3(N)(M3)

  module SS1 = SBString1(M1)
  module SS2 = SBString2(M2)
  module SS3 = SBString3(M3)

  module SB1 = SynthBench1(M1)(C1)
  module SB2 = SynthBench2(M2)(C2)
  module SB3 = SynthBench3(M3)(C3)

  module S1  = Synth1(N)(Q)(G1)(M1)(C1)(SB1)
  module S2  = Synth2(N)(Q)(G2)(M2)(C2)(SB2)
  module S3  = Synth3(N)(Q)(G3)(M3)(C3)(SB3)

  module X = MakeGlue(N)(M1)(C1)(SS1)(SB1)(S1)
                        (M2)(C2)(SS2)(SB2)(S2)
                        (M3)(C3)(SS3)(SB3)(S3)
  include X

end

module OpenSCADGlue (N : NUM) : GLUE = struct

  module LA  = Matrix(N)

  module Q   = PriorityQueue(N)

  module G1  = Geom1(N)
  module M1  = Mesh1(N)(G1)
  module C1  = CAD1(N)(M1)

  module G2  = Geom2(N)
  module M2  = Mesh2(N)(G2)
  module C2  = CAD2(N)(M2)

  module G3  = Geom3(N)(LA)
  module M3  = Mesh3(N)(G3)
  module C3  = OpenSCAD.CAD(N)(M3)(CAD3(N)(M3))

  module SS1 = SBString1(M1)
  module SS2 = SBString2(M2)
  module SS3 = SBString3(M3)

  module SB1 = SynthBench1(M1)(C1)
  module SB2 = SynthBench2(M2)(C2)
  module SB3 = SynthBench3(M3)(C3)

  module S1  = Synth1(N)(Q)(G1)(M1)(C1)(SB1)
  module S2  = Synth2(N)(Q)(G2)(M2)(C2)(SB2)
  module S3  = Synth3(N)(Q)(G3)(M3)(C3)(SB3)

  module X = MakeGlue(N)(M1)(C1)(SS1)(SB1)(S1)
                        (M2)(C2)(SS2)(SB2)(S2)
                        (M3)(C3)(SS3)(SB3)(S3)
  include X

end
