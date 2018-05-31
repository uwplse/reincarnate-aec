open Logging
open Util
open Timeout
open NumSys
open MPFRNumSys
open Glue
open Translator

include MakeTaggedLogging(struct let tag = "Main" end)

type numsys =
  { ns_desc : string
  ; ns      : (module NUM)
  }

let numsys =
  [ "float",
      { ns_desc = "IEEE754 floating point (default)"
      ; ns      = (module FloatNum) }
  ; "float+sym",
      { ns_desc = "floats tagged with their symbolic values"
      ; ns      = (module FloatSymNum) }
  ; "mpfr128",
      { ns_desc = "128-bit MPFR"
      ; ns      = (module MPFRNum_128) }
  ; "mpfr1024",
      { ns_desc = "1024-bit MPFR"
      ; ns      = (module MPFRNum_1024) }
  ; "mpfr30000",
      { ns_desc = "30000-bit MPFR"
      ; ns      = (module MPFRNum_30000) }
  ; "debug",
      { ns_desc = "crash on bad rounding error"
      ; ns      = (module DebugNum) }
  ; "exact",
      { ns_desc = "Use exact trigonometric arithmetic"
      ; ns      = (module ExHackedNum) }
  ]

let numsys_usage =
  let aux (nm, ns) =
    Printf.sprintf "  %-10s  %s"
      nm ns.ns_desc
  in
  numsys
    |> List.map aux
    |> String.concat "\n"

type glue =
  { gl_desc : string
  ; gl      : (module NUM) -> (module GLUE)
  }

let glue =
  [ "reincarnate",
      { gl_desc = "Reincarnate compilers (default)"
      ; gl      = fun ns ->
                    let module N = (val ns : NUM) in
                    let module G = DefaultGlue(N) in
                    (module G) }
  ; "os-cad",
      { gl_desc = "use OpenSCAD CAD compiler"
      ; gl      = fun ns ->
                    let module N = (val ns : NUM) in
                    let module G = OpenSCADGlue(N) in
                    (module G) }
  ; "os-mesh",
      { gl_desc = "use OpenSCAD to implement Mesh operations"
      ; gl      = fun ns ->
                    let module N = (val ns : NUM) in
                    let module G = OpenSCADMeshGlue(N) in
                    (module G) }
  ]

let glue_usage =
  let aux (nm, gl) =
    Printf.sprintf "  %-10s  %s"
      nm gl.gl_desc
  in
  glue
    |> List.map aux
    |> String.concat "\n"

let usage = Printf.sprintf "
Main --src <input> --tgt <output>

Compile <input> to <output>.  The sequence of transformations is determined
by the file extensions of <input> and <output>.  Posssible extensions are:

  .lc           LambdaCAD
  .cad[1-3]     1D, 2D, 3D CAD
  .scad         OpenSCAD program
  .mesh[1-3]    1D, 2D, 3D Mesh
  .tikz[2-3]    2D, 3D tikz visualization
  .js           3D three.js visualization
  .stl          STereoLithograpy 3D Mesh

The following additional extensions are currently unsupported, but might
be again someday:

  .slicer[1-3]  1D, 2D, 3D Slicers
  .tp[1-3]      1D, 2D, 3D ToolPath
  .gcode        G-code

REQUIRED OPTIONS:

  --src <input>     read source program from input
  --tgt <output>    write target program to output

OPTIONAL OPTIONS:

  -h --help         print this usage information and exit
  --seed N          set random number generator seed to integer N
  --log <path>      write log to <path>
  --no-invariants   disable compiler invariants
  --eps-abs E       set absolute epsilon equivalence near zero
  --eps-rel E       set relative epsilon equivalence away from zero
  --epsilon E       set absolute and relative epsilons to E
  --oprec   N       set output number precision to N digits
  --timeout T       set T second timeout
  --numsys  NUMSYS  change to number system NUMSYS
  --glue    GLUE    change to glue GLUE

NUMBER SYSTEMS:

%s

GLUES:

%s
" numsys_usage glue_usage

let src = ref ""
let tgt = ref ""

let __invariants = ref true
let __numsys     = ref "float"
let __glue       = ref "reincarnate"
let __eps_abs    = ref "1e-8"
let __eps_rel    = ref "1e-8"
let __oprec      = ref "10"

let __prims1 = ref
  [ ("segment", "Unit1()") ]

let __prims2 = ref
  [ ("square", "Unit2()")
  ; ("circle", "circle 10")
  ; ("house", "house 1 1.5")
  ; ("eq_tri", "tri_eq 1")
  ; ("iso_tri", "tri_iso 1 2")
  ; ("tri", "tri 3 7 9")
  ; ("rt_tri", "tri_rt 3 5")
  ; ("para", "paragram_h 5 9 3")
  ; ("trap", "trap_rt 7 5 3")
  ; ("pgon_reg1", "pgon_reg_r 5 7")
  ; ("pgon_reg2", "pgon_reg_r 7 11")
  ]

let __prims3 = ref
  [ ("cube", "Unit")
  ; ("sphere", "Sphere")
  ; ("cylinder", "Cylinder")
  ; ("pentagon", "Pentagon")
  ; ("hexagon", "Hexagon")
  ]

let __fuel = ref 5

let __timeout    = ref 0

let parse_args () =
  let bogus_arg x =
    prerr_endline usage;
    failwith ("Main: bogus arg: " ^ x)
  in
  let rec loop = function
    | [] -> ()
    | "-h" :: rest
    | "--help" :: rest ->
        print_endline usage;
        Pervasives.exit 0
    | "--src" :: s :: rest ->
        src := s;
        loop rest
    | "--tgt" :: s :: rest ->
        tgt := s;
        loop rest
    | "--seed" :: n :: rest ->
        (* important to set this before apply_args *)
        begin try
          Util.set_rand_seed (int_of_string n);
        with _ ->
          bogus_arg ("--seed " ^ n)
        end;
        loop rest
    | "--log" :: f :: rest ->
        Logging.set_log (open_out f);
        loop rest
    | "--eps-abs" :: e :: rest ->
        __eps_abs := e;
        loop rest
    | "--eps-rel" :: e :: rest ->
        __eps_rel := e;
        loop rest
    | "--epsilon" :: e :: rest ->
        __eps_abs := e;
        __eps_rel := e;
        loop rest
    | "--oprec" :: p :: rest ->
        __oprec := p;
        loop rest
    | "--timeout" :: t :: rest ->
        (* important to set this before apply_args *)
        begin try
          __timeout := int_of_string t;
        with _ ->
          bogus_arg ("--timeout " ^ t)
        end;
        loop rest
    | "--numsys" :: x :: rest ->
        __numsys := x;
        loop rest
    | "--glue" :: x :: rest ->
        __glue := x;
        loop rest
    | "--no-invariants" :: rest ->
        __invariants := false;
        loop rest
    | "--fuel" :: x :: rest ->
        __fuel := int_of_string x;
        loop rest
    | x :: _ ->
        bogus_arg x
  in
  Sys.argv
    |> Array.to_list
    |> List.tl
    |> loop

let __transl : (module TRANSLATE) option ref =
  ref None

let apply_args () = begin

  if !src = "" then begin
    failwith "ERROR: must specify non-empty --src"
  end;
  if !tgt = "" then begin
    failwith "ERROR: must specify non-empty --tgt"
  end;

  let n =
    try
      (List.assoc !__numsys numsys).ns
    with Not_found ->
      failwith (Printf.sprintf
        "ERROR: no such number system '%s'\n" !__numsys)
  in
  let g =
    try
      (List.assoc !__glue glue).gl
    with Not_found ->
      failwith (Printf.sprintf
        "ERROR: no such glue '%s'\n" !__glue)
  in
  let module G =
    (val (g n) : GLUE)
  in

  G.set_eps_abs !__eps_abs;
  G.set_eps_rel !__eps_rel;
  G.set_oprec   !__oprec;
  G.set_invariants !__invariants;

  !__prims1
    |> List.map (fun (x, y) -> (x, G.cad1_of_lc (G.lc_of_string y)))
    |> G.set_prims1;
  !__prims2
    |> List.map (fun (x, y) -> (x, G.cad2_of_lc (G.lc_of_string y)))
    |> G.set_prims2;
(* We cannot use LC for prims right now,
   because LC doesn't support Mesh *)
(*
  !__prims3
    |> List.map (G.cad3_of_lc <| G.lc_of_string)
    |> G.set_prims3;
*)
  !__prims3
    |> List.map (fun (x, y) -> (x, G.cad3_of_string y))
    |> G.set_prims3;

  G.set_fuel !__fuel;

  __transl := Some (module Translate(G));

end

let () = begin
  parse_args ();
  log (Printf.sprintf "args parsed, rand seed is %d"
    (Util.get_rand_seed ()));
  Timeout.exn1 !__timeout (fun () ->
    apply_args ();
    let (module T) = valOf (!__transl) in
    T.translate !src !tgt
  ) ();
  log "end"
end
