(** Translation among languages via a directed graph of individual translations. *)

open Util
open Glue
open Graph

module type TRANSLATE = sig
  val translate : string -> string -> unit
end

module RawTranslate (G : GLUE) = struct
  include MakeTaggedLogging(struct let tag = "Transl" end)

  type state =
    | LC      of G.lc
    | Cad1    of G.cad1
    | Cad2    of G.cad2
    | Cad3    of G.cad3
    | SCAD    of G.cad3
    | Mesh1   of G.mesh1
    | Mesh2   of G.mesh2
    | Mesh3   of G.mesh3
    | Tikz2    of G.mesh2
    | Tikz3    of G.mesh3
    | ThreeJS of G.mesh3
    | STL     of G.mesh3
    | Bench1  of G.bench1
    | Bench2  of G.bench2
    | Bench3  of G.bench3

  (* It is somewhat frustrating to have these tags, which are in 1-1
     correspondence to the variants of state. Could refactor to use GADTs. *)
  type tag =
    | TagLC
    | TagCad1
    | TagCad2
    | TagCad3
    | TagSCAD
    | TagMesh1
    | TagMesh2
    | TagMesh3
    | TagTikz2
    | TagTikz3
    | TagThreeJS
    | TagSTL
    | TagBench1
    | TagBench2
    | TagBench3

  let string_of_state = function
    | LC      c -> G.string_of_lc     c
    | Cad1    c -> G.string_of_cad1   c
    | Cad2    c -> G.string_of_cad2   c
    | Cad3    c -> G.string_of_cad3   c
    | SCAD    c -> G.scad_of_cad3     c
    | Mesh1   m -> G.string_of_mesh1  m
    | Mesh2   m -> G.string_of_mesh2  m
    | Mesh3   m -> G.string_of_mesh3  m
    | Tikz2   m -> G.tikz2_of_mesh2   m
    | Tikz3   m -> G.tikz3_of_mesh3   m
    | STL     m -> G.stl_of_mesh3     m
    | ThreeJS m -> G.threejs_of_mesh3 "REINCARNATE" m
    | Bench1  b -> G.string_of_bench1 b
    | Bench2  b -> G.string_of_bench2 b
    | Bench3  b -> G.string_of_bench3 b

  let state_name = function
    | LC      _ -> "LambdaCad"
    | Cad1    _ -> "Cad1"
    | Cad2    _ -> "Cad2"
    | Cad3    _ -> "Cad3"
    | SCAD    _ -> "OpenSCAD"
    | Mesh1   _ -> "Mesh1"
    | Mesh2   _ -> "Mesh2"
    | Mesh3   _ -> "Mesh3"
    | Tikz2    _ -> "Tikz2"
    | Tikz3    _ -> "Tikz3"
    | STL     _ -> "STL"
    | ThreeJS _ -> "ThreeJS"
    | Bench1  _ -> "Bench1"
    | Bench2  _ -> "Bench2"
    | Bench3  _ -> "Bench3"

  let string_of_tag = function
    | TagLC      -> "LambdaCad"
    | TagCad1    -> "Cad1"
    | TagCad2    -> "Cad2"
    | TagCad3    -> "Cad3"
    | TagSCAD    -> "SCAD"
    | TagMesh1   -> "Mesh1"
    | TagMesh2   -> "Mesh2"
    | TagMesh3   -> "Mesh3"
    | TagTikz2   -> "Tikz2"
    | TagTikz3   -> "Tikz3"
    | TagThreeJS -> "STL"
    | TagSTL     -> "ThreeJS"
    | TagBench1  -> "Bench1"
    | TagBench2  -> "Bench2"
    | TagBench3  -> "Bench3"

  let tag_of_extension ext =
    match ext with
    | ".lc"
    | ".lc1"
    | ".lc2"
    | ".lc3"   -> TagLC
    | ".cad1"  -> TagCad1
    | ".cad2"  -> TagCad2
    | ".cad3"  -> TagCad3
    | ".scad"  -> TagSCAD
    | ".mesh1" -> TagMesh1
    | ".mesh2" -> TagMesh2
    | ".mesh3" -> TagMesh3
    | ".sb1"   -> TagBench1
    | ".sb2"   -> TagBench2
    | ".sb3"   -> TagBench3
    | ".stl"   -> TagSTL
    | ".tikz2" -> TagTikz2
    | ".tikz3" -> TagTikz3
    | ".js"    -> TagThreeJS
    | _        -> failwith "Translate.tag_of_extension: bogus extension"

  let tag_of_state = function
    | LC      _ -> TagLC
    | Cad1    _ -> TagCad1
    | Cad2    _ -> TagCad2
    | Cad3    _ -> TagCad3
    | SCAD    _ -> TagSCAD
    | Mesh1   _ -> TagMesh1
    | Mesh2   _ -> TagMesh2
    | Mesh3   _ -> TagMesh3
    | Tikz2   _ -> TagTikz2
    | Tikz3   _ -> TagTikz3
    | STL     _ -> TagSTL
    | ThreeJS _ -> TagThreeJS
    | Bench1  _ -> TagBench1
    | Bench2  _ -> TagBench2
    | Bench3  _ -> TagBench3

(*
  let step tgt state =
    match tgt, state with
    | _,          _         ->
       failwith (Printf.sprintf "Translate.step: cannot step %s toward %s"
                                (state_name state)
                                (string_of_tag tgt))
*)

  let start_of_path p =
    match Filename.extension p with
    | ".lc"
    | ".lc1"
    | ".lc2"
    | ".lc3"   -> LC     (G.lc_of_file     p)
    | ".cad1"  -> Cad1   (G.cad1_of_file   p)
    | ".cad2"  -> Cad2   (G.cad2_of_file   p)
    | ".cad3"  -> Cad3   (G.cad3_of_file   p)
    | ".mesh1" -> Mesh1  (G.mesh1_of_file  p)
    | ".mesh2" -> Mesh2  (G.mesh2_of_file  p)
    | ".mesh3" -> Mesh3  (G.mesh3_of_file  p)
    | ".stl"   -> STL    (G.stl_of_file    p)
    | ".sb1"   -> Bench1 (G.bench1_of_file p)
    | ".sb2"   -> Bench2 (G.bench2_of_file p)
    | ".sb3"   -> Bench3 (G.bench3_of_file p)
  (*
    | ".gcode" -> GCode (RParse.gcode_of_file p)
  *)
    | _ -> failwith "Translate.start_of_path: unknown extension"

  let graph_of_edge_list es =
    List.fold_left Graph.add_edge Graph.empty es

  (** The graph of all languages and translations among them. *)
  let graph =
    let mkedge src dst f =
      { Graph.src = src
      ; Graph.dst = dst
      ; Graph.data = begin fun x ->
          log (Printf.sprintf "%s -> %s"
            (string_of_tag src) (string_of_tag dst));
          f x
        end
      }
    in

    (* There is quite a bit of verbosity in "lifting"
       various translations into the type [state -> state].
       It would be nice to investigate how well this
       works in GADT land. *)

    let lc_to_cad1 = function
      | LC e -> Cad1 (G.cad1_of_lc e)
      | _ -> failwith "lc_to_cad1: impossible"
    in
    let lc_to_cad2 = function
      | LC e -> Cad2 (G.cad2_of_lc e)
      | _ -> failwith "lc_to_cad2: impossible"
    in
    let lc_to_cad3 = function
      | LC e -> Cad3 (G.cad3_of_lc e)
      | _ -> failwith "lc_to_cad3: impossible"
    in
    let cad1_to_mesh1 = function
      | Cad1 c -> Mesh1 (G.mesh1_of_cad1 c)
      | _ -> failwith "cad1_to_mesh1: impossible"
    in
    let cad2_to_mesh2 = function
      | Cad2 c -> Mesh2 (G.mesh2_of_cad2 c)
      | _ -> failwith "cad2_to_mesh2: impossible"
    in
    let cad3_to_mesh3 = function
      | Cad3 c -> Mesh3 (G.mesh3_of_cad3 c)
      | _ -> failwith "cad3_to_mesh3: impossible"
    in
    let cad3_to_scad = function
      | Cad3 c -> SCAD c
      | _ -> failwith "cad3_to_scad: impossible"
    in
    let mesh1_to_cad1 = function
      | Mesh1 m -> Cad1 (G.cad1_of_mesh1 m)
      | _ -> failwith "mesh1_to_cad1: impossible"
    in
    let mesh2_to_cad2 = function
      | Mesh2 m -> Cad2 (G.cad2_of_mesh2 m)
      | _ -> failwith "mesh2_to_cad2: impossible"
    in
    let mesh3_to_cad3 = function
      | Mesh3 m -> Cad3 (G.cad3_of_mesh3 m)
      | _ -> failwith "mesh3_to_cad3: impossible"
    in
    let bench1_to_cad1 = function
      | Bench1 b -> Cad1 (G.cad1_of_bench1 b)
      | _ -> failwith "bench1_to_cad1: impossible"
    in
    let bench2_to_cad2 = function
      | Bench2 b -> Cad2 (G.cad2_of_bench2 b)
      | _ -> failwith "bench2_to_cad2: impossible"
    in
    let bench3_to_cad3 = function
      | Bench3 b -> Cad3 (G.cad3_of_bench3 b)
      | _ -> failwith "bench3_to_cad3: impossible"
    in
    let mesh2_to_tikz2 = function
      | Mesh2 m -> Tikz2 m
      | _ -> failwith "mesh2_to_tikz2: impossible"
    in
    let mesh3_to_tikz3 = function
      | Mesh3 m -> Tikz3 m
      | _ -> failwith "mesh3_to_tikz3: impossible"
    in
    let mesh3_to_threejs = function
      | Mesh3 m -> ThreeJS m
      | _ -> failwith "mesh3_to_threejs: impossible"
    in
    let mesh3_to_stl = function
      | Mesh3 m -> STL m
      | _ -> failwith "mesh3_to_stl: impossible"
    in
    let stl_to_mesh3 = function
      | STL m -> Mesh3 m
      | _ -> failwith "stl_to_mesh3: impossible"
    in

    (* Finally, the graph itself. *)
    graph_of_edge_list
      [ mkedge TagLC     TagCad1    lc_to_cad1
      ; mkedge TagLC     TagCad2    lc_to_cad2
      ; mkedge TagLC     TagCad3    lc_to_cad3
      ; mkedge TagCad1   TagMesh1   cad1_to_mesh1
      ; mkedge TagCad2   TagMesh2   cad2_to_mesh2
      ; mkedge TagCad3   TagMesh3   cad3_to_mesh3
      ; mkedge TagCad3   TagSCAD    cad3_to_scad
      ; mkedge TagMesh1  TagCad1    mesh1_to_cad1
      ; mkedge TagMesh2  TagCad2    mesh2_to_cad2
      ; mkedge TagMesh3  TagCad3    mesh3_to_cad3
      ; mkedge TagBench1 TagCad1    bench1_to_cad1
      ; mkedge TagBench2 TagCad2    bench2_to_cad2
      ; mkedge TagBench3 TagCad3    bench3_to_cad3
      ; mkedge TagMesh2  TagTikz2   mesh2_to_tikz2
      ; mkedge TagMesh3  TagTikz3   mesh3_to_tikz3
      ; mkedge TagMesh3  TagThreeJS mesh3_to_threejs
      ; mkedge TagMesh3  TagSTL     mesh3_to_stl
      ; mkedge TagSTL    TagMesh3   stl_to_mesh3
      ]

  (** Translate from one filesystem path to another. *)
  let translate src tgt =
    let src_tag = tag_of_extension (Filename.extension src) in
    let tgt_tag = tag_of_extension (Filename.extension tgt) in
    match Graph.search graph src_tag tgt_tag with
    | None ->
        failwith (Printf.sprintf "Translator.translate: cannot translate %s to %s"
                 (string_of_tag src_tag)
                 (string_of_tag tgt_tag))
    | Some p ->
       let translate_along p =
         Graph.fold_path_edges
           (fun _ st -> st)
           (fun e st -> e st)
           (fun f g st -> g (f st))
           p
       in
       log ("parsing input from " ^ src);
       src
         |> start_of_path
         |> translate_along p
         |> string_of_state
         |> to_file tgt;
       log ("wrote results to " ^ tgt)

end

module Translate (G : GLUE) : TRANSLATE =
  RawTranslate(G)

