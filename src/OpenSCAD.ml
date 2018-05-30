open Util
open NumSys
open Mesh
open CAD

let __PREC = 3

module OpenSCADUtil = struct

  (* TODO maybe check env var for path to openscad *)
  let compile src =
    with_tmp "openscad-compile" ".scad" (fun srcP ->
    with_tmp "openscad-compile" ".stl"  (fun stlP ->
    with_tmp "openscad-compile" ".out"  (fun outP ->
      Util.to_file srcP src;
      let cmd =
        Printf.sprintf "openscad %s -o %s > %s 2>&1"
          srcP stlP outP
      in
      if Sys.command cmd = 0 then
        Util.of_file stlP
      else begin
        let last_out =
          outP |> Util.of_file_lines
               |> List.rev
               |> (function [] -> "" | l :: _ -> l)
        in
        if last_out = "Current top level object is empty." then
          "solid EMPTY\nendsolid EMPTY"
        else
          let msg =
            Printf.sprintf "
OpenSCAD barfed!

# SCAD '%s'
%s

# STL '%s'
%s

# Output '%s'
%s

"           srcP (Util.of_file srcP)
              stlP (Util.of_file stlP)
              outP (Util.of_file outP)
          in
          prerr_endline msg;
          failwith msg
        end
    )))

end

module RawMesh
  (N  : NUM)
  (M3 : MESH3 with type num = N.t
               and type pt  = N.t * N.t * N.t)
= struct
  include M3
  module P = RParse.MakeSTLParser(N)(M3)

  let with_tmp_m m f =
    with_tmp "openscad-mesh" ".stl" (fun stlP ->
      N.with_oprec __PREC (fun () ->
        Util.to_file stlP (M3.to_stl m);
        f stlP) ())

  let meshop1 op a =
    with_tmp_m a (fun pa ->
      P.of_string @@ OpenSCADUtil.compile @@
        Printf.sprintf "%s {\n  import(\"%s\");\n} "
          op pa)

  let meshop1v op (x, y, z) =
    meshop1 (Printf.sprintf "%s([%s, %s, %s])" op
      (N.to_string x)
      (N.to_string y)
      (N.to_string z))

  let meshop2 op a b =
    with_tmp_m a (fun pa ->
    with_tmp_m b (fun pb ->
      P.of_string @@ OpenSCADUtil.compile @@
        Printf.sprintf "%s {\n  import(\"%s\");\n  import(\"%s\");\n}"
          op pa pb))

  let hull       = meshop1  "hull()"
  let trans   pt = meshop1v "translate" pt
  let scale   pt = meshop1v "scale"     pt
  let fit     pt = meshop1v "resize"    pt
  let rotateX dx = meshop1v "rotate"    (dx, N.n0, N.n0)
  let rotateY dy = meshop1v "rotate"    (N.n0, dy, N.n0)
  let rotateZ dz = meshop1v "rotate"    (N.n0, N.n0, dz)

  let union = meshop2 "union()"
  let diff  = meshop2 "difference()"
  let inter = meshop2 "intersection()"
end

module Mesh
  (N  : NUM)
  (M3 : MESH3 with type num = N.t
               and type pt  = N.t * N.t * N.t)
  : (MESH3 with type num = N.t
            and type pt  = N.t * N.t * N.t)
  = RawMesh(N)(M3)


module RawCAD
  (N  : NUM)
  (M3 : MESH3
        with type num  = N.t
         and type pt   = N.t * N.t * N.t)
  (C3 : CAD3
        with type num  = N.t
         and type mesh = M3.t)
= struct
  include C3
  module P = RParse.MakeSTLParser(N)(M3)

  let compile c =
    c |> N.with_oprec __PREC C3.to_scad
      |> OpenSCADUtil.compile
      |> P.of_string
end

module CAD
  (N : NUM)
  (M3 : MESH3
        with type num  = N.t
         and type pt   = N.t * N.t * N.t)
  (C3 : CAD3
        with type num  = N.t
         and type mesh = M3.t)
  : (CAD3 with type num  = N.t
           and type mesh = M3.t)
  = RawCAD(N)(M3)(C3)

