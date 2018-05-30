open Util
open NumSys
open Mesh

module Make
  (N : NUM)
  (M : MESH3 with type num = N.t
              and type pt  = N.t * N.t * N.t) :
sig
  val of_file : string -> M.t
end = struct

  (* read Int32 from input channel `ic` in little endian order *)
  let read_i32_le ic =
    let b0 = Int32.of_int (input_byte ic) in
    let b1 = Int32.shift_left (Int32.of_int (input_byte ic))  8 in
    let b2 = Int32.shift_left (Int32.of_int (input_byte ic)) 16 in
    let b3 = Int32.shift_left (Int32.of_int (input_byte ic)) 24 in
    Int32.logor b3 (Int32.logor b2 (Int32.logor b1 b0))

  let read_int ic =
    ic |> read_i32_le
       |> Int32.to_int

  let read_num ic =
    ic |> read_i32_le
       |> Int32.float_of_bits
       |> N.of_float

  let read_num_triple ic =
    let x = read_num ic in
    let y = read_num ic in
    let z = read_num ic in
    (x, y, z)

  let rec burn n ic =
    if n > 0 then begin
      let _ = input_char ic in
      burn (n - 1) ic
    end

  let read_face ic =
    (* ignore normal *)
    let _ = burn 12 ic in
    let pt1 = read_num_triple ic in
    let pt2 = read_num_triple ic in
    let pt3 = read_num_triple ic in
    (* ignore attribute byte count *)
    let _ = burn 2 ic in
    (pt1, pt2, pt3)

  let of_file path =
    let ic = Pervasives.open_in_bin path in
    (* ignore header *)
    let _ = burn 80 ic in
    let rec loop i acc =
      if i <= 0 then
        M.mesh acc
      else
        loop (i - 1) (read_face ic :: acc)
    in
    let m = loop (read_int ic) [] in
    Pervasives.close_in ic;
    m
end
