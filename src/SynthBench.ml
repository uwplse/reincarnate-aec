open Util
open NumSys
open Mesh
open CAD

module type SYNTHBENCH = sig

  type mesh
  type prim

  type t =
    { bench  : string
    ; input  : mesh
    ; fuel   : int
    ; prims  : prim list option
    }

  val prim_of_string : string -> prim
  val prim_to_string : prim   -> string
  val mkbench        : string -> mesh -> int -> prim list option -> t
  val to_string      : t -> string
end

module SBString1 (M1 : MESH1)
  : (SYNTHBENCH
     with type mesh = M1.t
      and type prim = string)
= struct

  type mesh = M1.t
  type prim = string

  type t =
    { bench  : string
    ; input  : mesh
    ; fuel   : int
    ; prims  : prim list option
    }

  let mkbench nm m f ps =
    { bench = nm
    ; input = m
    ; fuel  = f
    ; prims = ps
    }

  let prim_of_string s = s

  let prim_to_string p = p

  let string_of_prims = function
    | None -> "None"
    | Some ps ->
        ps |> String.concat "; "
           |> Printf.sprintf "Some [%s]"

  let to_string b =
    [ Printf.sprintf "Bench : %s" b.bench
    ; Printf.sprintf "Input : %s" (M1.to_string b.input)
    ; Printf.sprintf "Fuel  : %d" b.fuel
    ; Printf.sprintf "Prims : %s" (string_of_prims b.prims) ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"
end

module SBString2 (M2 : MESH2)
  : (SYNTHBENCH
     with type mesh = M2.t
      and type prim = string)
= struct

  type mesh = M2.t
  type prim = string

  type t =
    { bench  : string
    ; input  : mesh
    ; fuel   : int
    ; prims  : prim list option
    }

  let mkbench nm m f ps =
    { bench = nm
    ; input = m
    ; fuel  = f
    ; prims = ps
    }

  let prim_of_string s = s

  let prim_to_string p = p

  let string_of_prims = function
    | None -> "None"
    | Some ps ->
        ps |> String.concat "; "
           |> Printf.sprintf "Some [%s]"

  let to_string b =
    [ Printf.sprintf "Bench : %s" b.bench
    ; Printf.sprintf "Input : %s" (M2.to_string b.input)
    ; Printf.sprintf "Fuel  : %d" b.fuel
    ; Printf.sprintf "Prims : %s" (string_of_prims b.prims) ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"
end

module SBString3 (M3 : MESH3)
  : (SYNTHBENCH
     with type mesh = M3.t
      and type prim = string)
= struct

  type mesh = M3.t
  type prim = string

  type t =
    { bench  : string
    ; input  : mesh
    ; fuel   : int
    ; prims  : prim list option
    }

  let mkbench nm m f ps =
    { bench = nm
    ; input = m
    ; fuel  = f
    ; prims = ps
    }

  let prim_of_string s = s

  let prim_to_string p = p

  let string_of_prims = function
    | None -> "None"
    | Some ps ->
        ps |> String.concat "; "
           |> Printf.sprintf "Some [%s]"

  let to_string b =
    [ Printf.sprintf "Bench : %s" b.bench
    ; Printf.sprintf "Input : %s" (M3.to_string b.input)
    ; Printf.sprintf "Fuel  : %d" b.fuel
    ; Printf.sprintf "Prims : %s" (string_of_prims b.prims) ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"
end

module SynthBench1
  (M1 : MESH1)
  (C1 : CAD1 with type mesh = M1.t)
  : (SYNTHBENCH
     with type mesh = M1.t
      and type prim = C1.t)
= struct

  type mesh = M1.t
  type prim = C1.t

  type t =
    { bench  : string
    ; input  : mesh
    ; fuel   : int
    ; prims  : prim list option
    }

  let mkbench nm m f ps =
    { bench = nm
    ; input = m
    ; fuel  = f
    ; prims = ps
    }

  let prim_of_string s = failwith "BOGUS"

  let prim_to_string p = C1.to_string p

  let string_of_prims = function
    | None -> "None"
    | Some ps ->
        ps |> List.map C1.to_string
           |> String.concat "; "
           |> Printf.sprintf "Some [%s]"

  let to_string b =
    [ Printf.sprintf "Bench : %s" b.bench
    ; Printf.sprintf "Input : %s" (M1.to_string b.input)
    ; Printf.sprintf "Fuel  : %d" b.fuel
    ; Printf.sprintf "Prims : %s" (string_of_prims b.prims) ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"
end

module SynthBench2
  (M2 : MESH2)
  (C2 : CAD2 with type mesh = M2.t)
  : (SYNTHBENCH
     with type mesh = M2.t
      and type prim = C2.t)
= struct

  type mesh = M2.t
  type prim = C2.t

  type t =
    { bench  : string
    ; input  : mesh
    ; fuel   : int
    ; prims  : prim list option
    }

  let mkbench nm m f ps =
    { bench = nm
    ; input = m
    ; fuel  = f
    ; prims = ps
    }

  let prim_of_string s = failwith "BOGUS"

  let prim_to_string p = C2.to_string p

  let string_of_prims = function
    | None -> "None"
    | Some ps ->
        ps |> List.map C2.to_string
           |> String.concat "; "
           |> Printf.sprintf "Some [%s]"

  let to_string b =
    [ Printf.sprintf "Bench : %s" b.bench
    ; Printf.sprintf "Input : %s" (M2.to_string b.input)
    ; Printf.sprintf "Fuel  : %d" b.fuel
    ; Printf.sprintf "Prims : %s" (string_of_prims b.prims) ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"

end

module SynthBench3
  (M3 : MESH3)
  (C3 : CAD3
        with type mesh = M3.t)
  : (SYNTHBENCH
      with type mesh = M3.t
       and type prim = C3.t)
= struct

  type mesh = M3.t
  type prim = C3.t

  type t =
    { bench  : string
    ; input  : mesh
    ; fuel   : int
    ; prims  : prim list option
    }

  let mkbench nm m f ps =
    { bench = nm
    ; input = m
    ; fuel  = f
    ; prims = ps
    }

  let prim_of_string s = failwith "BOGUS"

  let prim_to_string p = C3.to_string p

  let string_of_prims = function
    | None -> "None"
    | Some ps ->
        ps |> List.map C3.to_string
           |> String.concat "; "
           |> Printf.sprintf "Some [%s]"

  let to_string b =
    [ Printf.sprintf "Bench : %s" b.bench
    ; Printf.sprintf "Input : %s" (M3.to_string b.input)
    ; Printf.sprintf "Fuel  : %d" b.fuel
    ; Printf.sprintf "Prims : %s" (string_of_prims b.prims) ]
    |> String.concat "\n; "
    |> Printf.sprintf "{ %s\n}"
end
