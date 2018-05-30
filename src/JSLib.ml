open Util

module N = NumSys.FloatNum
module G = Glue.DefaultGlue(N)

let () = begin
  G.set_eps_abs "1e-8";
  G.set_eps_rel "1e-8";
  G.set_oprec   "5";
end

let alert msg =
  msg |> String.escaped
      |> Printf.sprintf "alert(\"%s\")"
      |> Js.Unsafe.eval_string
      |> ignore

let log msg =
  msg |> String.escaped
      |> Printf.sprintf "ide_log(\"%s\")"
      |> Js.Unsafe.eval_string
      |> ignore

let set_editor id s = ignore @@
  Js.Unsafe.eval_string @@
    Printf.sprintf "editors[\"%s\"].setValue(\"%s\")"
      (String.escaped id)
      (String.escaped s)

let view id = ignore @@
  Js.Unsafe.eval_string @@
    Printf.sprintf "view(\"%s\")" @@
      String.escaped id

let get_rand_seed s =
  () |> Util.get_rand_seed
     |> string_of_int

let set_rand_seed s =
  try
    s |> int_of_string
      |> set_rand_seed
      |> fun () -> "OK"
  with _ ->
    failwith "Seed must be an integer."

let compile_lc s =
  let tgt =
    s |> G.lc_of_string
      |> G.eval_lc
      |> G.string_of_lc
  in
  set_editor "cad" tgt;
  view "cad";
  "OK"

let compile_cad s =
  let tgt =
    try
      s |> G.cad1_of_string
        |> G.mesh1_of_cad1
        |> G.string_of_mesh1
    with ParseCommon.Error msg1 -> try
      s |> G.cad2_of_string
        |> G.mesh2_of_cad2
        |> G.string_of_mesh2
    with ParseCommon.Error msg2 -> try
      s |> G.cad3_of_string
        |> G.mesh3_of_cad3
        |> G.string_of_mesh3
    with ParseCommon.Error msg3 ->
      raise (ParseCommon.Error (
        String.concat "\n"
          [ "Could not parse as 1D, 2D, or 3D CAD.\n"
          ; "1D parse error:"
          ; msg1
          ; ""
          ; "2D parse error:"
          ; msg2
          ; ""
          ; "3D parse error:"
          ; msg3
          ]))
  in
  set_editor "mesh" tgt;
  view "mesh";
  "OK"

let synth_cad s =
  let tgt =
    try
      s |> G.mesh1_of_string
        |> G.cad1_of_mesh1
        |> G.string_of_cad1
    with ParseCommon.Error msg1 -> try
      s |> G.mesh2_of_string
        |> G.cad2_of_mesh2
        |> G.string_of_cad2
    with ParseCommon.Error msg2 -> try
      s |> G.mesh3_of_string
        |> G.cad3_of_mesh3
        |> G.string_of_cad3
    with ParseCommon.Error msg3 ->
      raise (ParseCommon.Error (
        String.concat "\n"
          [ "Could not parse as 1D, 2D, or 3D mesh.\n"
          ; "1D parse error:"
          ; msg1
          ; ""
          ; "2D parse error:"
          ; msg2
          ; ""
          ; "3D parse error:"
          ; msg3
          ]))
  in
  set_editor "cad" tgt;
  view "cad";
  "OK"

let _ =
  let reset_seed_wrap f s =
    () |> Util.get_rand_seed
       |> Util.set_rand_seed;
    f s
  in
  let js_string_wrap f s =
    s |> Js.to_string
      |> f
      |> Js.string
  in
  let alert_failure f x =
    try f x with
    | Failure msg ->
        alert(msg);
        failwith msg
(* TODO figure out exceptions and glue
    | C2.Compile (c, msg) ->
        alert("CAD2 compile error:\n" ^ msg);
        failwith msg
*)
    | ParseCommon.Error msg ->
        alert("Parse error:\n" ^ msg);
        failwith msg
  in
  let register (nm, f) =
    f |> reset_seed_wrap
      |> js_string_wrap
      |> alert_failure
      |> Js.wrap_callback
      |> Js.export nm
  in
  List.iter register
    [ ("compile_lc",     compile_lc)
    ; ("compile_cad",    compile_cad)
    ; ("synth_cad",      synth_cad)
    ; ("get_rand_seed",  get_rand_seed)
    ; ("set_rand_seed",  set_rand_seed)
    ]

