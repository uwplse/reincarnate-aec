open Ocamlbuild_plugin

let (>>) x f = ignore (f x); x

let add_flag key mode =
  flag [mode; key] (S [A ("--" ^ key)])

let add_pflag key mode =
  pflag [mode] key (fun x -> (S [A ("--" ^ key); A x]))

let () =
  [ "menhir"
  ; "menhir_ocamldep" ]
  >> List.iter (add_flag  "unused-tokens")
  >> List.iter (add_flag  "unused-precedence-levels")
  >> List.iter (add_pflag "unused-token")
  >> List.iter (add_pflag "compile-errors")
  |> ignore
