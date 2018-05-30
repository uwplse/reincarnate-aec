open Util

type t =
  | True
  | False
  | Var   of string
  | Not   of t
  | Conj  of t * t
  | Disj  of t * t

let mkconj f1 f2 = Conj (f1, f2)
let mkdisj f1 f2 = Disj (f1, f2)

let sat form env =
  let rec loop = function
    | True  -> true
    | False -> false
    | Var x -> List.mem x env
    | Not f -> not (loop f)
    | Conj (f1, f2) -> loop f1 && loop f2
    | Disj (f1, f2) -> loop f1 || loop f2
  in
  loop form
