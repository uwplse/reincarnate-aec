(** Lambda CAD *)

open Util
open NumSys
open Geom
open Mesh
open CAD

module type LAMBDACAD = sig

  type num

  type cad1
  type cad2
  type cad3

  type expr =
    (* prim types *)
    | Bool of bool
    | Num  of num
    | Str  of string
    | Cad1 of cad1
    | Cad2 of cad2
    | Cad3 of cad3
    | Prim of string
    (* data structures *)
    | Nil
    | Cons of expr * expr
    (* control flow *)
    | Cond of expr * expr * expr
    (* calculus *)
    | Var  of string
    | Fun  of string * expr
    | App  of expr * expr
    | Fix  of expr

  type letbinding =
    bool * string list * expr

  val mklet  : letbinding -> expr -> expr
  val mklets : letbinding list -> expr -> expr

  val mkapp         : expr   -> expr -> expr
  val prim_app      : string -> expr -> expr
  val prim_app_cons : string -> expr -> expr -> expr

  val prelude : string

  val eval    : expr -> expr
  val to_cad1 : expr -> cad1
  val to_cad2 : expr -> cad2
  val to_cad3 : expr -> cad3

  val to_string : expr -> string
end

module RawLC
  (N  : NUM)
  (C1 : CAD1 with type num = N.t)
  (C2 : CAD2 with type num = N.t)
  (C3 : CAD3 with type num = N.t)
= struct

  (*
  module Mat = LinAlg.Matrix(N)

  module G1 = Geom1(N)
  module M1 = Mesh1(N)(G1)
  module C1 = CAD1(N)(M1)

  module G2 = Geom2(N)
  module M2 = Mesh2(N)(G2)
  module C2 = CAD2(N)(M2)

  module G3 = Geom3(N)(Mat)
  module M3 = Mesh3(N)(G3)
  module C3 = CAD3(N)(M3)
  *)

  type num  = N.t
  type cad1 = C1.t
  type cad2 = C2.t
  type cad3 = C3.t


  (* TODO add mesh values? *)
  type expr =
    (* prim types *)
    | Bool of bool
    | Num  of N.t
    | Str  of string
    | Cad1 of C1.t
    | Cad2 of C2.t
    | Cad3 of C3.t
    | Prim of string
    (* data structures *)
    | Nil
    | Cons of expr * expr
    (* control flow *)
    | Cond of expr * expr * expr
    (* calculus *)
    | Var  of string
    | Fun  of string * expr
    | App  of expr * expr
    | Fix  of expr

  (* TODO pretty to_string *)
  let rec to_string = function
    | Bool b -> string_of_bool b
    | Num  n -> N.to_string  n
    | Str  s -> Printf.sprintf "%S" s
    | Cad1 c -> C1.to_string c
    | Cad2 c -> C2.to_string c
    | Cad3 c -> C3.to_string c
    | Prim p -> p
    | Nil    -> "()"
    | Var x  -> x
    | Cons (e1, e2) ->
        Printf.sprintf "(%s, %s)"
          (to_string e1)
          (to_string e2)
    | Cond (e1, e2, e3) ->
        Printf.sprintf "if %s then %s else %s"
          (to_string e1)
          (to_string e2)
          (to_string e3)
    | Fun (x, e1) ->
        Printf.sprintf "(fun %s -> %s)"
          x
          (to_string e1)
    | App (e1, e2) ->
        Printf.sprintf "(%s %s)"
          (to_string e1)
          (to_string e2)
    | Fix e1 ->
        Printf.sprintf "(fix %s)"
          (to_string e1)

  let rec is_value e =
    match e with
    | Bool _ -> true
    | Num  _ -> true
    | Str  _ -> true
    | Cad1 _ -> true
    | Cad2 _ -> true
    | Cad3 _ -> true
    | Prim _ -> true
    | Nil    -> true
    | Cond _ -> false
    | Var  _ -> false
    | Fun  _ -> true
    | App  _ -> false
    | Fix  _ -> false
    | Cons (e1, e2) ->
        is_value e1 && is_value e2

  let rec free e =
    match e with
    | Bool _ -> []
    | Num  _ -> []
    | Str  _ -> []
    | Cad1 _ -> []
    | Cad2 _ -> []
    | Cad3 _ -> []
    | Prim _ -> []
    | Nil    -> []
    | Cons (e1, e2) ->
        free e1 @ free e2
    | Cond (e1, e2, e3) ->
        free e1 @ free e2 @ free e3
    | Var x ->
        [x]
    | Fun (x, e1) ->
        List.filter ((<>) x) (free e1)
    | App (e1, e2) ->
        free e1 @ free e2
    | Fix e1 ->
        free e1

  (**
    [subst x eA eB] replaces all free occurrences of
    [Var x] with [eA] in [eB].
  *)
  let rec subst x eA eB =
    match eB with
    | Bool _ -> eB
    | Num  _ -> eB
    | Str  _ -> eB
    | Cad1 _ -> eB
    | Cad2 _ -> eB
    | Cad3 _ -> eB
    | Prim _ -> eB
    | Nil    -> eB
    | Cons (e1, e2) ->
        Cons ( subst x eA e1
             , subst x eA e2)
    | Cond (e1, e2, e3) ->
        Cond ( subst x eA e1
             , subst x eA e2
             , subst x eA e3)
    | Var v ->
        if x = v
        then eA
        else eB
    | Fun (v, e1) ->
        if x = v
        then eB
        else Fun (v, subst x eA e1)
    | App (e1, e2) ->
        App ( subst x eA e1
            , subst x eA e2)
    | Fix e1 ->
        Fix (subst x eA e1)

  let rec value_eq v1 v2 =
    match v1, v2 with
    | Bool b1, Bool b2 ->
        b1 = b2
    | Num n1, Num n2 ->
        N.equiv n1 n2
    | Str s1, Str s2 ->
        s1 = s2
    | Cad1 c1, Cad1 c2 ->
        failwith "TODO"
    | Cad2 c1, Cad2 c2 ->
        failwith "TODO"
    | Cad3 c1, Cad3 c2 ->
        failwith "TODO"
    | Nil, Nil ->
        true
    | Cons (v1a, v1b), Cons (v2a, v2b) ->
        value_eq v1a v2a &&
        value_eq v1b v2b
    | _, _ ->
        failwith @@ Printf.sprintf
          "LambdaCAD.value_eq: bogus eq test between\n  %s\nand\n  %s"
            (to_string v1)
            (to_string v2)

  let mkbool b = Bool b
  let mknum  n = Num  n
  let mkstr  s = Str  s
  let mkcad1 c = Cad1 c
  let mkcad2 c = Cad2 c
  let mkcad3 c = Cad3 c

  let prim_type_err p =
    failwith ("LambdaCAD: primitive type error on " ^ p)

  let prim_impl p =
    let prim_tab =
      [ ("Error", function
          | Str s ->
                failwith ("LC ERROR: " ^ s)
          | _ -> prim_type_err p)
      ; ("IsBool", function
          | Bool _ -> Bool true
          | _      -> Bool false)
      ; ("IsNum", function
          | Num _ -> Bool true
          | _     -> Bool false)
      ; ("IsStr", function
          | Str _ -> Bool true
          | _     -> Bool false)
      ; ("IsCad1", function
          | Cad1 _ -> Bool true
          | _      -> Bool false)
      ; ("IsCad2", function
          | Cad2 _ -> Bool true
          | _      -> Bool false)
      ; ("IsCad3", function
          | Cad3 _ -> Bool true
          | _      -> Bool false)
      ; ("IsNil", function
          | Nil -> Bool true
          | _   -> Bool false)
      ; ("IsCons", function
          | Cons _ -> Bool true
          | _      -> Bool false)
      ; ("Fst", function
          | Cons (v1, v2) ->
              v1
          | _ -> prim_type_err p)
      ; ("Snd", function
          | Cons (v1, v2) ->
              v2
          | _ -> prim_type_err p)
      ; ("Not", function
          | Bool b -> mkbool @@
              not b
          | _ -> prim_type_err p)
      ; ("Eq", function
          | Cons (v1, v2) -> mkbool @@
              value_eq v1 v2
          | _ -> prim_type_err p)
      ; ("PI", function
          | Nil -> mknum @@
              N.pi
          | _ -> prim_type_err p)
      ; ("Neg", function
          | Num n -> mknum @@
              N.neg n
          | _ -> prim_type_err p)
      ; ("Add", function
          | Cons (Num n1, Num n2) -> mknum @@
              N.add n1 n2
          | Cons (Str s1, Str s2) -> mkstr @@
              s1 ^ s2
          | _ -> prim_type_err p)
      ; ("Sub", function
          | Cons (Num n1, Num n2) -> mknum @@
              N.sub n1 n2
          | _ -> prim_type_err p)
      ; ("Mul", function
          | Cons (Num n1, Num n2) -> mknum @@
              N.mul n1 n2
          | _ -> prim_type_err p)
      ; ("Div", function
          | Cons (Num n1, Num n2) -> mknum @@
              N.div n1 n2
          | _ -> prim_type_err p)
      ; ("Rem", function
          | Cons (Num n1, Num n2) -> mknum @@
              N.rem n1 n2
          | _ -> prim_type_err p)
      ; ("Sqrt", function
          | Num n -> mknum @@
              N.sqrt n
          | _ -> prim_type_err p)
      ; ("Sin", function
          | Num n -> mknum @@
              N.sin n
          | _ -> prim_type_err p)
      ; ("Cos", function
          | Num n -> mknum @@
              N.cos n
          | _ -> prim_type_err p)
      ; ("Tan", function
          | Num n -> mknum @@
              N.tan n
          | _ -> prim_type_err p)
      ; ("Lt", function
          | Cons (Num n1, Num n2) -> mkbool @@
              (N.cmp n1 n2 = LT)
          | _ -> prim_type_err p)
      ; ("Le", function
          | Cons (Num n1, Num n2) -> mkbool @@
              (N.cmp n1 n2 <> GT)
          | _ -> prim_type_err p)
      ; ("Gt", function
          | Cons (Num n1, Num n2) -> mkbool @@
              (N.cmp n1 n2 = GT)
          | _ -> prim_type_err p)
      ; ("Ge", function
          | Cons (Num n1, Num n2) -> mkbool @@
              (N.cmp n1 n2 <> LT)
          | _ -> prim_type_err p)
      ; ("StrLen", function
          | Str s -> mknum (
              s |> String.length
                |> string_of_int
                |> N.of_string)
          | _ -> prim_type_err p)
      ; ("StrIdx", function
          | Cons (Str s, Num i) -> mkstr (
              i |> N.to_string
                |> float_of_string
                |> int_of_float
                |> String.get s
                |> Char.escaped)
          | _ -> prim_type_err p)
      ; ("StrLower", function
          | Str s -> mkstr @@
              String.lowercase_ascii s
          | _ -> prim_type_err p)
      ; ("StrUpper", function
          | Str s -> mkstr @@
              String.uppercase_ascii s
          | _ -> prim_type_err p)
      ; ("Empty1", function
          | Nil -> mkcad1 @@
              C1.Empty
          | _ -> prim_type_err p)
      ; ("Empty2", function
          | Nil -> mkcad2 @@
              C2.Empty
          | _ -> prim_type_err p)
      ; ("Empty3", function
          | Nil -> mkcad3 @@
              C3.Empty
          | _ -> prim_type_err p)
      ; ("Unit1", function
          | Nil -> mkcad1 @@
              C1.Unit
          | _ -> prim_type_err p)
      ; ("Unit2", function
          | Nil -> mkcad2 @@
              C2.Unit
          | _ -> prim_type_err p)
      ; ("Unit3", function
          | Nil -> mkcad3 @@
              C3.Unit
          | _ -> prim_type_err p)
      ; ("Hull1", function
          | Cad1 c -> mkcad1 @@
              C1.Unop (C1.Hull, c)
          | _ -> prim_type_err p)
      ; ("Hull2", function
          | Cad2 c -> mkcad2 @@
              C2.Unop (C2.Hull, c)
          | _ -> prim_type_err p)
      ; ("Hull3", function
          | Cad3 c -> mkcad3 @@
              C3.Unop (C3.Hull, c)
          | _ -> prim_type_err p)
      ; ("Trans1", function
          | Cons (Num n, Cad1 c) -> mkcad1 @@
              C1.Unop (C1.Trans n, c)
          | _ -> prim_type_err p)
      ; ("Trans2", function
          | Cons (Cons (Num n1, Num n2), Cad2 c) -> mkcad2 @@
              C2.Unop (C2.Trans (n1, n2), c)
          | _ -> prim_type_err p)
      ; ("Trans3", function
          | Cons (Cons (Num n1, Cons (Num n2, Num n3)), Cad3 c) -> mkcad3 @@
              C3.Unop (C3.Trans (n1, n2, n3), c)
          | _ -> prim_type_err p)
      ; ("Home2", function
          | Cons (Cons (Num n1, Num n2), Cad2 c) -> mkcad2 @@
              C2.Unop (C2.Home (n1, n2), c)
          | _ -> prim_type_err p)
      ; ("Home3", function
          | Cons (Cons (Num n1, Cons (Num n2, Num n3)), Cad3 c) -> mkcad3 @@
              C3.Unop (C3.Home (n1, n2, n3), c)
          | _ -> prim_type_err p)
      ; ("Scale1", function
          | Cons (Num n, Cad1 c) -> mkcad1 @@
              C1.Unop (C1.Scale n, c)
          | _ -> prim_type_err p)
      ; ("Scale2", function
          | Cons (Cons (Num n1, Num n2), Cad2 c) -> mkcad2 @@
              C2.Unop (C2.Scale (n1, n2), c)
          | _ -> prim_type_err p)
      ; ("Scale3", function
          | Cons (Cons (Num n1, Cons (Num n2, Num n3)), Cad3 c) -> mkcad3 @@
              C3.Unop (C3.Scale (n1, n2, n3), c)
          | _ -> prim_type_err p)
      ; ("Fit2", function
          | Cons (Cons (Num n1, Num n2), Cad2 c) -> mkcad2 @@
              C2.Unop (C2.Fit (n1, n2), c)
          | _ -> prim_type_err p)
      ; ("Fit3", function
          | Cons (Cons (Num n1, Cons (Num n2, Num n3)), Cad3 c) -> mkcad3 @@
              C3.Unop (C3.Fit (n1, n2, n3), c)
          | _ -> prim_type_err p)
      ; ("Rotate2", function
          | Cons (Num n, Cad2 c) -> mkcad2 @@
              C2.Unop (C2.Rotate n, c)
          | _ -> prim_type_err p)
      ; ("RotateX", function
          | Cons (Num n, Cad3 c) -> mkcad3 @@
              C3.Unop (C3.RotateX n, c)
          | _ -> prim_type_err p)
      ; ("RotateY", function
          | Cons (Num n, Cad3 c) -> mkcad3 @@
              C3.Unop (C3.RotateY n, c)
          | _ -> prim_type_err p)
      ; ("RotateZ", function
          | Cons (Num n, Cad3 c) -> mkcad3 @@
              C3.Unop (C3.RotateZ n, c)
          | _ -> prim_type_err p)
      ; ("Union1", function
          | Cons (Cad1 c1, Cad1 c2) -> mkcad1 @@
              C1.Binop (C1.Union, c1, c2)
          | _ -> prim_type_err p)
      ; ("Union2", function
          | Cons (Cad2 c1, Cad2 c2) -> mkcad2 @@
              C2.Binop (C2.Union, c1, c2)
          | _ -> prim_type_err p)
      ; ("Union3", function
          | Cons (Cad3 c1, Cad3 c2) -> mkcad3 @@
              C3.Binop (C3.Union, c1, c2)
          | _ -> prim_type_err p)
      ; ("Diff1", function
          | Cons (Cad1 c1, Cad1 c2) -> mkcad1 @@
              C1.Binop (C1.Diff, c1, c2)
          | _ -> prim_type_err p)
      ; ("Diff2", function
          | Cons (Cad2 c1, Cad2 c2) -> mkcad2 @@
              C2.Binop (C2.Diff, c1, c2)
          | _ -> prim_type_err p)
      ; ("Diff3", function
          | Cons (Cad3 c1, Cad3 c2) -> mkcad3 @@
              C3.Binop (C3.Diff, c1, c2)
          | _ -> prim_type_err p)
      ; ("Inter1", function
          | Cons (Cad1 c1, Cad1 c2) -> mkcad1 @@
              C1.Binop (C1.Inter, c1, c2)
          | _ -> prim_type_err p)
      ; ("Inter2", function
          | Cons (Cad2 c1, Cad2 c2) -> mkcad2 @@
              C2.Binop (C2.Inter, c1, c2)
          | _ -> prim_type_err p)
      ; ("Inter3", function
          | Cons (Cad3 c1, Cad3 c2) -> mkcad3 @@
              C3.Binop (C3.Inter, c1, c2)
          | _ -> prim_type_err p)
      ]
    in
    try List.assoc p prim_tab
    with Not_found ->
      failwith ("LambdaCAD: unknown primitive " ^ p)

  let eval e =
    let rec loop e =
      match e with
      | Bool _ -> e
      | Num  _ -> e
      | Str  _ -> e
      | Cad1 _ -> e
      | Cad2 _ -> e
      | Cad3 _ -> e
      | Prim _ -> e
      | Nil    -> e
      | Fun  _ -> e
      | Cons (e1, e2) ->
          Cons (loop e1, loop e2)
      | Cond (e1, e2, e3) ->
          begin match loop e1 with
          | Bool true  -> loop e2
          | Bool false -> loop e3
          | _ -> failwith "LambdaCAD.eval: branching on non-bool"
          end
      | Var _ ->
          failwith "LambdaCAD.eval: free var"
      | App (e1, e2) ->
          begin match loop e1, loop e2 with
          | Fun (x, body), v ->
              loop (subst x v body)
          | Prim p, v ->
              (prim_impl p) v
          | f, v ->
              failwith @@ Printf.sprintf
                "LambdaCAD.eval: applying non-function\n  %s\nto\n  %s"
                  (to_string f)
                  (to_string v)
          end
      | Fix e1 ->
          begin match loop e1 with
          | Fun (x, body) ->
              loop (subst x (Fix (Fun (x, body))) body)
          | _ ->
              failwith "LambdaCAD.eval: fixpoint of non-function"
          end
    in
    match free e with
    | [] -> loop e
    | xs -> failwith (Printf.sprintf "LambdaCAD.eval: free vars '%s'"
              (String.concat " " xs))

  type letbinding =
    bool * string list * expr

  (*
    let x1 x2 ... xN = eA in eB
    ==>
    (fun x1 -> eB) (fun x2 -> ... fun xN -> eA)
  *)
  let mklet (isrec, xs, eA) eB =
    let rec loop acc = function
      | [] ->
          failwith "LambdaCAD.mklet: no binder"
      | [x] ->
          if isrec
          then App (Fun (x, eB), Fix (Fun (x, acc)))
          else App (Fun (x, eB), acc)
      | x :: xs' ->
          loop (Fun (x, acc)) xs'
    in
    loop eA (List.rev xs)

    (*
  let rec mklet (isrec, xs, eA) eB =
    match xs with
    | [] ->
        failwith "LambdaCAD.mklet: no binder"
    | [x] ->
        if isrec
        then App (Fun (x, eB), Fix (Fun (x, eA)))
        else App (Fun (x, eB), eA)
    | x :: xs' ->
        mklet (isrec, xs', (Fun (x, eA))) eB
        *)

  let mklets ls e =
    ls |> List.rev
       |> List.fold_left (Util.flip mklet) e

  let mkapp e1 e2 =
    App (e1, e2)

  let prim_app p e =
    mkapp (Prim p) e

  let prim_app_cons p e1 e2 =
    prim_app p (Cons (e1, e2))

  let to_cad1 e =
    match eval e with
    | Cad1 c -> c
    | _ -> failwith "LambdaCAD.to_cad1: did not eval to CAD1"

  let to_cad2 e =
    match eval e with
    | Cad2 c -> c
    | _ -> failwith "LambdaCAD.to_cad2: did not eval to CAD2"

  let to_cad3 e =
    match eval e with
    | Cad3 c -> c
    | _ -> failwith "LambdaCAD.to_cad3: did not eval to CAD3"


  let prelude = "
#
# handy combinators
#

let curry f x y =
  f (x, y)

let uncurry f xy =
  f (Fst xy) (Snd xy)

let flip f a b =
  f b a

#
# lower case, curried, polymorphic primitives
#

let error = Error

let cons a b = (a, b)
let fst = Fst
let snd = Snd

let isbool = IsBool
let isnum  = IsNum
let isstr  = IsStr
let iscad1 = IsCad1
let iscad2 = IsCad2
let iscad3 = IsCad3
let isnil  = IsNil
let iscons = IsCons

let type_str x =
  if isbool x then
    \"Bool\"
  elif isnum x then
    \"Num\"
  elif isstr x then
    \"Str\"
  elif iscad1 x then
    \"Cad1\"
  elif iscad2 x then
    \"Cad2\"
  elif iscad3 x then
    \"Cad3\"
  elif isnil x then
    \"Nil\"
  elif iscons x then
    \"Cons\"
  else
    \"Unknown\"

let ispt2 v =
  iscons v
  && isnum (fst v)
  && isnum (snd v)

let ispt3 v =
  iscons v
  && iscons (snd v)
  && isnum (fst v)
  && isnum (fst (snd v))
  && isnum (snd (snd v))

let eq = curry Eq

let not = Not

let conj a b =
  if a
  then b
  else false

let disj a b =
  if a
  then true
  else b

let pi = PI ()

let neg  = Neg
let add  = curry Add
let sub  = curry Sub
let mul  = curry Mul
let div  = curry Div
let rem  = curry Rem
let sqrt = Sqrt
let sin  = Sin
let cos  = Cos
let tan  = Tan

let rad_of_deg d =
  d * pi / 180

let deg_of_rad r =
  r * 180 / pi

let sin_d d = sin (rad_of_deg d)
let cos_d d = cos (rad_of_deg d)
let tan_d d = tan (rad_of_deg d)

let lt = curry Lt
let le = curry Le
let gt = curry Gt
let ge = curry Ge

let strlen   = StrLen
let stridx   = curry StrIdx
let strlower = StrLower
let strupper = StrUpper

let rec conslen c =
  if isnil c
  then 0
  else 1 + conslen (snd c)

let rec len x =
  if isstr x then
    strlen x
  elif iscons x then
    conslen x
  else
    error \"len\"

let rec considx c i =
  if 0 <= i
  then fst c
  else considx (snd c) (i - 1)

let idx x i =
  if isstr x then
    stridx x i
  elif iscons x then
    considx x i
  else
    error \"idx\"

let empty1  = Empty1 ()
let unit1   = Unit1  ()
let hull1   = Hull1
let trans1  = curry Trans1
let scale1  = curry Scale1
let union1  = curry Union1
let diff1   = curry Diff1
let inter1  = curry Inter1

let empty2  = Empty2 ()
let unit2   = Unit2  ()
let hull2   = Hull2
let trans2  = curry Trans2
let home2   = curry Home2
let scale2  = curry Scale2
let fit2    = curry Fit2
let rotate2 = curry Rotate2
let union2  = curry Union2
let diff2   = curry Diff2
let inter2  = curry Inter2

let empty3  = Empty3 ()
let unit3   = Unit3  ()
let hull3   = Hull3
let trans3  = curry Trans3
let home3   = curry Home3
let scale3  = curry Scale3
let fit3    = curry Fit3
let rotateX = curry RotateX
let rotateY = curry RotateY
let rotateZ = curry RotateZ
let union3  = curry Union3
let diff3   = curry Diff3
let inter3  = curry Inter3

let cad1ish x =
  iscad1 x || isnum x

let cad1 x =
  if iscad1 x then
    x
  elif isnum x then
    scale1 x unit1
  else
    error \"not CAD1-ish\"

let cad2ish x =
  iscad2 x || ispt2 x

let cad2 x =
  if iscad2 x then
    x
  elif ispt2 x then
    scale2 x unit2
  else
    error \"not CAD2-ish\"

let cad3ish x =
  iscad3 x || ispt3 x

let cad3 x =
  if iscad3 x then
    x
  elif ispt3 x then
    scale3 x unit3
  else
    error \"not CAD3-ish\"

let hull c =
  if cad1ish c then
    hull1 (cad1 c)
  elif cad2ish c then
    hull2 (cad2 c)
  elif cad3ish c then
    hull3 (cad3 c)
  else
    error \"hull\"

let trans off c =
  if isnum off && cad1ish c then
    trans1 off (cad1 c)
  elif isnum off && cad2ish c then
    trans2 (off, off) (cad2 c)
  elif ispt2 off && cad2ish c then
    trans2 off (cad2 c)
  elif ispt3 off && cad3ish c then
    trans3 off (cad3 c)
  else
    error \"trans\"

let home pt c =
  if isnum pt && cad2ish c then
    home2 (pt, pt) (cad2 c)
  elif ispt2 pt && cad2ish c then
    home2 pt (cad2 c)
  elif ispt3 pt && cad3ish c then
    home3 pt (cad3 c)
  else
    error \"home\"

let scale fac c =
  if isnum fac && cad1ish c then
    scale1 fac (cad1 c)
  elif isnum fac && cad2ish c then
    scale2 (fac, fac) (cad2 c)
  elif ispt2 fac && cad2ish c then
    scale2 fac (cad2 c)
  elif ispt3 fac && cad3ish c then
    scale3 fac (cad3 c)
  else
    error \"scale\"

let mirror c =
  scale -1 c

let mirrorX c =
  if cad2ish c then
    scale (1, -1) c
  elif cad3ish c then
    scale (1, -1, -1) c
  else
    error \"mirrorX\"

let mirrorY c =
  if cad2ish c then
    scale (-1, 1) c
  elif cad3ish c then
    scale (-1, 1, -1) c
  else
    error \"mirrorY\"

let mirrorZ c =
  if cad3ish c then
    scale (-1, -1, 1) c
  else
    error \"mirrorZ\"

let fit box c =
  if isnum box && cad2ish c then
    fit2 (box, box) (cad2 c)
  elif ispt2 box && cad2ish c then
    fit2 box (cad2 c)
  elif ispt3 box && cad3ish c then
    fit3 box (cad3 c)
  else
    error \"fit\"

let rotate deg c =
  if isnum deg && cad2ish c then
    rotate2 deg (cad2 c)
  else
    error \"rotate\"

let rotateX deg c =
  if isnum deg && cad3ish c then
    rotateX deg (cad3 c)
  else
    error \"rotateX\"

let rotateY deg c =
  if isnum deg && cad3ish c then
    rotateY deg (cad3 c)
  else
    error \"rotateY\"

let rotateZ deg c =
  if isnum deg && cad3ish c then
    rotateZ deg (cad3 c)
  else
    error \"rotateZ\"

let union c1 c2 =
  if cad1ish c1 && cad1ish c2 then
    union1 (cad1 c1) (cad1 c2)
  elif cad2ish c1 && cad2ish c2 then
    union2 (cad2 c1) (cad2 c2)
  elif cad3ish c1 && cad3ish c2 then
    union3 (cad3 c1) (cad3 c2)
  else
    error \"union\"

let diff c1 c2 =
  if cad1ish c1 && cad1ish c2 then
    diff1 (cad1 c1) (cad1 c2)
  elif cad2ish c1 && cad2ish c2 then
    diff2 (cad2 c1) (cad2 c2)
  elif cad3ish c1 && cad3ish c2 then
    diff3 (cad3 c1) (cad3 c2)
  else
    error \"diff\"

let inter c1 c2 =
  if cad1ish c1 && cad1ish c2 then
    inter1 (cad1 c1) (cad1 c2)
  elif cad2ish c1 && cad2ish c2 then
    inter2 (cad2 c1) (cad2 c2)
  elif cad3ish c1 && cad3ish c2 then
    inter3 (cad3 c1) (cad3 c2)
  else
    error \"inter\"

#
# util
#

let rev l =
  let rec loop acc xs =
    if isnil xs then
      acc
    else
      loop (cons (fst xs) acc) (snd xs)
  in
  loop () l

let rec app l1 l2 =
  if isnil l1 then
    l2
  else
    cons (fst l1) (app (snd l1) l2)

let rec map f l =
  if isnil l then
    ()
  else
    cons (f (fst l)) (map f (snd l))

let rec fold f acc l =
  if isnil l then
    acc
  else
    fold f (f acc (fst l)) (snd l)

let rec assoc tab key =
  if isnil tab then
    ()
  else
    let h = fst tab in
    let k = fst h   in
    let v = snd h   in
    let t = snd tab in
    if key = k then
      v
    else
      assoc t key

let range n =
  let rec loop acc i =
    if i < 0 then
      acc
    else
      loop (cons i acc) (i - 1)
  in
  loop () (n - 1)

let map_circ f n =
  n |> range
    |> map (mul (360 / n))
    |> map f

let mapi f l =
  let rec loop i l =
    if isnil l then
      ()
    else
      cons (f i (fst l)) (loop (i + 1) (snd l))
  in
  loop 0 l

let explode s =
  s |> len
    |> range
    |> map (idx s)

let implode cs =
  fold add \"\" cs

let rec concat sep ss =
  if isnil ss then
    \"\"
  elif isnil (snd ss) then
    fst ss
  else
    fst ss + sep + concat sep (snd ss)

#
# derived cad primitives
#

# TRIANGLES

# right trianle given legs
# a - horizontal
# b - vertical
#
## typ tri_rt =
##   (a : R) -> (b : R) ->
##   { (x, y) : 0 <= x
##            & 0 <= y
##            & b * x + a * y <= a * b }
let tri_rt a b =
  let mask =
    (2, 2)
      |> home (0, 0.5)
      |> rotate 45
  in
  (1, 1)
    |> home 0.5
    |> flip diff mask
    |> home 0
    |> scale (a, b)

# right trianle given leg and hypotenuse
# a - horizontal
## typ tri_rt =
##   (a : R) -> (hyp : R) ->
##   { (x, y) : 0 <= x
##            & 0 <= y
##            & b * x + a * y <= a * b }
let tri_rt_hyp a hyp =
  let b =
    sqrt (hyp * hyp - a * a)
  in
  tri_rt a b

# right triangle given leg and angle
# a - horizontal
# deg - angle between a and hypotenuse
let tri_rt_deg a deg =
  let b =
    # law of sines
    (a / sin_d (90 - deg)) * sin_d deg
  in
  tri_rt a b

# right triangle given hypotenuse and angle
# deg - angle between horizontal leg and hypotenuse
let tri_rt_hyp_deg hyp deg =
  # law of sines, sin_d 90 = 1
  let a = hyp * sin_d (90 - deg) in
  let b = hyp * sin_d deg in
  tri_rt a b

# equilateral triangle with sides s
let tri_eq s =
  let half =
    tri_rt 0.5 (sin_d 60)
  in
  half
    |> mirrorY
    |> union half
    |> scale s
    |> home 0

# isosceles triangle given base b and sides s
# b - horizontal
let tri_iso b s =
  let half =
    tri_rt_hyp (b / 2) s
  in
  half
    |> mirrorY
    |> union half
    |> home 0

# isosceles triangle given base b and height h
let tri_iso_h b h =
  tri_eq 1
    |> scale (b, h)

# isosceles triangle given base b and angle
# deg - angle between base and side
let tri_iso_b_deg b deg =
  let half =
    tri_rt_deg (b / 2) deg
  in
  half
    |> mirrorY
    |> union half
    |> home 0

# isosceles triangle given side s and angle
# deg - angle between base and side
let tri_iso_s_deg s deg =
  let half =
    tri_rt_hyp_deg s deg
  in
  half
    |> mirrorY
    |> union half
    |> home 0

# isosceles triangle given height h and angle
# deg - angle between base and side
let tri_iso_h_deg h deg =
  let half =
    tri_rt_deg h (180 - 2 * deg)
  in
  half
    |> mirrorX
    |> union half
    |> rotate 90
    |> home 0

# triangle from sides
# a - \"/\" side
# b - \"\\\" side
# c - longest side, horizontal
let tri a b c =
  if a >= c || b >= c then
    error \"tri: c not longest side\"
  else
    let h =
      # heron's formula
      let area =
        let p2 = (a + b + c) / 2 in
        sqrt (p2 * (p2 - a) * (p2 - b) * (p2 - c))
      in
      2 * area / c
    in
    let at = tri_rt_hyp h a in
    let bt = tri_rt_hyp h b in
    bt |> mirrorX
       |> union at
       |> rotate 90
       |> home 0

# TODO tri_deg
# TODO tri_deg2

# HOUSE
let house b r =
  union
    (b, b)
    (trans (0, b) (tri_iso b r))

# TRAPEZOIDS

# parallelogram with side a, base b, and height h
let paragram_h a b h =
	if h > a then
    error \"paragram_h: bogus height\"
  elif h = a then
    cad2 (b, a)
  else
    let t =
      tri_rt_hyp h a
        |> rotate 90
        |> home 0
    in
    t |> rotate 180
      |> home 0
      |> trans (b, 0)
      |> union t
      |> hull

# parallelogram with side a, base b, and a/b angle deg
let paragram a b deg =
  paragram_h a b (sin_d deg * a)

# TODO
# let kite = ...

# right trapezoid with bottom base blo, top base bhi, height h
let trap_rt blo bhi h =
  let tlo =
    tri_iso_h blo h
      |> home 0
  in
  let thi =
    tri_iso_h bhi h
      |> mirrorX
      |> home 0
  in
  union tlo thi
    |> hull
    |> home 0

# isoscles trapezoid with side a, bottom base b, height h
let trap_iso_lo a b h =
  if h > a then
    error \"trap_iso_lo: bogus height\"
  elif h = a then
    cad2 (b, a)
  else
    let t =
      tri_rt_hyp h a
        |> rotate 90
        |> home 0
    in
    t |> mirrorY
      |> home (1, 0)
      |> trans (b, 0)
      |> union t
      |> hull
      |> home 0

# isoscles trapezoid with side a, top base b, height h
let trap_iso_hi a b h =
  if h > a then
    error \"trap_iso_hi: bogus height\"
  elif h = a then
    cad2 (b, a)
  else
    let t =
      tri_rt_hyp h a
        |> rotate 90
        |> home 0
    in
    t |> mirrorY
      |> home 0
      |> trans (b, 0)
      |> union (home (1, 0) t)
      |> hull
      |> home 0

# isoscles trapezoid with side a, bottom base b, and a/b angle deg
let trap_iso a b deg =
  trap_iso_lo a b (sin_d deg * a)

# isoscles trapezoid with bottom base blo, top base bhi, and height h
let trap_iso_bs blo bhi h =
  let tlo =
    tri_iso_h blo h
      |> home (0.5, 0)
  in
  let thi =
    tri_iso_h bhi h
      |> mirrorX
      |> home (0.5, 1)
      |> trans (0, h)
  in
  union tlo thi
    |> hull
    |> home 0


# POLYGONS

# n-sided regular polygon with side length s
let pgon_reg n s =
  if n < 3 then
    error \"pgon_reg_s: bogus side number\"
  else
    let apothem =
      s / (2 * tan(pi / n))
    in
    let t =
      tri_iso_h s apothem
        |> home (0.5, 1)
    in
    n |> range
      |> map (mul (360 / n))
      |> map (flip rotate t)
      |> fold union (0, 0)
      |> home 0

# n-sided regular polygon with circumcircle radius r
let pgon_reg_r n r =
  if n < 3 then
    error \"pgon_reg: bogus side number\"
  else
    # start with a half turn
    let ptr =
      tri_rt (r / 4) r
        |> mirrorX
        |> rotate (180 / n)
    in
    n |> range
      |> map (mul (360 / n))
      |> map (flip rotate ptr)
      |> fold union (0, 0)
      |> hull
      |> home 0


# CURVES

let circle_aux n =
  let base = home 0.5 (1, 1) in
  n |> range
    |> map (mul (90 / n))
    |> map (flip rotate base)
    |> fold union (0, 0)
    |> fit 1
    |> home 0

let circle n =
  n |> circle_aux
    |> hull

# 3D primitives
let sphere_aux n =
  let base = home (0.5, 0.5, 0.5) (1, 1, 1) in
  n |> range
    |> map (mul (90 / n))
    |> map (flip rotateX base)
    |> fold union (0, 0, 0)
    |> fit (1, 1, 1)
    |> home (0, 0, 0)

let sphere n =
  n |> sphere_aux
    |> hull

#
# misc
#

# gear
# d   - diameter
# n   - number of teeth
# tlo - lower tooth width
# thi - upper tooth width
# tht - tooth height
let gear d n tlo thi tht =
  let base_d =
    d - 2 * tht
  in
  let base =
    10 |> circle
       |> home 0.5
       |> scale base_d
  in
  # TODO compute good overlap
  let overlap = 0.1 in
  let tooth =
    trap_iso_bs tlo thi tht
      |> home (0.5, 0)
      |> trans (0, base_d / 2 - overlap)
  in
  n |> map_circ (flip rotate tooth)
    |> fold union base
    |> home 0

let morse_of_char c =
  let morse_tab =
    ( (\"a\", \".-\")
    , (\"b\", \"-...\")
    , (\"c\", \"-.-.\")
    , (\"d\", \"-..\")
    , (\"e\", \".\")
    , (\"f\", \"..-.\")
    , (\"g\", \"--.\")
    , (\"h\", \"....\")
    , (\"i\", \"..\")
    , (\"j\", \".---\")
    , (\"k\", \"-.-\")
    , (\"l\", \".-..\")
    , (\"m\", \"--\")
    , (\"n\", \"-.\")
    , (\"o\", \"---\")
    , (\"p\", \".--.\")
    , (\"q\", \"--.-\")
    , (\"r\", \".-.\")
    , (\"s\", \"...\")
    , (\"t\", \"-\")
    , (\"u\", \"..-\")
    , (\"v\", \"...-\")
    , (\"w\", \".--\")
    , (\"x\", \"-..-\")
    , (\"y\", \"-.--\")
    , (\"z\", \"--..\")
    , (\"0\", \"-----\")
    , (\"1\", \".----\")
    , (\"2\", \"..---\")
    , (\"3\", \"...--\")
    , (\"4\", \"....-\")
    , (\"5\", \".....\")
    , (\"6\", \"-....\")
    , (\"7\", \"--...\")
    , (\"8\", \"---..\")
    , (\"9\", \"----.\")
    , ()
    )
  in
  let m = assoc morse_tab c in
  if isnil m then
    \" \"
  else
    m

let morse_of_string s =
  s |> strlower
    |> explode
    |> map morse_of_char
    |> concat \" \"

let cad1_of_morse s =
  let space = 1 in
  let aux cad dd =
    let dur =
      if dd = \"-\" then
        3
      elif dd = \".\" then
        1
      else
        0
    in
    dur |> trans space
        |> union cad
        |> trans (- (space + dur))
  in
  s |> explode
    |> fold aux 0

let cad2_of_morse s =
  let space = 1 in
  let aux cad dd =
    let dur =
      if dd = \"-\" then
        3
      elif dd = \".\" then
        1
      else
        0
    in
    (dur, 1)
      |> trans (space, 0)
      |> union cad
      |> trans (- (space + dur), 0)
  in
  s |> explode
    |> fold aux (0, 0)

"


(* GRAVEYARD

# right triangle given short sides
# a - horizontal
# b - vertical
let tri_rt a b =
  let mask =
    union
      (home (1, 0.5) (2, 2))
      (home (0.5, 1) (2, 2))
  in
  (1, 1)
    |> home 0.5
    |> rotate 45
    |> flip diff mask
    |> fit 1
    |> scale (a, b)

# right triangle given a side and hypotenuse
# a - horizontal
let tri_rt' a hyp =
  let b =
    sqrt (hyp * hyp - a * a)
  in
  tri_rt a b

# isosceles triangle given base and side
# b - horizontal
let tri_iso b s =
  let half =
    tri_rt' (b / 2) s
  in
  half
    |> mirrorY
    |> union half
    |> home 0

# isosceles triangle given base and height
# b - horizontal
let tri_iso' b h =
  let half =
    tri_rt (b / 2) h
  in
  half
    |> mirrorY
    |> union half
    |> home 0

# equilateral triangle given side
let tri_eq s =
  pgon_reg 3 s


*)

end

module Make
  (N  : NUM)
  (C1 : CAD1 with type num = N.t)
  (C2 : CAD2 with type num = N.t)
  (C3 : CAD3 with type num = N.t)
  : (LAMBDACAD with type num  = N.t
                and type cad1 = C1.t
                and type cad2 = C2.t
                and type cad3 = C3.t)
= RawLC(N)(C1)(C2)(C3)

