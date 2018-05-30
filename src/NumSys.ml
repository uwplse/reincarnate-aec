(** Number Systems *)

open Util

module type REALCONST = sig
  type op1 =
    | Abs
    | Neg
    | Sqrt
    | Sin
    | Cos
    | Tan
    | Acos
    | Asin

  type op2 =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Atan2

  type t

  type view =
    | Pi
    | Lit of string
    | Op1 of op1 * t
    | Op2 of op2 * t * t
    | Unknown

  val of_view : view -> t
  val to_view : t -> view

  module ViewHashtbl : Hashtbl.S with type key = view

  val pi  : t
  val lit : string -> t
  val op1 : op1 -> t -> t
  val op2 : op2 -> t -> t -> t
  val unknown : t

  val print_self_contained : t -> unit
  val raw_to_string        : t -> string
  val to_string            : t -> string
  val get_stats            : unit -> int
  val print_table          : unit -> unit

  val get_symbol      : string -> t
  val register_symbol : string -> t -> unit
  val size            : t -> int
  val dedup_size      : t -> int
  val depth           : t -> int
  val tag             : t -> int
end

module RealConst : REALCONST = struct
  type op1 =
    | Abs
    | Neg
    | Sqrt
    | Sin
    | Cos
    | Tan
    | Acos
    | Asin

  type op2 =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Atan2

  type view =
    | Pi
    | Lit of string
    | Op1 of op1 * t
    | Op2 of op2 * t * t
    | Unknown
  and t = view Hashcons.hash_consed

  module RCHashedType = struct
    type t = view

    let equal a b =
      match a, b with
      | Pi, Pi ->
          true
      | Lit s1, Lit s2 ->
          s1 = s2
      | Op1 (o1, a1), Op1 (o2, a2) ->
          o1 = o2 && a1 == a2
      | Op2 (o1, a1, b1), Op2 (o2, a2, b2) ->
          o1 = o2 && a1 == a2 && b1 == b2
      | Unknown, Unknown ->
          true
      | _, _ ->
          false

    let combine acc n = acc * 65599 + n
    let combine2 acc n1 n2 = combine (combine acc n1) n2
    let combine3 acc n1 n2 n3 = combine (combine2 acc n1 n2) n3

    let hash = function
      | Pi -> 17
      | Lit s -> combine (Hashtbl.hash s) 13
      | Op1 (op, e) -> combine2 (Hashtbl.hash op) e.Hashcons.hkey 19
      | Op2 (op, e1, e2) ->
          combine3 (Hashtbl.hash op) e1.Hashcons.hkey e2.Hashcons.hkey 23
      | Unknown -> 11
  end

  module HC = Hashcons.Make(RCHashedType)

  let table = HC.create 17

  let intern x = HC.hashcons table x

  let zero = intern (Lit "0")
  let one = intern (Lit "1")
  let two = intern (Lit "2")
  let three = intern (Lit "3")
  let minus_one = intern (Op1 (Neg, one))
  let one_half = intern (Op2 (Div, one, two))
  let n45 = intern (Lit "45")
  let n90 = intern (Lit "90")
  let n180 = intern (Lit "180")

  let to_view x = x.Hashcons.node

  let pi = intern Pi
  let lit s =
    if s = "0.5" then one_half
    else if s = "-1" then minus_one
    else intern (Lit s)

  let unknown = intern Unknown

  let get_degrees e =
    match to_view e with
    | Op2 (Div, a, d) when d == n180 -> begin
        match to_view a with
        | Op2 (Mul, b, p) when p == pi -> Some b
        | _ -> None
      end
    | _ -> None

  let is_degrees n e =
    match get_degrees e with
    | Some d when d == n -> true
    | _ -> false

  let is_given_neg needle haystack =
    match to_view haystack with
    | Op1 (Neg, e) when e == needle -> true
    | _ -> false

  let is_given_cos_degrees n e =
    match to_view e with
    | Op1 (Cos, e) -> is_degrees n e
    | _ -> false

  let rec of_view x =
    match x with
    | Op2 (Add, e1, e2) when e1.Hashcons.tag > e2.Hashcons.tag ->
        of_view (Op2 (Add, e2, e1))
    | Op2 (Add, e1, e2) when e1 == zero -> e2
    | Op2 (Add, e1, e2) when e2 == zero -> e1
    | Op2 (Add, e1, e2) when e1 == e2 -> of_view (Op2 (Mul, two, e1))
    | Op2 (Add, e1, e2) when is_given_neg e1 e2 || is_given_neg e2 e1 -> zero
    | Op2 (Add, e1, e2) when e1 == one && e2 == two -> three
    | Op2 (Add, e1, e2) when e1 == two && e2 == minus_one -> one
    | Op2 (Add, e1, e2) when is_given_neg two e1 && e2 == one -> minus_one
    | Op2 (Add, e1, e2) -> begin
       match to_view e1, to_view e2 with
       | Op2 (Mul, e11, e12), Op2 (Mul, e21, e22)
          when e11 == e12 && e21 == e22 -> begin
          match to_view e11, to_view e21 with
          | Op1 (Sin, a), Op1 (Cos, b) when a == b -> one
          | Op1 (Cos, a), Op1 (Sin, b) when a == b -> one
          | _, _ -> intern x
         end
       | _, Op1 (Neg, e2') -> of_view (Op2 (Sub, e1, e2'))
       | _, _ -> intern x
      end
    | Op2 (Sub, e1, e2) when e1 == zero -> of_view (Op1 (Neg, e2))
    | Op2 (Sub, e1, e2) when e2 == zero -> e1
    | Op2 (Sub, e1, e2) when e1 == e2 -> zero
    | Op2 (Sub, e1, e2) when e1 == two && e2 == one -> one
    | Op2 (Sub, e1, e2) when e1 == one && e2 == two -> minus_one
    | Op2 (Sub, e1, e2) when is_given_neg e2 e1 ->
       of_view (Op1 (Neg, of_view (Op2 (Mul, two, e2))))
    | Op2 (Sub, e1, e2) -> begin
        match to_view e1, to_view e2 with
        | Op1 (Neg, e1'), _ -> of_view (Op1 (Neg, of_view (Op2 (Add, e1', e2))))
        | _, Op1 (Neg, e2') -> of_view (Op2 (Add, e1, e2'))
        | _, Op2 (Mul, e21, e22) when e21 == two && e22 == e1 ->
           of_view (Op2 (Mul, minus_one, e1))
        | Op2 (Mul, e11, e12), _ when e11 == two && e12 == e2 ->
           of_view (Op2 (Mul, one, e2))
        | Op2 (Add, e11, e12), _ when e11 == e2 -> e12
        | _, Op2 (Add, e21, e22) when e21 == e1 ->
           of_view (Op1 (Neg, e22))
        | Op2 (Sub, e11, e12), _ when e11 == e2 ->
           of_view (Op1 (Neg, e12))
        | _, Op2 (Sub, e21, e22) when e21 == e1 ->
           e22
        | _, _ -> intern x
      end

    | Op2 (Mul, e1, e2) when e1.Hashcons.tag > e2.Hashcons.tag -> of_view (Op2 (Mul, e2, e1))
    | Op2 (Mul, e1, e2) when e1 == zero || e2 == zero -> zero
    | Op2 (Mul, e1, e2) when e1 == one -> e2
    | Op2 (Mul, e1, e2) when e2 == one -> e1
    | Op2 (Mul, e1, e2) when e1 == minus_one -> of_view (Op1 (Neg, e2))
    | Op2 (Mul, e1, e2) when e2 == minus_one -> of_view (Op1 (Neg, e1))
    | Op2 (Mul, e1, e2) when e1 == two && e2 == one_half -> one
    | Op2 (Mul, e1, e2) when is_given_cos_degrees n45 e1 && is_given_cos_degrees n45 e2 ->
       one_half

    | Op2 (Mul, e1, e2) -> begin
       match to_view e1, to_view e2 with
       | _, Op2 (Mul, e21, e22) when is_given_cos_degrees n45 e1 && e21 == two && e22 == e1 ->
          one
       | Op1 (Neg, e1'), Op1 (Neg, e2') -> of_view (Op2 (Mul, e1', e2'))
       | Op1 (Neg, e1'), _ -> of_view (Op1 (Neg, of_view (Op2 (Mul, e1', e2))))
       | _, Op1 (Neg, e2') -> of_view (Op1 (Neg, of_view (Op2 (Mul, e1, e2'))))
       | _, Op2 (Div, e21, e22) when e22 == e1 -> e21
       | _, _ -> intern x
      end

    | Op2 (Div, e1, e2) when e1 == zero -> zero
    | Op2 (Div, e1, e2) when e2 == one -> e1
    | Op2 (Div, e1, e2) when e2 == minus_one -> of_view (Op1 (Neg, e1))
    | Op2 (Div, e1, e2) when e1 == e2 -> one
    | Op2 (Div, e1, e2) -> begin
       match to_view e1, to_view e2 with
       | Op1 (Neg, e1'), Op1 (Neg, e2') -> of_view (Op2 (Div, e1', e2'))
       | Op1 (Neg, e1'), _ -> of_view (Op1 (Neg, of_view (Op2 (Div, e1', e2))))
       | _, Op1 (Neg, e2') -> of_view (Op1 (Neg, of_view (Op2 (Div, e1, e2'))))
       | _, _ -> intern x
      end


    | Op2 (Rem, e1, e2) when e1 == zero -> zero

    | Op1 (Sin, e) when e == zero -> zero
    | Op1 (Sin, e) when is_degrees zero e -> zero
    | Op1 (Sin, e) when is_degrees n90 e -> one
    | Op1 (Sin, e) when is_degrees n45 e -> of_view (Op1 (Cos, e))

    | Op1 (Cos, e) when e == zero -> one
    | Op1 (Cos, e) when is_degrees zero e -> one
    | Op1 (Cos, e) when is_degrees n90 e -> zero


    | Op1 (Neg, e) when e == zero -> zero
    | Op1 (Neg, e) -> begin
        match to_view e with
        | Op1 (Neg, e') -> e'
        | _ -> intern x
      end

    | Op1 (Abs, e) when e == zero -> zero
    | Op1 (Abs, e) when e == one -> one
    | Op1 (Abs, e) when e == minus_one -> one
    | Op1 (Abs, e) -> begin
        match to_view e with
        | Op1 (Neg, e') -> of_view (Op1 (Abs, e'))
        | _ -> intern x
      end


    | Op1 (Sqrt, e) when e == zero -> zero
    | Op1 (Sqrt, e) when e == one -> one


    | _ -> intern x

  let fmt_op1 = function
    | Abs  -> Printf.sprintf "abs(%s)"
    | Neg  -> Printf.sprintf "-%s"
    | Sqrt -> Printf.sprintf "sqrt(%s)"
    | Sin  -> Printf.sprintf "sin(%s)"
    | Cos  -> Printf.sprintf "cos(%s)"
    | Tan  -> Printf.sprintf "tan(%s)"
    | Acos -> Printf.sprintf "acos(%s)"
    | Asin -> Printf.sprintf "asin(%s)"

  let fmt_op2 = function
    | Add   -> Printf.sprintf "(%s + %s)"
    | Sub   -> Printf.sprintf "(%s - %s)"
    | Mul   -> Printf.sprintf "(%s * %s)"
    | Div   -> Printf.sprintf "(%s / %s)"
    | Rem   -> Printf.sprintf "(%s rem %s)"
    | Atan2 -> Printf.sprintf "atan2(%s, %s)"

  module IntSet = Set.Make(struct
      type t = int
      let compare = Pervasives.compare
  end)

  let varname e = Printf.sprintf "x%d" e.Hashcons.tag

  let abbreviate_degrees e =
    match get_degrees e with
    | Some d -> d
    | _ -> e

  let rec raw_to_string e =
    match to_view e with
    | Pi ->
       "pi"
    | Lit x ->
       x
    | Op1 (op, a) ->
       let arg = match op with
         | Sin | Cos -> abbreviate_degrees a
         | _ -> a
       in
       fmt_op1 op (raw_to_string arg)
    | Op2 (op, a, b) ->
       fmt_op2 op (raw_to_string a) (raw_to_string b)
    | Unknown ->
       "unk"

  let entry_rhs v =
    match v with
    | Pi ->
       "pi"
    | Lit x ->
        x
    | Op1 (op, a) ->
       let arg = match op with
         | Sin | Cos -> abbreviate_degrees a
         | _ -> a
       in
       fmt_op1 op (varname arg)
    | Op2 (op, a, b) ->
        fmt_op2 op (varname a) (varname b)
    | Unknown -> "unk"


  let print_table_entry e =
    Printf.printf "let %s = %s\n" (varname e) (entry_rhs (to_view e))

  let print_table () =
    HC.iter print_table_entry table

  let to_string e =
    varname e

  let op1 op a =
    match to_view a with
    | Unknown -> of_view Unknown
    | _ -> of_view (Op1 (op, a))

  let op2 op a b =
    match to_view a, to_view b with
    | Unknown, Unknown -> of_view Unknown
    | Unknown, _ ->
       of_view Unknown
    | _, Unknown ->
       of_view Unknown
    | _, _ -> of_view (Op2 (op, a, b))

  let get_stats () =
    let (n, _, _, _, _, _) = HC.stats table in
    n

  let table : (string, t) Hashtbl.t = Hashtbl.create 17

  let get_symbol = Hashtbl.find table
  let register_symbol = Hashtbl.add table

  module ViewHashtbl = Hashtbl.Make(RCHashedType)

  let rec size' h e =
    let e = to_view e in
    try ViewHashtbl.find h e
    with _ ->
    let ans =
      match e with
      | Pi -> 1
      | Lit x -> 1
      | Op1 (op, a) -> 1 + size' h a
      | Op2 (op, a, b) -> 1 + size' h a + size' h b
      | Unknown -> 1
    in
    ViewHashtbl.add h e ans;
    ans

  let size = size' (ViewHashtbl.create 17)

  let dedup_size e =
    let h = ViewHashtbl.create 17 in
    ignore (size' h e);
    ViewHashtbl.length h

  let rec depth' h e =
    let e = to_view e in
    try ViewHashtbl.find h e
    with _ ->
    let ans =
      match e with
      | Pi -> 1
      | Lit x -> 1
      | Op1 (op, a) -> 1 + depth' h a
      | Op2 (op, a, b) -> 1 + Pervasives.max (depth' h a) (depth' h b)
      | Unknown -> 1
    in
    ViewHashtbl.add h e ans;
    ans

  let depth = depth' (ViewHashtbl.create 17)

  let print_self_contained e =
    let rec loop h acc e =
      let v = to_view e in
      try ViewHashtbl.find h v; acc
      with _ -> begin
          ViewHashtbl.add h v ();
          let acc = e :: acc in
          match v with
          | Op1 (op, a) -> loop h acc a
          | Op2 (op, a, b) -> loop h (loop h acc a) b
          | _ -> acc
        end
    in
    let h = ViewHashtbl.create 17 in
    let l = List.rev (loop h [] e) in
    List.iter print_table_entry l

  let tag e = e.Hashcons.tag
end

module type NUM_CORE = sig
  type t

  val of_string : string -> t

  val pi : t

  val neg  : t -> t
  val abs  : t -> t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val div  : t -> t -> t
  val rem  : t -> t -> t
  val sqrt : t -> t

  (** trig functions are in radians *)

  val sin   : t -> t
  val cos   : t -> t
  val tan   : t -> t
  val asin  : t -> t
  val acos  : t -> t
  val atan2 : t -> t -> t

  (** [order] provides a total order on [t] *)
  val order : t -> t -> cmp

  (** pretty printing with precision controls *)

  val get_oprec : unit -> int
  val set_oprec : int  -> unit
  val to_string : t    -> string

  val forget : t -> t

  val get_stats : unit -> int
  val print_table : unit -> unit
end

(** Generic number system interface *)
module type NUM = sig
  include NUM_CORE

  val n0    : t
  val n1    : t
  val n2    : t
  val n3    : t
  val n4    : t
  val n5    : t
  val n6    : t
  val n180  : t
  val n360  : t
  val twoPi : t

  val of_int   : int -> t
  val of_float : float -> t
  val of_const : RealConst.t -> t

  (** run function under a temporary output precision *)
  val with_oprec : int -> ('a -> 'b) -> 'a -> 'b

  (**
     [eps_abs] used to decide if numbers near [0] are "equivalent"
     [eps_rel] used to decide if numbers far from [0] are "equivalent"
  *)

  val set_eps_abs : t -> unit
  val set_eps_rel : t -> unit
  val get_eps_abs : unit -> t
  val get_eps_rel : unit -> t

  (**
     [equiv] returns true iff its arguments are "close".
     EQUIV IS NOT GUARANTEED TO BE TRANSITIVE!
  *)
  val equiv  : t -> t -> bool
  val equivs : t list -> bool

  val cmp  : t -> t -> cmp
  val sum  : t list -> t
  val rand : unit -> t

  val rad_of_deg : t -> t
  val deg_of_rad : t -> t
end

module MakeOfConst (NC : NUM) = struct
  include NC

  module RC = RealConst

  let of_const x =
    let denote_op1 = function
      | RC.Abs  -> NC.abs
      | RC.Neg  -> NC.neg
      | RC.Sqrt -> NC.sqrt
      | RC.Sin  -> NC.sin
      | RC.Cos  -> NC.cos
      | RC.Tan  -> NC.tan
      | RC.Acos -> NC.acos
      | RC.Asin -> NC.asin
    in
    let denote_op2 = function
      | RC.Add   -> NC.add
      | RC.Sub   -> NC.sub
      | RC.Mul   -> NC.mul
      | RC.Div   -> NC.div
      | RC.Rem   -> NC.rem
      | RC.Atan2 -> NC.atan2
    in
    let rec denote h e =
      let v = RC.to_view e in
      try RC.ViewHashtbl.find h v
      with Not_found ->
        let ans =
          match v with
          | RC.Pi ->
              NC.pi
          | RC.Lit x ->
              NC.of_string x
          | RC.Op1 (op, a) ->
              denote_op1 op (denote h a)
          | RC.Op2 (op, a, b) ->
              denote_op2 op (denote h a) (denote h b)
          | RC.Unknown ->
              failwith "RealConst.denote: Unknown!"
        in
        RC.ViewHashtbl.add h v ans;
        ans
    in
    denote (RC.ViewHashtbl.create 17) x
end

module MakeNumSys (NC : NUM_CORE) :
  NUM with type t = NC.t
= struct
  include NC

  let n0    = NC.of_string "0"
  let n1    = NC.of_string "1"
  let n2    = NC.of_string "2"
  let n3    = NC.of_string "3"
  let n4    = NC.of_string "4"
  let n5    = NC.of_string "5"
  let n6    = NC.of_string "6"
  let n180  = NC.of_string "180"
  let n360  = NC.of_string "360"
  let twoPi = NC.add NC.pi NC.pi

  let of_int   x = NC.of_string (string_of_int   x)
  let of_float x = NC.of_string (string_of_float x)

  module RC = RealConst

  let of_const x =
    let denote_op1 = function
      | RC.Abs  -> NC.abs
      | RC.Neg  -> NC.neg
      | RC.Sqrt -> NC.sqrt
      | RC.Sin  -> NC.sin
      | RC.Cos  -> NC.cos
      | RC.Tan  -> NC.tan
      | RC.Acos -> NC.acos
      | RC.Asin -> NC.asin
    in
    let denote_op2 = function
      | RC.Add   -> NC.add
      | RC.Sub   -> NC.sub
      | RC.Mul   -> NC.mul
      | RC.Div   -> NC.div
      | RC.Rem   -> NC.rem
      | RC.Atan2 -> NC.atan2
    in
    let rec denote h e =
      let v = RC.to_view e in
      try RC.ViewHashtbl.find h v
      with Not_found ->
        let ans =
          match v with
          | RC.Pi ->
              NC.pi
          | RC.Lit x ->
              NC.of_string x
          | RC.Op1 (op, a) ->
              denote_op1 op (denote h a)
          | RC.Op2 (op, a, b) ->
              denote_op2 op (denote h a) (denote h b)
          | RC.Unknown ->
              failwith "RealConst.denote: Unknown!"
        in
        RC.ViewHashtbl.add h v ans;
        ans
    in
    denote (RC.ViewHashtbl.create 17) x

  let with_oprec p f x =
    let q = get_oprec () in
    set_oprec p;
    let r = f x in
    set_oprec q;
    r

  (* Q: how much perf is sacrificed by allowing these to change? *)
  let eps_abs       = ref n0
  let eps_rel       = ref n0
  let set_eps_abs e = eps_abs := e
  let set_eps_rel e = eps_rel := e
  let get_eps_abs e = !eps_abs
  let get_eps_rel e = !eps_rel

  (* adapted from:
   *   http://floating-point-gui.de/errors/comparison/
   *   https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
   *)
  let equiv a b =
    let diff = NC.abs (NC.sub a b) in
    if NC.order diff !eps_abs <> GT then
      (* needed when comparing near 0 *)
      true
    else
      let maxAB = max NC.order (NC.abs a) (NC.abs b) in
      NC.order diff (NC.mul maxAB !eps_rel) <> GT

  (* avoid depending on list order *)
  let rec equivs ns =
    ns |> Util.sort NC.order
       |> Util.pair_chain
       |> List.for_all (uncurry equiv)

  let cmp a b =
    if equiv a b
    then EQ
    else NC.order a b

  let sum ns =
    ns |> Util.sort NC.order
       |> List.fold_left NC.add n0

  let rand () =
    let radius = 1.0 in
    let x = Random.float (2.0 *. radius) -. radius in
    NC.of_string (Printf.sprintf "%0.1f" x)

  let rad_of_deg deg =
    NC.div (NC.mul deg NC.pi) n180

  let deg_of_rad rad =
    NC.div (NC.mul rad n180) NC.pi
end

(** Adding invariant checks to a number system *)

module type CHECKED_CONFIG = sig
  type t
  val check : t -> t
end

module MakeCheckedNumCore
  (NC : NUM_CORE)
  (CC : CHECKED_CONFIG with type t = NC.t)
  : (NUM_CORE with type t = NC.t)
= struct
  include NC

  let ck1 f a   = CC.check (f a)
  let ck2 f a b = CC.check (f a b)

  let of_string = ck1 NC.of_string

  let pi = CC.check NC.pi

  let neg  = ck1 NC.neg
  let abs  = ck1 NC.abs
  let add  = ck2 NC.add
  let sub  = ck2 NC.sub
  let mul  = ck2 NC.mul
  let div  = ck2 NC.div
  let rem  = ck2 NC.rem
  let sqrt = ck1 NC.sqrt

  let sin   = ck1 NC.sin
  let cos   = ck1 NC.cos
  let tan   = ck1 NC.tan
  let asin  = ck1 NC.asin
  let acos  = ck1 NC.acos
  let atan2 = ck2 NC.atan2
end

(** Floating point number system *)

module FloatNumCore : (NUM_CORE with type t = float) = struct
  type t = float

  let of_string = Pervasives.float_of_string

  let pi = 3.1415926535897932384626433832795028841971693993751058209749445923

  let neg  = ( ~-. )
  let abs  = Pervasives.abs_float
  let add  = ( +. )
  let sub  = ( -. )
  let mul  = ( *. )
  let div  = ( /. )
  let rem  = mod_float
  let sqrt = Pervasives.sqrt

  let sin   = Pervasives.sin
  let cos   = Pervasives.cos
  let tan   = Pervasives.tan
  let asin  = Pervasives.asin
  let acos  = Pervasives.acos
  let atan2 = Pervasives.atan2

  let order =
    cmp_of_cmpi Pervasives.compare

  let __oprec      = ref 15
  let get_oprec () = !__oprec
  let set_oprec p  = __oprec := p
  let to_string f  = Printf.sprintf "%.*f" (get_oprec ()) f

  let forget x = x
  let get_stats () = 0
  let print_table () = ()
end

module FloatNum : (NUM with type t = float) =
  MakeNumSys(FloatNumCore)

module OrdinaryFloatNumCore : (NUM_CORE with type t = float) =
  MakeCheckedNumCore(FloatNumCore)(struct
    type t = float

    let check x =
      match classify_float x with
      | FP_normal    -> x
      | FP_zero      -> x
      | FP_subnormal -> failwith "OrdinaryFloat: subnormal"
      | FP_infinite  -> failwith "OrdinaryFloat: infinity"
      | FP_nan       -> failwith "OrdinaryFloat: nan"
  end)

module OrdinaryFloatNum : NUM =
  MakeNumSys(OrdinaryFloatNumCore)

(** Symbolic number system *)

module SymNumCore : NUM_CORE = struct
  module RC = RealConst

  type t = RC.t

  let known_consts =
    [ "0"; "1"; "-1"; "2"; "3"; "4"; "5"
    ; "30"; "45"; "60"; "90"; "180"; "360"
    ; "18"; "36"; "54"; "72"
    ]

  let of_string x =
    if List.mem x known_consts then
      RC.lit x
    else begin
      RC.unknown
    end

  let pi = RC.pi

  let neg  = RC.op1 RC.Neg
  let abs  = RC.op1 RC.Abs
  let add  = RC.op2 RC.Add
  let sub  = RC.op2 RC.Sub
  let mul  = RC.op2 RC.Mul
  let div  = RC.op2 RC.Div
  let rem  = RC.op2 RC.Rem
  let sqrt = RC.op1 RC.Sqrt

  let sin   = RC.op1 RC.Sin
  let cos   = RC.op1 RC.Cos
  let tan   = RC.op1 RC.Tan
  let acos  = RC.op1 RC.Acos
  let asin  = RC.op1 RC.Asin
  let atan2 = RC.op2 RC.Atan2

  let order a b =
    failwith "SymNum: cannot order symbolic numbers"

  (** precision ignored *)
  let get_oprec () = -1
  let set_oprec p  = ()
  let to_string x  = RC.raw_to_string x

  let forget x = RC.unknown
  let get_stats = RC.get_stats
  let print_table = RC.print_table
end

module SymNum : NUM =
  MakeNumSys(SymNumCore)

module NumCorePair (A : NUM_CORE) (B : NUM_CORE) : NUM_CORE = struct

  type t = A.t * B.t

  let of_string s = (A.of_string s, B.of_string s)
  let to_string (a,b) =
    Printf.sprintf "(%s, %s)" (A.to_string a) (B.to_string b)

  let lift_unary f g (a, b) = (f a, g b)
  let lift_binary f g (a, b) (c, d) = (f a c, g b d)

  let pi        = (A.pi, B.pi)
  let neg       = lift_unary A.neg B.neg
  let abs       = lift_unary A.abs B.abs
  let add       = lift_binary A.add B.add
  let sub       = lift_binary A.sub B.sub
  let mul       = lift_binary A.mul B.mul
  let div       = lift_binary A.div B.div
  let rem       = lift_binary A.rem B.rem
  let sqrt      = lift_unary A.sqrt B.sqrt
  let sin       = lift_unary A.sin B.sin
  let cos       = lift_unary A.cos B.cos
  let tan       = lift_unary A.tan B.tan
  let asin      = lift_unary A.asin B.asin
  let acos      = lift_unary A.acos B.acos
  let atan2     = lift_binary A.atan2 B.atan2

  let order (a1,_) (a2,_) = A.order a1 a2

  (* Precision is passed through to A. *)
  let get_oprec () = A.get_oprec ()
  let set_oprec p  = A.set_oprec p

  let forget = lift_unary A.forget B.forget
  let get_stats () = A.get_stats () + B.get_stats ()
  let print_table () = A.print_table (); B.print_table ()
end

module ExHackedNum : NUM with type t =
  ExactArith.ExHackedArith.t = MakeOfConst(ExactArith.ExHackedArith)
module FloatSymNumCore = NumCorePair(FloatNumCore)(SymNumCore)
module FloatSymNum = MakeNumSys(FloatSymNumCore)
