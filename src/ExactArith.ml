
open Big_int_Z
open Util

let display_float = ref true
let display_exact = ref false
let display_field = ref false

let rec last = function
  | [] -> failwith "last: empty list"
  | [x] -> x
  | n :: ns -> last ns

let rec drop_last = function
  | [] -> failwith "drop_last: empty list"
  | [x] -> []
  | x :: ns -> x :: drop_last ns

let rec my_zip2 f a b =
  match a with
  | [] -> []
  | (a' :: an) ->
     match b with
     | (b' :: bn) -> f a' b' :: (my_zip2 f an bn)
     | [] -> a

let rec extend l n v =
  if n = 0
  then []
  else
    match l with
    | [] -> v :: extend l (n - 1) v
    | l' :: ls -> l' :: extend ls (n - 1) v

let rec repeat f n v =
  if n = 0
  then v
  else repeat f (n - 1) (f v)

let rec concatMaybe = function
  | [] -> []
  | Some x :: l' -> x :: concatMaybe l'
  | None :: l' -> concatMaybe l'

let init n f =
  let rec init' f n i = if i = n then [] else f i :: init' f n (i + 1) in
  init' f n 0

let dbl_big_int n =
  mult_big_int n (big_int_of_int 2)

let gcd_big_int p q =
  if eq_big_int p zero_big_int
  then q
  else if eq_big_int q zero_big_int
  then p
  else gcd_big_int p q (* Not recursive! *)

let reduce_frac p q =
  let g = gcd_big_int p q in
  div_big_int p g, div_big_int q g

let rec rat_of_string s =
  let s = String.trim s in
  if String.contains s 'e'
  then match String.split_on_char 'e' s with
       | [ frac_s; exp_s ] ->
          let n, d = rat_of_string frac_s in
          let exp = int_of_string exp_s in
          let multiplier = power_int_positive_int 10 (abs exp) in
          if exp < 0
          then reduce_frac n (mult_big_int d multiplier)
          else reduce_frac (mult_big_int d multiplier) d
       | _ -> failwith "rat_of_string: input string is ill formed!"
  else if String.contains s '.'
  then let p = String.split_on_char '.' s in
       assert (List.length p = 2);
       let frac_part = List.nth p 1 in
       let nexp = big_int_of_int (String.length frac_part) in
       let num = String.concat "" [ List.nth p 0 ; frac_part ] in
       reduce_frac (big_int_of_string num) (power_int_positive_big_int 10 nexp)
  else if String.contains s '/'
  then let p = String.split_on_char '/' s in
       assert (List.length p = 2);
       reduce_frac (big_int_of_string (List.nth p 0)) (big_int_of_string (List.nth p 1))
  else big_int_of_string s, unit_big_int

let every_nth l g =
  let a = Array.of_list l in
  let n = Array.length a in
  init (n / g) (fun i -> a.(i * g))

(* O(n); output size O(n^(1 / log log n)) *)
let divisors n =
  let rec f' i =
    if i = n
    then [i]
    else
      let tl = f' (i + 1) in
      if n mod i = 0 then i :: tl else tl
  in
  f' 1

(* O(l log log max(l)) *)
let sieve l =
  let rec sieve' l ps =
    match l with
    | [] -> ps
    | pc :: l' ->
       if List.exists (fun p -> pc mod p = 0) ps
       then sieve' l' ps
       else sieve' l' (pc :: ps)
  in
  sieve' l []

(* O(n) *)
let maximal_odd_divisors n =
  let divisors = List.filter (fun x -> x mod 2 = 1) (List.tl (divisors n)) in (* O(n) *)
  let primes = sieve divisors in (* O((log log n) n^(1 / log log n))  *)
  List.map (fun p -> n / p) primes

module Chebynomial : sig
  type t

  val to_float : t -> float
  val to_string : t -> string

  val of_string : string -> t
  val of_int : int -> t
  val of_big_int : big_int -> t
  val of_rat : big_int -> big_int -> t

  val zero : t
  val one : t
  val sqrt2 : t
  val sqrt3 : t
  val sqrt5 : t

  val neg : t -> t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t

  val cos : t -> t

  val positive : t -> bool

  val equal : t -> t -> bool

  (** A fast check for whether the argument is zero.
      If [quick_zero] returns true, then the argument is definitely zero,
      but if it returns false, then it might or might not be zero. *)
  val quick_zero : t -> bool
  val quick_is_rat : t -> bool
  val quick_get_rat : t -> big_int * big_int

end = struct
  type t = big_int * (big_int list) * (float * float) option ref

  exception UnsupportedFunction of string

  (* O(1) *)
  let of_big_int n : t = (unit_big_int, [n], ref None)
  let of_int n : t = of_big_int (big_int_of_int n)
  let of_rat a b : t = let a, b = reduce_frac a b in (b, [a], ref None)
  let of_string x : t = let a, b = rat_of_string x in of_rat a b


  let zero = of_big_int zero_big_int
  let one = of_big_int unit_big_int
  let pi = one (* TODO: Fix *)
  let twoPi = one (* TODO: Fix *)

  (* A few common square roots *)
  let two_big_int = big_int_of_int 2
  let sqrt2 = (unit_big_int, [zero_big_int; two_big_int], ref None)
  let sqrt3 = (unit_big_int, [zero_big_int; two_big_int; zero_big_int], ref None)
  let sqrt5 = (unit_big_int, [minus_big_int unit_big_int; zero_big_int; dbl_big_int two_big_int; zero_big_int; zero_big_int], ref None)

  let quick_zero (_, p, _) = List.for_all (eq_big_int zero_big_int) p

  let quick_is_rat c =
    match c with
    | (_, [p], _) -> true
    | _ -> false

  let quick_get_rat c =
    match c with
    | (d, [p], _) -> p, d
    | _ -> raise (UnsupportedFunction "quick_get_rat: not rat")

  let ulp x =
    (* Only a rough approximation *)
    if x > 1.0e-305 || x < -1.0e-305
    then x *. 2.5e-16
    else 5.0e-324

  let to_interval (d, p, cache) =
    match !cache with
    | Some x -> x
    | None ->
       (* WARNING: optimized for speed *)
       let x = 2.0 *. (atan 1.0) /. (float_of_int (List.length p)) in
       let num, biggest, len =
         List.fold_left (fun (s, m, n) a ->
             let term = float_of_big_int a *. Pervasives.cos (n *. x) in
             (s +. term,
              Pervasives.max m (abs_float term),
              1.0 +. n)) (0.0, 0.0, 0.0) p in
       let d' = float_of_big_int d in
       let out = num /. d', ulp biggest *. len *. len *. 5.0 /. d' in
       cache := Some out;
       out

  let to_float p = fst (to_interval p)

  let leading_term (d, p, _) =
    let n = List.length p in
    let rec f p n =
      match p with
      | x :: p' ->
         if eq_big_int x zero_big_int
         then f p' (n - 1)
         else n-1, x, d
      | [] ->
         -1, zero_big_int, unit_big_int
    in
    f (List.rev p) n

  let len (d, p, _) = List.length p

  let deg f = let d, _, _ = leading_term f in d

  let to_string (d, p, c) =
    let n = big_int_of_int (List.length p) in
    let print_frac a b : string =
      let g = gcd_big_int a b in
      let a' = div_big_int a g in
      let b' = div_big_int b g in
      if eq_big_int b' unit_big_int
      then string_of_big_int a'
      else Printf.sprintf "%s/%s" (string_of_big_int a') (string_of_big_int b')
    in
    let rec print_term l i already : string option list =
      match l with
      | [] -> if already then [] else [Some "0"]
      | x :: l' ->
         let sign = (if lt_big_int x zero_big_int
                     then Some " - "
                     else if already then Some " + " else None) in
         let already' = already || not (eq_big_int x zero_big_int) in
         let coeff = if eq_big_int (abs_big_int x) d
                     then if i = 0 then Some "1" else None
                     else Some (String.concat "" [print_frac (abs_big_int x) d; " "]) in
         let term = if i = 0 then None else Some (String.concat "" [ "cos("; Printf.sprintf "PI*%s" (print_frac (big_int_of_int i) (dbl_big_int n)) ;")"]) in
         let mult = match coeff, term with
           | Some _, Some _ -> Some " * "
           | _ , _ -> None in
         let rest = print_term l' (i + 1) already' in
         if eq_big_int x zero_big_int
         then rest
         else sign :: coeff :: mult :: term :: rest
    in
    if not !display_exact
    then Printf.sprintf "%g" (to_float (d, p, c))
    else
      [[if !display_float then Some (Printf.sprintf "%g" (to_float (d, p, c))) else None];
       [Some "[|"];
       print_term p 0 false;
       [Some "|]"];
       if !display_field then [Some " in Q_"; Some (string_of_big_int n)] else []] |>
        List.concat |> concatMaybe |> String.concat ""

  (* O(n) *)
  let basis i n =
    let i = (abs i) mod (4 * n) in
    let i = if i < 2*n then i else 4*n - i in
    let coeff = if i < n then unit_big_int else if i = n then zero_big_int else minus_big_int unit_big_int in
    let i = if i < n then i else 2*n - i in
    (unit_big_int, init n (fun j -> if i = j then coeff else zero_big_int), ref None)

  (* O(n) *)
  let rebase (d, p, cache) n : t =
    let mult = n / (List.length p) in
    let rec loop = function
      | [], 0 -> []
      | p :: l, 0 -> p :: loop (l, mult - 1)
      | l, n -> zero_big_int :: loop (l, n - 1)
    in
    (d, loop (p, 0), cache)

  (* O(n) *)
  let fix_basis (d, p, cache) : t =
    let n = List.length p in
    let indices = List.mapi (fun i a -> big_int_of_int i) p in
    let g = int_of_big_int (List.fold_left2 (fun (g : big_int) c i -> if eq_big_int c zero_big_int then g else gcd_big_int g i) (big_int_of_int n) p indices) in
    if g = 0
    then (d, [ List.hd p ], cache)
    else (d, every_nth p g, cache)

  (* O(lcm(p1, p2)) *)
  let same_field (d1, p1, cache1) (d2, p2, cache2) =
    let n1 = List.length p1 in
    let n2 = List.length p2 in
    if n1 = n2
    then (d1, p1, cache1), (d2, p2, cache2)
    else
      let n1' = big_int_of_int n1 in
      let n2' = big_int_of_int n2 in
      let lcm = int_of_big_int (mult_big_int n1' (div_big_int n2' (gcd_big_int n1' n2'))) in
      (rebase (d1, p1, cache1) lcm, rebase (d2, p2, cache2) lcm)

  (* O(p1 + p2) *)
  let same_denom (d1, p, _) (d2, q, _) =
    assert (not (eq_big_int d1 zero_big_int));
    assert (not (eq_big_int d2 zero_big_int));
    if eq_big_int d1 d2
    then d1, p, q
    else
      let g = gcd_big_int d1 d2 in
      let g1 = div_big_int d1 g in
      let g2 = div_big_int d2 g in
      (mult_big_int d1 g2, List.map (fun x -> mult_big_int x g2) p, List.map (fun x -> mult_big_int x g1) q)

  (* O(p) *)
  let fix_denom (d, p, cache) : t =
    assert (not (eq_big_int d zero_big_int));
    if eq_big_int d unit_big_int
    then (d, p, cache)
    else
      let g = List.fold_left (fun g x -> gcd_big_int g x) d p in
      let g = if lt_big_int d zero_big_int && gt_big_int g zero_big_int then minus_big_int g else g in
      (div_big_int d g, List.map (fun x -> div_big_int x g) p, cache)

  (* O(lcm(p, q)) *)
  let add p q : t =
    let (p, q) = same_field p q in
    let (d, p, q) = same_denom p q in
    (d, List.map2 (fun a b -> add_big_int a b) p q, ref None) |> fix_denom

  (* O(p) *)
  let neg (d, p, _) : t =
    (d, List.map minus_big_int p, ref None)

  (* O(lcm(p, q)) *)
  let sub p q : t =
    let (p, q) = same_field p q in
    let (d, p, q) = same_denom p q in
    (d, List.map2 (fun a b -> sub_big_int a b) p q, ref None) |> fix_denom

  let mul' (d1, p, _) (d2, q, _) =
    let n = List.length p in
    let a = Array.make n zero_big_int in
    List.iteri (fun i cp ->
        if eq_big_int cp zero_big_int
        then ()
        else
          List.iteri (fun j cq ->
              let delta = mult_big_int cp cq in
              let head = i + j in
              let tail = abs (i - j) in
              a.(tail) <- (add_big_int (a.(tail)) delta);
              if head < n
              then a.(head) <- add_big_int a.(head) delta
              else if head = n
              then ()
              else a.(2*n - head) <- sub_big_int a.(2*n - head) delta)
            q)
      p;
    (dbl_big_int (mult_big_int d1 d2), Array.to_list a, ref None) |> fix_denom

  (* O(p q) *)
  let mul p q : t =
    let (d1, p, c1), (d2, q, c2) = same_field p q in
    mul' (d1, p, c1) (d2, q, c2)

  (* TODO: this can be done without explicitly building the basis or product *)
  let mul_basis (d, p, c) i : t =
    let n = List.length p in
    mul' (d, p, c) (basis i n)

  (* O(p) *)
  let scale (d, p, _) a b : t =
    assert (not (eq_big_int b zero_big_int));
    if eq_big_int a zero_big_int
    then zero
    else fix_denom (mult_big_int d b, List.map (mult_big_int a) p, ref None)

  (* If this returns LT or GT, it is correct. If it returns EQ, who knows? *)
  (* O((p + q)) but really slow *)
  let fast_order p q =
    let (p_c, p_d), (q_c, q_d) = to_interval p, to_interval q in
    if p_c -. p_d > q_c +. q_d
    then GT
    else if q_c -. q_d > p_c +. p_d
    then LT
    else EQ

  (* O((p + q)) *)
  let medium_equal (d1, p1, _) (d2, p2, _) =
    List.length p1 = List.length p2 && eq_big_int d1 d2 && List.for_all2 eq_big_int p1 p2

  (* O((p + q) W(p + q)) once fix_basis is made fast *)
  let slow_equal p q =
    let p, q = fix_basis p, fix_basis q in
    let (d, pq, c) = sub p q |> fix_basis in
    if quick_zero (d, pq, c)
    then true
    else List.exists (fun d -> quick_zero (mul_basis (unit_big_int, pq, ref None) d)) (maximal_odd_divisors (List.length pq))

  let equal p q = medium_equal p q || (fast_order p q = EQ && slow_equal p q)
  let equiv = equal

  let simple_div f g b =
    assert (if b then equal f zero else true);
    let f, g = same_field f g in
    let n = len f in
    let deg_g, lc_g, den_g = leading_term g in
    let rec div q r i =
      let r, _ = same_field r g in
      assert (equal f (add (mul q g) r));
      assert ((len r) = (len g));
      let deg_r, lc_r, den_r = leading_term r in
      if deg_r < deg_g
      then q, r
      else
        let k = deg_r - deg_g in
        let num, den = (mult_big_int lc_r den_g), (mult_big_int lc_g den_r) in
        assert (not (eq_big_int den zero_big_int));
        let num = if deg_r = deg_g || deg_g = 0 then num else (dbl_big_int num) in
        let q' = scale (basis k n) num den in
        let r_delta = scale (mul_basis g k) num den in
        let r' = sub r r_delta in
        assert (deg r' < deg_r);
        div (add q q') r' (i + 1)
    in
    let div_first () =
      let i = n - deg_g in
      assert (not (eq_big_int lc_g zero_big_int));
      div (scale (basis i n) unit_big_int lc_g) (neg (scale (mul (basis i n) g) unit_big_int lc_g)) 0
    in
    let q, r = if b then div_first () else div zero f 0 in
    assert (equal f (add (mul q g) r));
    assert (deg r < deg_g);
    q, r

  let extended_gcd f g b =
    assert (if b then equal f zero else true);
    let f, g = same_field f g in
    let n = len f in
    (* Assume g != 0 *)
    let rec loop rm1 um1 vm1 r u v i =
      assert (equal r (add (mul u f) (mul v g)));
      if i > n then Printf.printf "Looped %d times! GCD %s and %s\n" i (if b then "T_n" else (to_string f)) (to_string g);
      let deg_r, lc_r, den_r = leading_term r in
      if deg_r = -1
      then rm1, um1, vm1
      else
        let q, r' = simple_div rm1 r false in
        let u', v' = sub um1 (mul q u), sub vm1 (mul q v) in
        assert (deg r' < deg_r);
        loop r u v r' u' v' (i + 1)
    in
    let loop_first () =
      let q, r' = simple_div zero g true in
      loop g zero one r' one (neg q) 0
    in
    let r, u, v = if b then loop_first () else loop f one zero g zero one 0 in
    assert (equal r (add (mul u f) (mul v g)));
    r, u, v

  let inv q =
    let r, a, b = extended_gcd zero q true in
    let deg, lc, den = leading_term r in
    if deg = 0
    then scale b den lc
    else let newmod, _zero = simple_div zero r true in
         let r, a, b = extended_gcd newmod q false in
         let deg, lc, den = leading_term r in
         if (deg <> 0) then Printf.printf "Could not invert q = %s; got remainder %s\n%!" (to_string q) (to_string r);
         assert (deg = 0); (* TODO: Failing assertion! *)
         scale b den lc

  let div p q =
    if equal q zero
    then failwith "Chebynomial: division by zero"
    else match same_field p q with
         | (d1, [p], _), (d2, [q], _) ->
            fix_denom (mult_big_int d1 q, [mult_big_int d2 p], ref None)
         | (d1, [a1; b1], _), (d2, [a2; b2], _) ->
            (* Hardcoded for speed *)
            let a' = sub_big_int (dbl_big_int (mult_big_int a1 a2)) (mult_big_int b1 b2) in
            let b' = dbl_big_int (sub_big_int (mult_big_int a2 b1) (mult_big_int a1 b2)) in
            let d' = sub_big_int (dbl_big_int (mult_big_int a2 a2)) (mult_big_int b2 b2) in
            let d2, d' =
              if lt_big_int d' zero_big_int
              then minus_big_int d2, minus_big_int d'
              else d2, d'
            in
            fix_denom (mult_big_int d1 d', [mult_big_int a' d2; mult_big_int b' d2], ref None)
         | _, _ -> mul p (inv q)

  (* Where p = a / b, O(b) *)
  let cos (d, p, _) : t =
    match p with
    | [c] ->
       let c2, d2 = reduce_frac (dbl_big_int c) d in
       basis (int_of_big_int c2) (int_of_big_int d2)
    | _ -> raise (UnsupportedFunction "cos of non-rationals unsupported")
  let sin x : t = cos (sub (div pi (of_int 2)) x)
  let tan x : t = div (sin x) (cos x)

  let pi_float = 3.14159265358979323846264338327950288419716939937510582097494459233
  let cos_float a b =
    Pervasives.cos (pi_float *. (float_of_int a) /. (2.0 *. (float_of_int b)))

  external z_of_mpz: Mpz.t -> Z.t = "ml_z_mlgmpidl_of_mpz"

  (* TODO: improve; upper and lower bound for n cos(pi a / 2 b); currently uses shady float *)
  let min_cos a b n =
    if n > 1000
    then raise (UnsupportedFunction "Values too close together, ordering currently unsupported")
    else ();

    let prec = n + 15 in

    let x = Mpfr.init2 prec in
    let _ = Mpfr.const_pi x Mpfr.Near in

    let tmp = Mpfr.init2 prec in

    let _ = Mpfr.set_si tmp a Mpfr.Near in
    let _ = Mpfr.mul x x tmp Mpfr.Near in

    let _ = Mpfr.set_si tmp b Mpfr.Near in
    let _ = Mpfr.div x x tmp Mpfr.Near in

    let _ = Mpfr.div_ui x x 2 Mpfr.Near in
    let _ = Mpfr.cos x x Mpfr.Near in

    let _ = Mpfr.mul_2si x x n Mpfr.Near in

    let i = Mpz.init () in
    let _ = Mpfr.get_z i x Mpfr.Down in
    z_of_mpz i


  let max_cos a b n =
    add_big_int (min_cos a b n) unit_big_int

  let rec positive (d, p, _) =
    let n = List.length p in
    let positive' b =
      let min_term c i =
        mult_big_int c ((if gt_big_int c zero_big_int then min_cos else max_cos) i n b)
      in
      let max_term c i =
        mult_big_int c ((if gt_big_int c zero_big_int then max_cos else min_cos) i n b)
      in
      let accum term = (fun a c i -> add_big_int a (term c i)) in
      let indices = List.tl (List.mapi (fun i a -> i) p) in
      let start = (mult_big_int (power_int_positive_int 2 b) (List.hd p)) in
      let tot_min = List.fold_left2 (accum min_term) start (List.tl p) indices in
      if gt_big_int tot_min zero_big_int
      then Some true
      else let tot_max = List.fold_left2 (accum max_term) start (List.tl p) indices in
           if le_big_int tot_max zero_big_int then Some false else None
    in
    let rec positive_rec b =
      match positive' b with
      | Some res -> res
      | None -> positive_rec (10 + b)
    in
    positive_rec 10

  let rec to_big_int (d, p, _) =
    let n = List.length p in
    let to_int' b =
      let min_term c i =
        mult_big_int c ((if gt_big_int c zero_big_int then min_cos else max_cos) i n b)
      in
      let max_term c i =
        mult_big_int c ((if gt_big_int c zero_big_int then max_cos else min_cos) i n b)
      in
      let accum term = (fun a c i -> add_big_int a (term c i)) in
      let indices = List.tl (List.mapi (fun i a -> i) p) in
      let start = (mult_big_int (power_int_positive_int 2 b) (List.hd p)) in
      let tot_min = List.fold_left2 (accum min_term) start (List.tl p) indices in
      let tot_max = List.fold_left2 (accum max_term) start (List.tl p) indices in
      let den = mult_big_int d (power_int_positive_int 2 b) in
      if eq_big_int (div_big_int tot_min den) (div_big_int tot_max den)
      then Some (div_big_int tot_min den)
      else None
    in
    let rec loop b =
      match to_int' b with
      | Some res -> res
      | None -> loop (1 + b)
    in
    loop 0

  (* O(n) *)
  let order x y =
    match fast_order x y with
    | LT -> LT
    | GT -> GT
    | EQ ->
       if equal x y
       then EQ
       else if positive (sub x y) then GT else LT

  (* O(n) *)
  let abs x =
    match order x zero with
    | LT -> neg x
    | EQ -> zero
    | GT -> x

  let rem a b =
    let q = div a b in
    let q' = to_big_int q in
    sub a (scale b q' unit_big_int)

  let sqrt x = Printf.printf "Taking square root of %s\n%!" (to_string x); raise (UnsupportedFunction "sqrt")
  let atan2 x y = raise (UnsupportedFunction "atan2")
  let acos x y = raise (UnsupportedFunction "acos")
  let asin x y = raise (UnsupportedFunction "asin")

  let get_oprec () = 0
  let set_oprec _ = ()

  let forget x = x
  let get_stats () = 3
  let print_table () = ()

end

module ExHackedArith = struct
  type t = Exact of Chebynomial.t | Pi of Chebynomial.t | Atan2 of t * t | Sqrt of t | Backup of float

  let eps_abs = ref 0.0
  let eps_rel = ref 0.0


  let check c =
    if Chebynomial.equal c Chebynomial.zero then
      Chebynomial.zero
    else begin
        if Pervasives.abs_float (Chebynomial.to_float c) < !eps_abs then
          Printf.printf "Boy, that's a small number! %s\n" (Chebynomial.to_string c);
        c
      end

  let rec to_float = function
    | Exact x -> Chebynomial.to_float x
    | Pi x -> Chebynomial.to_float x *. (4.0 *. atan 1.0)
    | Sqrt t -> Pervasives.sqrt (to_float t)
    | Atan2 (y, x) -> Pervasives.atan2 (to_float y) (to_float x)
    | Backup y -> y

  let rec to_string = function
    | Exact x -> Chebynomial.to_string x
    (*    | Pi (d, [p]) -> Printf.sprintf "%s/%s pi" (string_of_big_int p) (string_of_big_int d) *)
    | Pi x -> String.concat "" ["pi "; Chebynomial.to_string x]
    | Backup y -> Printf.sprintf "%f" y
    | Atan2 (y, x) -> String.concat "" ["atan2 [ "; to_string y; " ; "; to_string x; "]"]
    | Sqrt y -> String.concat "" ["sqrt ["; to_string y; "]"]

  let split2 s f g h = fun x y ->
    match x, y with
    | Exact x, Exact y -> Exact (f x y |> check)
    | Pi x, Pi y -> Pi (g x y |> check)
    | Backup _, _ | _, Backup _ -> Backup (h (to_float x) (to_float y))
    | _, _ ->
       Printf.printf "Unsupported function %s on %s and %s!\n" s (to_string x) (to_string y);
       Backup (h (to_float x) (to_float y))

  let of_string x =
    let a, b = rat_of_string x in
    if gt_big_int b (power_int_positive_int 10 3) && gt_big_int (abs_big_int a) (power_int_positive_int 10 3)
    then begin
        Printf.printf "Value `%s` too exact to be rational!\n" x;
        Backup (float_of_string x)
      end
    else Exact (Chebynomial.of_string x |> check)
  let of_int n = Exact (Chebynomial.of_int n |> check)
  let of_big_int n = Exact (Chebynomial.of_big_int n |> check)
  let of_rat a b = Exact (Chebynomial.of_rat a b |> check)
  let of_float x = of_string (string_of_float x)

  let n0 = of_big_int zero_big_int
  let n1 = of_big_int unit_big_int
  let n2 = of_int 2
  let n3 = of_int 3
  let n4 = of_int 4
  let n5 = of_int 5
  let n6 = of_int 6
  let n180 = of_int 180
  let n360 = of_int 360
  let pi = Pi (Chebynomial.one)

  (* TODO ?!?! *)
  let twoPi = Pi (Chebynomial.one)

  let rec neg = function
    | Exact x -> Exact (Chebynomial.neg x |> check)
    | Pi x -> Pi (Chebynomial.neg x |> check)
    | Sqrt x ->
       Printf.printf "Unsupported function neg %s!\n" (to_string (Sqrt x));
       Backup (~-. (to_float (Sqrt x)))
    | Atan2 (y, x) -> Atan2 (neg y, x)
    | Backup y -> Backup (~-. y)

  let add = fun x y ->
    match x, y with
    | Exact c, o when Chebynomial.quick_zero c -> o
    | o, Exact c when Chebynomial.quick_zero c -> o
    | Pi c, o when Chebynomial.quick_zero c -> o
    | o, Pi c when Chebynomial.quick_zero c -> o
    | _, _ -> split2 "add" Chebynomial.add Chebynomial.add ( +. ) x y
  let sub = fun x y ->
    match x, y with
    | Exact c, o when Chebynomial.quick_zero c -> neg o
    | o, Exact c when Chebynomial.quick_zero c -> o
    | Pi c, o when Chebynomial.quick_zero c -> neg o
    | o, Pi c when Chebynomial.quick_zero c -> o
    | Pi c, Atan2 (b, a) when Chebynomial.equal c (Chebynomial.of_string "1/2") ->
       Atan2 (a, b)
    | _, _ -> split2 "sub" Chebynomial.sub Chebynomial.sub ( -. ) x y
  let rec mul x y =
    match x, y with
      (* First two here are special cases *)
(*
    | Exact (d1, [p1]), Exact q ->
       Exact (Chebynomial.scale q p1 d1)
    | Exact q, Exact (d1, [p1]) ->
       Exact (Chebynomial.scale q p1 d1)
 *)
    | Exact p, Exact q ->
       Exact (Chebynomial.mul p q |> check)

      (* First four here are special cases *)
(*
    | Pi (d1, [p1]), Exact q ->
       Pi (Chebynomial.scale q p1 d1)
    | Pi q, Exact (d1, [p1]) ->
       Pi (Chebynomial.scale q p1 d1)
    | Exact q, Pi (d1, [p1]) ->
       Pi (Chebynomial.scale q p1 d1)
    | Exact (d1, [p1]), Pi q ->
       Pi (Chebynomial.scale q p1 d1)
 *)
    | Pi p, Exact q ->
       Pi (Chebynomial.mul p q |> check)
    | Exact p, Pi q ->
       Pi (Chebynomial.mul p q |> check)

    | Sqrt x', Sqrt y' ->
       if x' = y' then x' else Sqrt (mul x' y')
    | Sqrt x', _ ->
       Sqrt (mul x' (mul y y))
    | _, Sqrt y' ->
       Sqrt (mul (mul x x) y')

    | Backup _, _ | _, Backup _ -> Backup (to_float x *. to_float y)
    | _, _ ->
       Printf.printf "Unsupported function mul %s & %s!\n" (to_string x) (to_string y);
       Backup (to_float x *. to_float y)

  let rec div x y =
    match x, y with
    (* special case *)
(*
    | Exact q, Exact (d1, [p1]) when not (eq_big_int p1 zero_big_int)->
       Exact (Chebynomial.scale q d1 p1)
 *)
    | Exact p, Exact q ->
       Exact (Chebynomial.div p q |> check)

    | Pi q, Exact c  ->
       Pi (Chebynomial.div q c |> check)

(*
    | Pi q, Pi (d1, [p1]) when not (eq_big_int p1 zero_big_int) ->
       Exact (Chebynomial.scale q d1 p1)
 *)
    | Pi p, Pi q ->
       Exact (Chebynomial.div p q |> check)
    | Sqrt x', Sqrt y' ->
       Sqrt (div x' y')
    | Sqrt x', _ ->
       Sqrt (div x' (mul y y))
    | _, Sqrt y' ->
       Sqrt (div (mul x x) y')
    | Backup _, _ | _, Backup _ -> Backup (to_float x /. to_float y)
    | _, _ ->
       Printf.printf "Unsupported function div %s & %s!\n" (to_string x) (to_string y);
       Backup (to_float x /. to_float y)
  let rem = split2 "rem" Chebynomial.rem Chebynomial.rem ( Pervasives.mod_float )

  let sqrt = function
    | Exact x when Chebynomial.quick_is_rat x ->
       let p, d = Chebynomial.quick_get_rat x in
       if lt_big_int p zero_big_int
       then Sqrt (Exact x)
       else begin
           let dp = mult_big_int d p in
           (* TODO: Move to Chebynomial; also, is it correct? *)
           if eq_big_int dp (big_int_of_int 0)
           then n0
           else if eq_big_int dp (big_int_of_int 1)
           then n1
           else if eq_big_int dp (big_int_of_int 2)
           then Exact Chebynomial.sqrt2
           else if eq_big_int dp (big_int_of_int 3)
           then Exact Chebynomial.sqrt3
           else if eq_big_int dp (big_int_of_int 5)
           then Exact Chebynomial.sqrt5
           else
             let dp' = sqrt_big_int dp in
             if eq_big_int (square_big_int dp') dp
             then of_rat dp' d
             else Sqrt (Exact x)
         end
    | Backup x -> Backup (Pervasives.sqrt x)
    | x -> Sqrt x

  let cos x =
    match x with
    | Pi c when Chebynomial.quick_is_rat c ->
       Exact (Chebynomial.cos c |> check)
    | Atan2 (b, a) ->
       div a (sqrt (add (mul a a) (mul b b)))
    | Backup _ -> Backup (Pervasives.cos (to_float x))
    | _ ->
       Printf.printf "Unsupported function cos %s!\n" (to_string x);
       Backup (Pervasives.cos (to_float x))
  let sin x = cos (sub (div pi (of_int 2)) x)
  let tan x = div (sin x) (cos x)
  let atan2 y x = Atan2 (y, x)
  let acos z = Atan2 (sqrt (sub n1 (mul z z)), z)
  let asin z = failwith "TODO"

  let rec equiv x y =
    match x, y with
    | Exact x', Exact y' ->
       Chebynomial.equal x' y'
    | Exact x', Pi y' | Pi x', Exact y' ->
       Chebynomial.equal x' Chebynomial.zero &&
       Chebynomial.equal y' Chebynomial.zero
    | Pi x', Pi y' ->
       Chebynomial.equal x' y'
    | Sqrt x, Sqrt y ->
       equiv x y
    | Sqrt x, y when equiv y n0 ->
       equiv x n0
    | x, Sqrt y when equiv x n0 ->
       equiv y n0
    | Exact x', Sqrt y' when not (Chebynomial.positive (Chebynomial.neg x')) ->
       equiv (mul x x) y'
    | Backup _, _ | _, Backup _ ->
       let xf, yf = (to_float x), (to_float y) in
       Pervasives.abs_float (xf -. yf) <= !eps_abs ||
         Pervasives.abs_float (xf -. yf) <= !eps_rel *. (Pervasives.max (Pervasives.abs_float xf) (Pervasives.abs_float yf))
    | _, _ ->
       Printf.printf "Unsupported function equiv %s & %s!\n" (to_string x) (to_string y);
       equiv (Backup (to_float x)) (Backup (to_float y))
  let rec equivs = function
    | n1 :: n2 :: ns ->
        equiv n1 n2 && equivs (n2 :: ns)
    | _ -> true

  let rec order x y =
    if equiv x y
    then EQ
    else
      match x, y with
      | Exact x', Exact y' | Pi x', Pi y' ->
         if Chebynomial.positive (Chebynomial.sub x' y') then GT else LT
      | Sqrt x', Sqrt y' ->
         order x' y'
      | Sqrt x, y when equiv y n0 ->
         order x n0
      | y, Sqrt x when equiv y n0 ->
         order n0 x
      | Backup _, _ | _, Backup _ ->
         let x', y' = to_float x, to_float y in
         if x' < y' then LT else GT
      | _, _ ->
         Printf.printf "Unsupported function order %s & %s!\n" (to_string x) (to_string y);
         let x', y' = to_float x, to_float y in
         if x' < y' then LT else GT

  let cmp = order
  let sum = List.fold_left add n0

  let rand () =
    let radius = 1.0 in
    let x = Random.float (2.0 *. radius) -. radius in
    of_string (Printf.sprintf "%0.1f" x)

  let abs x =
    match order x n0 with
    | LT -> neg x
    | EQ -> n0
    | GT -> x

  let rad_of_deg deg =
    div (mul deg pi) n180

  let deg_of_rad rad =
    div (mul rad n180) pi

  let set_eps_abs e = eps_abs := (to_float e)
  let set_eps_rel e = eps_rel := (to_float e)
  let get_eps_rel _ = of_float !eps_rel
  let get_eps_abs _ = of_float !eps_abs

  let get_oprec () = 0
  let set_oprec _ = ()

  let forget x = x
  let get_stats () = 3
  let print_table () = ()

  let with_oprec _ f x = f x

  (* Note: Overridden later *)
  let of_const _ = n0
end

                        (*
let main () =
  Printexc.record_backtrace true;
  Sys.catch_break true;
  let open Printf in
  let open Chebynomial in
  let p = of_string "1/2" in
  let q = cos (of_string "1/3") in
  printf "%s vs %s: %s\n" (to_string p) (to_string q) (if equal p q then "=" else "!=");
  let p = cos (of_string "1/6") in
  printf "%s positive? %s\n" (to_string (sub p q)) (if positive (sub p q) then "yes" else "no");
  let p = cos (of_string "1/4") in
  let s = mul (of_string "1/2") (sub (sub (sub one (cos (of_string "1/12"))) (cos (of_string "1/4"))) (cos (of_string "1/3"))) in
  printf "%s * %s = %s\n" (to_string p) (to_string s) (to_string (mul p s));
  let quot, r = simple_div p p false in
  printf "%s = %s * %s + %s\n" (to_string p) (to_string quot) (to_string p) (to_string r);
  assert (equal r zero);
  let quot, r = simple_div p one false in
  printf "%s = %s * 1 + %s\n" (to_string p) (to_string quot) (to_string r);
  let quot, r = simple_div p (neg one) false in
  printf "%s = %s * -1 + %s\n" (to_string p) (to_string quot) (to_string r);
  assert (equal r zero);
  let quot, r = simple_div zero p true in
  printf "0 = %s * %s + %s\n" (to_string quot) (to_string p) (to_string r);
  let quot, r = simple_div zero p false in
  printf "0 = %s * %s + %s\n" (to_string quot) (to_string p) (to_string r);
  let quot, r = simple_div q p false in
  printf "%s = %s * %s + %s\n" (to_string q) (to_string quot) (to_string p) (to_string r);
  let r, a, b = extended_gcd q p false in
  printf "%s = %s * %s + %s * %s\n" (to_string r) (to_string a) (to_string q) (to_string b) (to_string p);
  let r, a, b = extended_gcd zero p true in
  printf "%s = %s * %s\n" (to_string r) (to_string b) (to_string p);
  let quot, r = simple_div (neg (mul (of_string "1/4") (cos (of_string "1/6")))) (neg one) false in
  assert (equal r zero);
  let out = div p q in
  printf "%s / %s = %s\n%!" (to_string p) (to_string q) (to_string out);
  assert (equal out (mul p (of_int 2)));
  let p = mul (of_int 2) (sub (add (sub (cos (of_string "1/18")) (cos (of_string "1/6")))
                             (cos (of_string "5/18"))) (cos (of_string "7/18"))) in
  let out = inv p in
  printf "1 / %s = %s\n%!" (to_string p) (to_string out);
  let q = sub (neg (of_string "1/2")) (mul (of_string "3") (sub (cos (of_string "353/360")) (cos (of_string "367/360")))) in
  let out = inv q in
  printf "1 / %s = %s\n" (to_string q) (to_string out);
  ()

let _ = main ()
          *)
