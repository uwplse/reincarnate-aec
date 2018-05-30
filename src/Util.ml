open Logging

(* register exception printers *)
let () =
  Printexc.register_printer (function
    | Failure s -> Some ("Failure:\n" ^ s)
    | _ -> None)

(** {2 Combinators} *)

(** Ignores the result of [f x] and returns [x] *)
let (>>) x f = ignore (f x); x

let (<|) f g x = f (g x)

(** [flip f a b] swaps the order of the arguments to [f] *)
let flip f a b = f b a

(** [uncurry f (a, b)] returns a function [f] taking arguments [a], [b] *)
let uncurry f (a, b) = f a b

(** [uncurry3 f (a, b, c)] returns a function [f] taking arguments [a], [b], [c] *)
let uncurry3 f (a, b, c) = f a b c

(** [notp f x] takes a predicate [f] and an argument [x] and negates [f x] *)
let notp f x = not (f x)

let cons x xs =
  x :: xs

let pair a b =
  (a, b)

let some x =
  Some x

let valOf = function
  | Some x -> x
  | None   -> failwith "Util.valOf: None"

let obind f = function
  | Some x -> f x
  | None   -> None

(** {2 List Auxiliaries} *)

let flatmap f l =
  l |> List.map f
    |> List.flatten

(** [rotateL] moves first element to end *)
let rotateL = function
  | [] -> []
  | x :: xs -> xs @ [x]

(** [rotateR] moves last element to beginning *)
let rotateR l =
  l |> List.rev
    |> rotateL
    |> List.rev

let rec pair_chain = function
  | a :: b :: cs ->
      (a, b) :: pair_chain (b :: cs)
  | _ -> []

let rec triples lm ln lp =
  match lm, ln, lp with
  | [], [], [] ->
      []
  | m :: ms, n :: ns, p :: ps ->
      (m, n, p) :: triples ms ns ps
  | _, _, _ ->
      failwith "Util.triples: bogus lengths"

let split n l =
  let rec loop acc n l =
    if n <= 0 then
      (List.rev acc, l)
    else
      match l with
      | [] -> failwith "Util.split: empty"
      | x :: xs -> loop (x :: acc) (n - 1) xs
  in
  loop [] n l

let rec choose2 l =
  match l with
  | [] -> []
  | f :: fs ->
      (List.map (fun f' -> (f, f')) fs) @
      choose2 fs

let xprod xs ys =
  let aux acc x =
    ys |> List.map (fun y -> (x, y))
       |> flip cons acc
  in
  xs |> List.fold_left aux []
     |> List.flatten

let rec transpose = function
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss)
      :: transpose (xs :: List.map List.tl xss)

(** [range lo n] returns a list of all values from [lo] to [hi - 1] *)
let range lo hi =
  let rec loop acc i =
    if i < lo  then
      acc
    else
      loop (i :: acc) (i - 1)
  in
  loop [] (hi - 1)

(** [repeat x n] returns a list of [n] repeated values of [x] *)
let repeat x n =
  let rec loop acc i =
    if i = 0 then
      acc
    else
      loop (x :: acc) (i - 1)
  in
  loop [] n

(** {2 Formatting} *)

let string_of_opt fmt optT =
  match optT with
  | None   -> "None"
  | Some t -> Printf.sprintf "Some (%s)" (fmt t)

let string_of_pair (s1, s2) =
  Printf.sprintf "(%s, %s)" s1 s2

let string_of_triple (s1, s2, s3) =
  Printf.sprintf "(%s, %s, %s)" s1 s2 s3

let indent ?spacer:(spacer = " ") n s =
  n |> repeat spacer
    |> String.concat ""
    |> flip (^) s

let indents ?spacer:(spacer = " ") n s =
  s |> String.split_on_char '\n'
    |> List.map (indent ~spacer:spacer n)
    |> String.concat "\n"

(** {2 Logging} *)

module type LOGTAG = sig
  val tag : string
end

module MakeTaggedLogging (T : LOGTAG) : sig
  val log : string -> unit
  val logd : string -> int -> string -> unit
end = struct
  let log msg =
    Logging.log (Printf.sprintf "%6s | %s" T.tag msg)

  let logd ftag depth msg =
    log (Printf.sprintf "%s) %s"
      ftag
      (indent (depth * 2) msg))
end

include MakeTaggedLogging(struct let tag = "Util" end)

(** {2 Generic Comparison} *)

type cmp = LT | EQ | GT

let string_of_cmp = function
  | LT -> "LT"
  | EQ -> "EQ"
  | GT -> "GT"

(** returns [LT], [EQ] or [GT] depending
  on whether a <, = or > b *)
let cmp_of_cmpi cmpi a b =
  match cmpi a b with
  | -1 -> LT
  |  0 -> EQ
  |  1 -> GT
  |  _ -> failwith "cmp_of_cmpi: bogus compare"

(** returns -1, 0 or 1 depending on whether a <, = or > b *)
let cmpi_of_cmp cmp a b =
  match cmp a b with
  | LT -> -1
  | EQ ->  0
  | GT ->  1

(** compares a pair *)
(* NOTE RESPECT EQUIV! *)
let cmp_pairs equiv cmp (x1, y1) (x2, y2) =
  if equiv x1 x2 then
    if equiv y1 y2 then
      EQ
    else
      cmp y1 y2
  else
    cmp x1 x2

(** compares a triple *)
(* NOTE RESPECT EQUIV! *)
let cmp_triples equiv cmp (x1, y1, z1) (x2, y2, z2) =
  if equiv x1 x2 then
    if equiv y1 y2 then
      if equiv z1 z2 then
        EQ
      else
        cmp z1 z2
    else
      cmp y1 y2
  else
    cmp x1 x2

(** [sort cmp] sorts a list using comparator [cmp] *)
let sort cmp =
  List.sort (cmpi_of_cmp cmp)

(** [derep] removes repetitive elements from a sorted list *)
let rec derep cmp = function
  | []  -> []
  | [x] -> [x]
  | x1 :: x2 :: xs ->
      if cmp x1 x2 = EQ then
        derep cmp (x2 :: xs)
      else
        x1 :: derep cmp (x2 :: xs)

(** [dedup cmp l] sorts [l] using comparator [cmp] and
  removes duplicates by calling [derep] *)
let dedup cmp l =
  l |> sort cmp
    |> derep cmp

(** [min cmp a b] returns the smaller
  value between [a] and [b] *)
let min cmp a b =
  match cmp a b with
  | LT -> a
  | EQ -> a
  | GT -> b

(** [max cmp a b] returns the larger
  value between [a] and [b] *)
let max cmp a b =
  match cmp a b with
  | LT -> b
  | EQ -> b
  | GT -> a

(** returns smallest element of a list *)
let minl cmp = function
  |      [] -> failwith "minl empty"
  | x :: xs -> List.fold_left (min cmp) x xs

(** returns largest element of a list *)
let maxl cmp = function
  |      [] -> failwith "maxl empty"
  | x :: xs -> List.fold_left (max cmp) x xs

let extremei test l =
  let aux ((e, ei), i) x =
    if test e x
    then ((e, ei), i + 1)
    else ((x,  i), i + 1)
  in
  match l with
  | []      -> failwith "Util.extremei: empty"
  | x :: xs -> fst @@
                List.fold_left aux ((x, 0), 1) xs

(** returns smallest element of a list along with its index *)
let minli cmp l =
  extremei (fun a b -> cmp a b <> GT) l

(** returns largest element of a list along with its index *)
let maxli cmp l =
  extremei (fun a b -> cmp a b <> LT) l

(**
   [argmax cmp f l] returns the index [i] of the element of
   list [l] which maximizes function [f] based on comparison [cmp].

   TODO: d'oh this is incorrectly named / documented because it
         was taken out of its context in LinAlg.ml. Bad.
         Either put it back or fix the name / docs.
*)
let argmax cmp f l =
  fst @@ extremei (fun a b -> cmp (f a) (f b) <> LT) l

let rec extract cmp dir = function
  | []  -> failwith "extract empty"
  | [x] -> (x, [])
  | (x :: xs) ->
      let (y, ys) = extract cmp dir xs in
      if cmp x y = dir
      then (x, y :: ys)
      else (y, x :: ys)

(** extracts the smallest element from a list and returns
  a pair of that element and the rest of the list *)
let extract_min cmp =
  extract cmp LT

(** extracts the largest element from a list and returns
  a pair of that element and the rest of the list *)
let extract_max cmp =
  extract cmp GT

(** [between cmp a b c] checks whether [b] is
  between [a] and [c] where [a] and [c] are included *)
(* do not assume a <= c *)
let between cmp a b c =
  (cmp a b <> GT && cmp b c <> GT) ||
  (cmp c b <> GT && cmp b a <> GT)

(** [between cmp a b c] checks whether [b] is
  between [a] and [c] where [a] and [c] are excluded *)
(* do not assume a <= c *)
let xbetween cmp a b c =
  (cmp a b = LT && cmp b c = LT) ||
  (cmp c b = LT && cmp b a = LT)


(** {2 Simple File I/O} *)

let with_ic path f =
  let ic = Pervasives.open_in path in
  let res = f ic in
  Pervasives.close_in ic;
  res

let with_oc path f =
  let oc = Pervasives.open_out path in
  let res = f oc in
  Pervasives.close_out oc;
  res

let with_app path f =
  let app = Pervasives.open_out_gen
              [ Open_wronly
              ; Open_append
              ; Open_creat ] 0o666 path in
  let res = f app in
  Pervasives.close_out app;
  res

let with_ic_fd fd f =
  let ic = Unix.in_channel_of_descr fd in
  let res = f ic in
  Pervasives.close_in ic;
  res

let with_oc_fd fd f =
  let oc = Unix.out_channel_of_descr fd in
  let res = f oc in
  Pervasives.close_out oc;
  res

let of_file_lines nm =
  with_ic nm (fun ic ->
    let rec loop ls =
      let next =
        try Some (Pervasives.input_line ic)
        with End_of_file -> None
      in
      match next with
      | None   -> List.rev ls
      | Some l -> loop (l :: ls)
    in
    loop [])

let of_file nm =
  nm |> of_file_lines
     |> String.concat "\n"

let to_file nm s =
  with_oc nm (fun oc ->
    Pervasives.output_string oc s)

let append_file nm s =
  with_app nm (fun app ->
    Pervasives.output_string app s)

let mktmp nm ext =
  Filename.temp_file ("reincarnate-" ^ nm) ext

let try_rm p =
  try Sys.remove p
  with _ -> ()

(* removes tmp immediately after executing f *)
let with_tmp nm ext f =
  let tmp = mktmp nm ext in
  let res = f tmp in
  try_rm tmp;
  res

(* removes tmp when process exits *)
let with_tmp' nm ext f =
  let tmp = mktmp nm ext in
  Pervasives.at_exit (fun () -> try_rm tmp);
  f tmp

(** {2 Miscellaneous} *)

(** [pair_map f (a, b)] returns a pair with [f]
  applied to both [a] and [b] *)
let pair_map f (a, b) =
  (f a, f b)

(** [triple_map f (a, b, c)] returns a triple with [f]
  applied to [a], [b] and [c] *)
let triple_map f (a, b, c) =
  (f a, f b, f c)

(** [of_parity even odd n] returns [even] if [n]
  is even and [odd] otherwise *)
let of_parity even odd n =
  if n mod 2 = 0 then
    even
  else
    odd

let overlap l1 l2 =
  List.exists (flip List.mem l2) l1

let substring_at needle haystack i =
  let rec loop j =
    if j >= String.length needle then true
    else if i + j >= String.length haystack then false
    else
      String.get haystack (i + j) = String.get needle j &&
        loop (j + 1)
  in loop 0

let is_substring needle haystack =
  let rec loop i =
    if i >= String.length haystack then false
    else substring_at needle haystack i || loop (i + 1)
  in loop 0

let round n x =
  let n = 10.0 ** (float_of_int n) in
  floor (x *. n +. 0.5) /. n

(** {2 Random Seeds} *)

let __seed =
  Random.init 0;
  ref 0

let set_rand_seed s =
  Random.init s;
  __seed := s;
  log ("rand seed set to " ^ string_of_int s)

let get_rand_seed () =
  !__seed

let init_rand_seed () =
  () |> Random.State.make_self_init
     |> Random.State.bits
     |> set_rand_seed

(* everything depends on Util, so do some global config *)
let () = begin
  init_rand_seed ();
  Printexc.record_backtrace true;
end
