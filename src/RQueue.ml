module Queue : sig

  type 'a t

  val empty    : 'a t
  val single   : 'a -> 'a t
  val push     : 'a t -> 'a -> 'a t
  val pushl    : 'a t -> 'a list -> 'a t
  val is_empty : 'a t -> bool
  val pop      : 'a t -> ('a * 'a t) option
  val to_list  : 'a t -> 'a list
  val mem : 'a t -> 'a -> bool
  val push_uniq : 'a t -> 'a -> 'a t

end = struct

  type 'a t =
    'a list * 'a list

  let norm (f, b) =
    match f with
    | [] -> (List.rev b, [])
    | _  -> (f, b)

  let empty =
    ([], [])

  let is_empty (f, b) =
    match f with
    | [] -> true
    | _  -> false

  let push (f, b) e =
    norm (f, e :: b)

  let pushl q l =
    List.fold_left push q l

  let single e =
    push empty e

  let pop q =
    match q with
    | (f :: fs, b) -> Some (f, norm (fs, b))
    | _ -> None

  let to_list (f, b) =
    f @ List.rev b

  let mem (f, b) x =
    List.mem x f ||
    List.mem x b

  let push_uniq q x =
    if mem q x
    then q
    else push q x

end
