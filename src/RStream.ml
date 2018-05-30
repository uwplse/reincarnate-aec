module Stream : sig

  type 'a t

  val empty   : 'a t
  val single  : 'a -> 'a t
  val nats    : int t
  val map     : ('a -> 'b) -> 'a t -> 'b t
  val from    : (int -> 'a) -> 'a t
  val fromo   : (int -> 'a option) -> 'a t
  val filter  : ('a -> bool) -> 'a t -> 'a t
  val get     : 'a t -> ('a * 'a t) option
  val find    : ('a -> bool) -> 'a t -> 'a option
  val fold    : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val app     : 'a t -> 'a t -> 'a t
  val flatten : 'a t t -> 'a t
  val xprod   : 'a t -> 'b t -> ('a * 'b) t
  val uniq    : 'a t -> 'a t

end = struct

  type 'a cell =
    | Nil
    | Cons of 'a * 'a t
  and 'a t =
    unit -> 'a cell

  let empty () =
    Nil

  let single x () =
    Cons (x, empty)

  let nats =
    let rec loop i () =
      Cons (i, loop (i + 1))
    in
    loop 0

  let rec map f s () =
    match s () with
    | Nil ->
        Nil
    | Cons (x, s') ->
        Cons (f x, map f s')

  let from f =
    map f nats

  let fromo f =
    let rec loop i () =
      match f i with
      | Some x -> Cons (x, loop (i + 1))
      | None   -> Nil
    in
    loop 0

  (* NOTE may effectively collapse stream *)
  let rec filter f s () =
    match s () with
    | Nil ->
        Nil
    | Cons (x, s') ->
        if f x
        then Cons (x, filter f s')
        else filter f s' ()

  let get s =
    match s () with
    | Nil ->
        None
    | Cons (x, s') ->
        Some (x, s')

  (* NOTE collapses stream *)
  (* NOTE find of an infinite stream <> None *)
  let rec find f s =
    match s () with
    | Nil ->
        None
    | Cons (x, s') ->
        if f x
        then Some x
        else find f s'

  (* NOTE collapses stream *)
  let rec fold f acc s =
    match s () with
    | Nil ->
        acc
    | Cons (x, s') ->
        fold f (f acc x) s'

  let rec app s1 s2 () =
    match s1 () with
    | Nil ->
        s2 ()
    | Cons (x, s1') ->
        Cons (x, app s1' s2)

  let rec flatten s () =
    match s () with
    | Nil ->
        Nil
    | Cons (ss, s') ->
        app ss (flatten s') ()

  let rec xprod s1 s2 () =
    match s1 () with
    | Nil ->
        Nil
    | Cons (x, s1') ->
        app (map (fun y -> (x, y)) s2)
            (xprod s1' s2)
            ()

  let uniq stream =
    let tbl = Hashtbl.create 1000 in
    let rec loop s () =
      match s () with
      | Nil ->
          Nil
      | Cons (x, s') ->
          if Hashtbl.mem tbl x then
            loop s' ()
          else begin
            Hashtbl.add tbl x ();
            Cons (x, loop s')
          end
    in
    loop stream

end
