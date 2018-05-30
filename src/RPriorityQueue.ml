open Util

module type ORDERED_TYPE = sig
  type t
  val order : t -> t -> cmp
end

module type PRIORITY_QUEUE = sig
  type priority
  type 'a t

  val empty    : 'a t
  val single   : (priority * 'a) -> 'a t
  val push     : 'a t -> (priority * 'a) -> 'a t
  val pushl    : 'a t -> (priority * 'a) list -> 'a t
  val is_empty : 'a t -> bool
  val pop      : 'a t -> (priority * 'a * 'a t) option
  val to_list  : 'a t -> (priority * 'a) list

  exception EmptyQueue

end

module PriorityQueue (P : ORDERED_TYPE) :
  PRIORITY_QUEUE with type priority = P.t =
struct
  type priority = P.t

  type 'a t =
    | Empty
    | Node of priority * 'a * 'a t * 'a t

  exception EmptyQueue

  let empty = Empty

  let rec push q (p, e) =
    match q with
    | Empty ->
        Node (p, e, Empty, Empty)
    | Node (p', e', l, r) ->
       begin match P.order p p' with
       | LT
       | EQ ->
           Node (p', e', push r (p, e), l)
       | GT ->
           Node (p, e, push r (p', e'), l)
       end

  let single (p, e) =
    push Empty (p, e)

  let pushl q l =
    List.fold_left push q l

  let rec to_list = function
    | Empty -> []
    | Node (p, e, l, Empty) ->
        (to_list l) @ [(p, e)]
    | Node (p, e, Empty, r) ->
        [(p, e)] @ (to_list r)
    | Node (p, e, l, r) ->
        (to_list l) @ [(p, e)] @ (to_list r)

  let is_empty q =
    match q with
    | Empty -> true
    | _     -> false

  let rec remove_top = function
    | Empty ->
        raise EmptyQueue
    | Node (p, e, l, Empty) ->
        l
    | Node (p, e, Empty, r) ->
        r
    | Node ( p
           , e
           , (Node (lp, le, _, _) as l)
           , (Node (rp, re, _, _) as r)) ->
       begin match P.order lp rp with
       | LT
       | EQ ->
           Node (rp, re, l, remove_top r)
       | GT ->
           Node (lp, le, remove_top l, r)
       end

  let pop = function
    | Empty ->
        None
    | Node (p, e, _, _) as q ->
        Some (p, e, remove_top q)
end

