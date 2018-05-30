(** Directed graphs with labeled vertices and labeled edges. *)

module type GRAPH =
sig
  (** A graph with vertex labels of type ['v] and edge labels of type ['e]. *)
  type ('v, 'e) t

  (** A graph edge, which has a source, a destination, and a label. *)
  type ('v, 'e) edge =
    { src : 'v
    ; dst : 'v
    ; data : 'e
    }

  (** A path, which consists of [n+1] vertices and [n] edges. In particular, a
      path always consists of at least 1 vertex. *)
  type ('v, 'e) path

  (** Generically combine edge labels. Apply the second argument to each edge,
      and then combine the results using the binary operator given as the third
      argument. (For sane results, only associative binary operators should be
      used.) In case the path consists of no edges, the first argument supplies
      a default value.

      {b NOTE}: The first argument is {e not} called on every vertex. It is only
      used if the entire path consists of a single vertex and no edges. *)
  val fold_path_edges : ('v -> 'a) -> ('e -> 'a) -> ('a -> 'a -> 'a) -> ('v, 'e) path -> 'a

  val empty : ('v, 'e) t
  val add_edge : ('v, 'e) t -> ('v, 'e) edge -> ('v, 'e) t

  (** Attempt to find a shortest path between two given vertices in a graph. *)
  val search : ('v, 'e) t -> 'v -> 'v -> ('v, 'e) path option

  val to_string : ('v -> string) -> ('e -> string) -> ('v, 'e) t -> string
end

module Graph : GRAPH =
struct
  module Q = RQueue.Queue

  type ('v, 'e) edge =
    { src : 'v
    ; dst : 'v
    ; data : 'e
    }

  type ('v, 'e) path =
    | Refl of 'v
    | Edge of ('v, 'e) edge
    | Trans of ('v, 'e) path * ('v, 'e) path

  let fold_path_edges v e op =
    let rec loop = function
      | Refl x -> v x
      | Edge x -> e x.data
      | Trans (p1, p2) -> op (loop p1) (loop p2)
    in loop

  let rec path_src = function
    | Refl v -> v
    | Edge e -> e.src
    | Trans (p1, p2) -> path_src p1

  let rec path_dst = function
    | Refl v -> v
    | Edge e -> e.dst
    | Trans (p1, p2) -> path_dst p2

  type ('v, 'e) t = ('v * ('v, 'e) edge list) list

  let empty = []

  let get_adj_list v g =
    try List.assoc v g
    with Not_found -> []

  let set_adj_list v a =
    let rec loop = function
      | [] -> [(v, a)]
      | (v', a') :: g -> if v = v' then (v', a) :: g
                         else (v', a') :: loop g
    in loop

  let add_to_list x l =
    if List.mem x l then l else x :: l

  let add_edge g e =
    let a = get_adj_list e.src g in
    set_adj_list e.src (add_to_list e a) g

  let remove_adj_list v g =
    List.remove_assoc v g

  let path_append p e =
    Trans (p, Edge e)

  let rec bfs g q d =
    match Q.pop q with
    | None -> None
    | Some (p, q) ->
       if path_dst p = d then
         Some p
       else
        let s = path_dst p in
        let a = get_adj_list s g in
        let g = remove_adj_list s g in
        bfs g (Q.pushl q (List.map (fun e -> path_append p e) a)) d

  let search g s d = bfs g (Q.single (Refl s)) d

  let to_string v_to_string e_to_string g =
    let adj_list_to_string (v, a) =
      let edge_to_string e = Printf.sprintf "%s -(%s)-> %s"
                                         (v_to_string v)
                                         (e_to_string e.data)
                                         (v_to_string e.dst)
      in
      a |> List.map edge_to_string
        |> String.concat "\n"
    in
    g |> List.map adj_list_to_string
      |> String.concat "\n"
end
