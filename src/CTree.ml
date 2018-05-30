type 'a ctree =
  | Node of ('a * 'a ctree) list

let empty =
  Node []

let flip f a b = f b a

let (|>) x f = f x

let flatmap f l =
  l |> List.map f
    |> List.flatten

(** returns a list of all ['a]s in the tree *)
let rec to_list n =
  match n with
  | Node [] -> []
  | Node ls ->
      let fst_vs = List.map fst ls in
      let snds   = List.map snd ls in
      let snd_vs = flatmap to_list snds in
      fst_vs @ snd_vs

let rec insert (contains: 'a -> 'a -> bool) e t =
  match t with
  | Node ts ->
      ( match List.partition (fun (x, _) -> contains e x) ts with
        | ([], _) ->
            (* nothing in this node contains e *)
            ( match List.partition (fun (x, _) -> contains x e) ts with
              | (ins, outs) ->
                  Node ((e, Node ins) :: outs)
            )
        | ([(p, ct)], ts') ->
            (* p uniquely contains e *)
            Node ((p, insert contains e ct) :: ts')
        | (_, _) ->
            failwith "CTree.insert: element has multiple parents"
      )

(** takes a list [l] and makes a CTree *)
let mkctree contains l =
  List.fold_left (flip (insert contains)) empty l
