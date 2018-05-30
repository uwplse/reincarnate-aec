open Util
open NumSys

module type MATRIX = sig
  type num
  type t

  val of_list       : num list list -> t
  val to_string     : t -> string
  val gaussian_elim : t -> unit
  val get           : t -> int -> int -> num
  val det           : num list list -> num
end

module Matrix (N : NUM) : (MATRIX with type num = N.t) = struct

  type num = N.t

  type t =
    { buf: num array array
    ; inner_dim: int
    }

  let of_list l =
    let buf =
      l |> List.map Array.of_list
        |> Array.of_list
    in
    let idim =
      if Array.length buf > 0
      then Array.length (Array.get buf 0)
      else 0
    in
    { buf = buf
    ; inner_dim = idim
    }

  let to_list m =
    m.buf |> Array.to_list
          |> List.map Array.to_list

  let to_string m =
    m |> to_list
      |> List.map (List.map N.to_string)
      |> List.map (String.concat " ")
      |> String.concat "\n"

  let swap_rows m i j =
    let t = m.buf.(i) in
    m.buf.(i) <- m.buf.(j);
    m.buf.(j) <- t

  let add_scaled_row m dst src alpha =
    let r_dst = m.buf.(dst) in
    let r_src = m.buf.(src) in
    for j = 0 to m.inner_dim - 1 do
      r_dst.(j) <- N.add r_dst.(j) (N.mul alpha r_src.(j))
    done

  let get m i j =
    m.buf.(i).(j)

  let set m i j x =
    m.buf.(i).(j) <- x

  (* https://rosettacode.org/wiki/Matrix_arithmetic#Java *)
  let minor mat x y =
    let res =
      Array.make_matrix
        (Array.length mat - 1)
        (Array.length (Array.get mat 0) - 1)
        N.n0
    in
    for i = 0 to (Array.length mat - 2) do
      for j = 0 to (Array.length (Array.get mat 0) - 2) do
        if i < x && j < y then
          res.(i).(j) <- mat.(i).(j)
        else if i >= x && j < y then
          res.(i).(j) <- mat.(i + 1).(j)
        else if i < x && j >= y then
          res.(i).(j) <- mat.(i).(j + 1)
        else
          res.(i).(j) <- mat.(i + 1).(j + 1)
      done
    done;
    res

  let rec det_aux mat =
    match mat with
    | [| |] -> failwith "empty matrix"
    | [| [| x |] |] -> x
    | _ ->
        let sign = ref N.n1 in
        let sum  = ref N.n0 in
        for i = 0 to (Array.length mat - 1) do
          sum :=
            N.add !sum
                  (N.mul !sign
                          (N.mul mat.(0).(i)
                                 (det_aux (minor mat 0 i))));
          sign := N.mul !sign (N.neg N.n1);
        done;
        !sum

  let det m =
    let mat =
      Array.make_matrix
        (List.length m)
        (List.length (List.hd m))
        N.n0
    in
    for i = 0 to (List.length m - 1) do
      for j = 0 to (List.length (List.hd m) - 1) do
        mat.(i).(j) <- List.nth (List.nth m i) j
      done
    done;
    det_aux mat

  (* transcribed from https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode *)
  let gaussian_elim mat =
    let m = Array.length mat.buf in
    let n = mat.inner_dim in
    for k = 0 to Pervasives.min m n - 1 do
      let i_max =
        argmax N.cmp
          (fun i -> N.abs (get mat i k))
          (range k m)
      in
      if N.equiv N.n0 (get mat i_max k) then
        (* this column is all zeroes; this algorithm is incorrect if this
           happens anywhere other than the rightmost un-augmented column. *)
        ()
      else begin
          swap_rows mat k i_max;
          for i = k + 1 to m - 1 do
            let f = N.div (get mat i k) (get mat k k) in
            for j = k + 1 to n - 1 do
              set mat i j (N.sub (get mat i j)
                                 (N.mul f (get mat k j)))
            done;
            set mat i k N.n0
          done
        end
    done
end
