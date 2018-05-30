open CAD

module N  = NumSys.FloatNum
module LA = LinAlg.Matrix(N)
module G3 = Geom.Geom3(N)(LA)
module M3 = Mesh.Mesh3(N)(G3)
module C3 = CAD3(N)(M3)

let usage = "
Fuzz

OPTIONS

    -n N            Number of failing expressions to generate (default: infinitely many)
    --depth N       Depth of expression to generate (default: 3)
    --log BASENAME  Log failing tests to files whose names start with BASENAME (default: do not log)
"

let n : int option ref = ref None
let depth = ref 3
let log : string option ref = ref None


let parse_args () =
  let bogus_arg x =
    prerr_endline usage;
    failwith ("Main: bogus arg: " ^ x)
  in
  let rec loop = function
    | [] -> ()
    | "-h" :: rest
    | "--help" :: rest ->
        print_endline usage;
        Pervasives.exit 0
    | "-n" :: x :: rest ->
        begin try
            n := Some (int_of_string x);
            loop rest
          with _ ->
            bogus_arg ("-n " ^ x)
        end
    | "--depth" :: x :: rest ->
       begin try
           depth := int_of_string x;
           loop rest
         with _ ->
              bogus_arg ("--depth " ^ x)
       end
    | "--log" :: x :: rest ->
       log := Some x;
       loop rest
    | x :: _ ->
        bogus_arg x
  in
  Sys.argv
    |> Array.to_list
    |> List.tl
    |> loop


let main () =
  parse_args ();
  let rec loop i =
    if Some i = !n then ()
    else
      let e = C3.rand !depth in
      begin try
          ignore (C3.compile e);
          loop i
        with
        | G3.BBoxOfEmpty -> loop i
        | ex ->
          Printf.printf "%s\n%!" (Printexc.to_string ex);
          Printf.printf "%s\n%!" (Printexc.get_backtrace ());
          Printf.printf "%s\n%!" (C3.to_string e);
          begin
          match !log with
          | None -> ()
          | Some log -> Util.to_file (Printf.sprintf "%s-%d.cad3" log i) (C3.to_string e)
          end;
          loop (i + 1)
      end
  in loop 0

let _ = Printexc.record_backtrace true

let _ = main ()
