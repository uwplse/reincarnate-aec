open Util

module type TIMEOUT = sig

  exception TimedOut of int

  val exn1 : int -> ('a -> 'b) -> 'a -> 'b
  val opt1 : int -> ('a -> 'b) -> 'a -> 'b option
  val exn2 : int -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  val opt2 : int -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option
  val exn3 : int -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
  val opt3 : int -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd option
end

module Timeout : TIMEOUT = struct
  include MakeTaggedLogging(struct let tag = "Tmout" end)

	exception TimedOut of int

  let () =
    Printexc.register_printer (function
      | TimedOut n ->
          Some (Printf.sprintf "Timed out after %d seconds." n)
      | _ -> None)

  let exn1 dur f arg =
    if dur <= 0 then begin
      log ("ignoring non-positive alarm " ^ string_of_int dur);
      f arg
    end else begin
      let new_handler =
        Sys.Signal_handle (fun _ -> raise (TimedOut dur)) in
      let old_handler =
        Sys.signal Sys.sigalrm new_handler in
      let reset () =
        ignore (Unix.alarm 0);
        ignore (Sys.set_signal Sys.sigalrm old_handler) in
      try
        log ("setting alarm for " ^ string_of_int dur);
        ignore (Unix.alarm dur);
        let res = f arg in
        reset ();
        log ("function returned before alarm fired");
        res
      with e -> begin
        reset ();
        raise e
      end
    end

  let opt1 dur f arg =
    try
      Some (exn1 dur f arg)
    with TimedOut _ ->
      None

  let exn2 dur f a b   = exn1 dur (fun (a, b)    -> f a b)   (a, b)
  let opt2 dur f a b   = opt1 dur (fun (a, b)    -> f a b)   (a, b)
  let exn3 dur f a b c = exn1 dur (fun (a, b, c) -> f a b c) (a, b, c)
  let opt3 dur f a b c = opt1 dur (fun (a, b, c) -> f a b c) (a, b, c)
end

