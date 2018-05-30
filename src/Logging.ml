module Logging : sig

  val set_log : out_channel -> unit
  val log : string -> unit

end = struct

  let time0 =
    Unix.gettimeofday ()

  let __log : out_channel option ref =
    ref None

  let set_log oc =
    __log := Some oc

  let log msg =
    match !__log with
    | None -> ()
    | Some oc ->
        Printf.fprintf oc "%08.03f %s\n%!"
          (Unix.gettimeofday () -. time0)
          msg
end
