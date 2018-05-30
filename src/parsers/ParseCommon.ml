open Util
open Lexing

exception Error of string

let () =
  Printexc.register_printer (function
    | Error s -> Some ("ParseCommon.Error:\n" ^ s)
    | _ -> None)

let set_fname path lexbuf =
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = path }

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos
             ; pos_lnum = pos.pos_lnum + 1 }

let string_of_lexpos lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let log_lexer lex lexbuf =
  let res = lex lexbuf in
  Printf.printf "Saw token [%s]\n%!" (Lexing.lexeme lexbuf);
  res

let parse_aux parse lex lexbuf =
  (* to log tokens, replace "lex" with "(log_lexer lex)" below *)
  try parse lex lexbuf with
  | Error msg ->
      raise (Error (Printf.sprintf "%s: %s"
        (string_of_lexpos lexbuf)
        msg))
  | e ->
      raise (Error (Printf.sprintf "
%s: unexpected exception while parsing

%s

Backtrace:
%s"     (string_of_lexpos lexbuf)
        (Printexc.to_string e)
        (Printexc.get_backtrace ())))

let of_file parse lex path =
  with_ic path (fun ic ->
    ic |> Lexing.from_channel
       >> set_fname path
       |> parse_aux parse lex)

let of_string parse lex s =
    s |> Lexing.from_string
      >> set_fname "(input)"
      |> parse_aux parse lex
