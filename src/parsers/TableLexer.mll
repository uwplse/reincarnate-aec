{

open Lexing
open ParseCommon
open TableParser

}

let e = 'e' | 'E'

let numlit =
  "-"?['0'-'9']*("."['0'-'9']*)?(e"-"?['0'-'9']*)?

let name =
  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'' '.']*

let comment =
  "#"[^'\n']*

let white =
  [' ' '\t']+

let line =
  '\r' | '\n' | "\r\n"

rule token = parse
  | "let"       { LET }

  | "="         { EQ }
  | "+"         { ADD }
  | "-"         { SUB }
  | "*"         { MUL }
  | "/"         { DIV }
  | "rem"       { REM }

  | "abs"       { ABS }
  | "sin"       { SIN }
  | "cos"       { COS }
  | "tan"       { TAN }
  | "atan2"     { ATAN2 }
  | "sqrt"      { SQRT }

  | "("         { LPAREN }
  | ")"         { RPAREN }
  | ","         { COMMA }

  | "pi"        { PI }
  | "unk"       { UNK }

  | eof         { EOF }

  | numlit as x { NUM x}
  | name as x   { VAR x }

  | comment { token lexbuf }
  | white   { token lexbuf }
  | line    { next_line lexbuf; token lexbuf }

  (* error *)
  | _ as c {
      raise (ParseCommon.Error
        (Printf.sprintf "Unexpected char: %c" c))
    }
