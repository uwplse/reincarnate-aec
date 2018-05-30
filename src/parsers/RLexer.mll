{

open Lexing
open ParseCommon
open ParserLib

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
  | "MESH1"   { MESH1 }
  | "MESH2"   { MESH2 }
  | "MESH3"   { MESH3 }
  | "CAD1"    { CAD1  }
  | "CAD2"    { CAD2  }
  | "CAD3"    { CAD3  }

  | "PI"      { PI      }
  | "abs"     { ABS     }
  | "+"       { ADD     }
  | "-"       { SUB     }
  | "*"       { MUL     }
  | "/"       { DIV     }
  | "rem"     { REM     }
  | "sqrt"    { SQRT    }
  | "sin"     { SIN     }
  | "cos"     { COS     }
  | "tan"     { TAN     }
  | "atan2"   { ATAN2   }

  | "true"    { TRUE  }
  | "false"   { FALSE }
  | "not"     { NOT   }
  | "&&"      { CONJ  }
  | "||"      { DISJ  }

  | "solid"    { SOLID    }
  | "endsolid" { ENDSOLID }
  | "facet"    { FACET    }
  | "endfacet" { ENDFACET }
  | "outer"    { OUTER    }
  | "loop"     { LOOP     }
  | "endloop"  { ENDLOOP  }
  | "normal"   { NORMAL   }
  | "vertex"   { VERTEX   }

  | "Diff"     { DIFF     }
  | "Empty"    { EMPTY    }
  | "Fit"      { FIT      }
  | "Mesh"     { MESH     }
  | "Home"     { HOME     }
  | "Hull"     { HULL     }
  | "Inter"    { INTER    }
  | "Rotate"   { ROTATE   }
  | "RotateX"  { ROTATEX  }
  | "RotateY"  { ROTATEY  }
  | "RotateZ"  { ROTATEZ  }
  | "Scale"    { SCALE    }
  | "Trans"    { TRANS    }
  | "Union"    { UNION    }
  | "Unit"     { UNIT     }
  | "Sphere"   { SPHERE   }
  | "Cylinder" { CYLINDER }
  | "Hexagon"  { HEXAGON  }
  | "Pentagon" { PENTAGON }

  | "="  { EQ }
  | "<"  { LT }
  | "<=" { LE }
  | ">"  { GT }
  | ">=" { GE }

  | "fun"   { FUN   }
  | "fix"   { FIX   }
  | "where" { WHERE }
  | "let"   { LET   }
  | "rec"   { REC   }
  | "in"    { IN    }
  | "if"    { IF    }
  | "then"  { THEN  }
  | "else"  { ELSE  }
  | "elif"  { ELIF  }

  | "Bench" { BENCH }
  | "Input" { INPUT }
  | "Fuel"  { FUEL  }
  | "Prims" { PRIMS }

  | ":"  { COLON   }
  | ","  { COMMA   }
  | ";"  { SEMI    }
  | "|"  { PIPE    }
  | "[|" { LRCONST }
  | "|]" { RRCONST }
  | "("  { LPAREN  }
  | ")"  { RPAREN  }
  | "["  { LSQUARE }
  | "]"  { RSQUARE }
  | "{"  { LCURL   }
  | "}"  { RCURL   }
  | "|>" { PIPELN  }
  | "@@" { ATAT    }
  | "<-" { LARR    }
  | "->" { RARR    }
  | eof  { EOF     }

  | '"' { read_string (Buffer.create 17) lexbuf }
  | numlit as x { NUMLIT x }
  | name   as x { NAME x   }

  (* ignore *)
  | comment { token lexbuf }
  | white   { token lexbuf }
  | line    { next_line lexbuf; token lexbuf }

  (* error *)
  | _ as c {
      raise (ParseCommon.Error
        (Printf.sprintf "Unexpected char: %c" c))
    }

and read_string buf = parse
  | '"'       { STRLIT (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'    ; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'   ; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'   ; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012' ; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'   ; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'   ; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'   ; read_string buf lexbuf }
  | line {
      next_line lexbuf;
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | [^ '"' '\\']+ {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ as c {
      raise (ParseCommon.Error
        (Printf.sprintf "Illegal string character: %c" c))
    }
  | eof {
      raise (ParseCommon.Error
        "Unterminated string literal")
    }
