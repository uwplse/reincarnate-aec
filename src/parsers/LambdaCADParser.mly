%parameter <N : NumSys.NUM>

%parameter <LC : LambdaCAD.LAMBDACAD
                 with type num = N.t>

%start prog
%type <LC.letbinding list * LC.expr> prog

%start lib
%type <LC.letbinding list> lib

(*
%nonassoc IN
%nonassoc top_prec
%right    ATAT
%left     PIPELN
%nonassoc FIX
%nonassoc ELSE
%right    COMMA
%nonassoc EQ LT LE GT GE
*)

%{

let is_var s =
  let c = String.get s 0 in
  c = '_' || c = Char.lowercase_ascii c

%}

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

prog:
  | top_expr WHERE list(letbinding) EOF
      { ($3, $1) }
  | top_expr EOF
      { ([], $1) }

lib:
  list(letbinding) EOF
    { $1 }

top_expr:
  expr
  %prec top_prec
    { $1 }

lc_name:
  | NAME     { $1         }
  | MESH1    { "MESH1"    }
  | MESH2    { "MESH2"    }
  | MESH3    { "MESH3"    }
  | CAD1     { "CAD1"     }
  | CAD2     { "CAD2"     }
  | CAD3     { "CAD3"     }
  | PI       { "PI"       }
  | ABS      { "abs"      }
  | REM      { "rem"      }
  | SQRT     { "sqrt"     }
  | SIN      { "sin"      }
  | COS      { "cos"      }
  | TAN      { "tan"      }
  | ATAN2    { "atan2"    }
  | NOT      { "not"      }
  | SOLID    { "solid"    }
  | ENDSOLID { "endsolid" }
  | FACET    { "facet"    }
  | ENDFACET { "endfacet" }
  | OUTER    { "outer"    }
  | LOOP     { "loop"     }
  | ENDLOOP  { "endloop"  }
  | NORMAL   { "normal"   }
  | VERTEX   { "vertex"   }
  | DIFF     { "Diff"     }
  | EMPTY    { "Empty"    }
  | FIT      { "Fit"      }
  | MESH     { "Mesh"     }
  | HOME     { "Home"     }
  | HULL     { "Hull"     }
  | INTER    { "Inter"    }
  | ROTATE   { "Rotate"   }
  | ROTATEX  { "RotateX"  }
  | ROTATEY  { "RotateY"  }
  | ROTATEZ  { "RotateZ"  }
  | SCALE    { "Scale"    }
  | TRANS    { "Trans"    }
  | UNION    { "Union"    }
  | UNIT     { "Unit"     }

var:
  nm = lc_name
    { if is_var nm
      then nm
      else raise (ParseCommon.Error (Printf.sprintf "
LambdaCAD variables must begin with a lowercase letter, not '%s'
"         nm)) }

name_expr:
  nm = lc_name
    { if is_var nm
      then LC.Var  nm
      else LC.Prim nm }

expr:
  | simple_expr
      { $1 }
  | simple_expr simple_exprs
      { List.fold_left LC.mkapp $1 (List.rev $2) }

  | SUB expr
    %prec unary_neg
      { LC.prim_app "Neg" $2 }

  | expr CONJ expr
      { LC.Cond ($1, $3, LC.Bool false) }
  | expr DISJ expr
      { LC.Cond ($1, LC.Bool true,  $3) }

  | expr COMMA expr { LC.Cons ($1, $3) }
  | expr ADD   expr { LC.prim_app_cons "Add"  $1 $3 }
  | expr SUB   expr { LC.prim_app_cons "Sub"  $1 $3 }
  | expr MUL   expr { LC.prim_app_cons "Mul"  $1 $3 }
  | expr DIV   expr { LC.prim_app_cons "Div"  $1 $3 }
  | expr EQ    expr { LC.prim_app_cons "Eq"   $1 $3 }
  | expr LT    expr { LC.prim_app_cons "Lt"   $1 $3 }
  | expr LE    expr { LC.prim_app_cons "Le"   $1 $3 }
  | expr GT    expr { LC.prim_app_cons "Gt"   $1 $3 }
  | expr GE    expr { LC.prim_app_cons "Ge"   $1 $3 }

  | FUN var RARR top_expr
      { LC.Fun ($2, $4) }
  | letbinding IN expr
      { LC.mklet $1 $3 }
  | FIX expr
      { LC.Fix $2 }
  | expr PIPELN expr
      { LC.App ($3, $1) }
  | expr ATAT expr
      { LC.App ($1, $3) }
  | IF expr THEN expr elses
      { LC.Cond ($2, $4, $5) }

elses:
  | ELSE expr
      { $2 }
  | ELIF expr THEN expr elses
      { LC.Cond ($2, $4, $5) }

simple_expr:
  | TRUE      { LC.Bool true    }
  | FALSE     { LC.Bool false   }
  | num       { LC.Num $1       }
  | STRLIT    { LC.Str $1       }
  | name_expr { $1              }
  | LPAREN RPAREN      { LC.Nil }
  | LPAREN expr RPAREN { $2     }
  | LCURL  expr RCURL  { $2     }

simple_exprs:
  | simple_expr
      { [$1] }
  | simple_exprs simple_expr
      { $2 :: $1 }

letbinding:
  | LET boption(REC) list(var) EQ expr
      { ($2, $3, $5) }

