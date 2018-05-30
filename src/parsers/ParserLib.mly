(** generic literals *)

%token <string> NAME
%token <string> NUMLIT
%token <string> STRLIT

(** NumSys *)

%token PI
%token ABS
%token ADD
%token SUB
%token MUL
%token DIV
%token REM
%token SQRT
%token SIN
%token COS
%token TAN
%token ATAN2

(** PropLogic *)

%token TRUE
%token FALSE
%token NOT
%token CONJ
%token DISJ

(** Mesh, STL *)

%token MESH1
%token MESH2
%token MESH3

%token SOLID
%token ENDSOLID
%token FACET
%token ENDFACET
%token LOOP
%token ENDLOOP
%token OUTER
%token NORMAL
%token VERTEX

(** CAD *)

%token CAD1
%token CAD2
%token CAD3

%token DIFF
%token EMPTY
%token FIT
%token MESH
%token HOME
%token HULL
%token INTER
%token ROTATE
%token ROTATEX
%token ROTATEY
%token ROTATEZ
%token SCALE
%token TRANS
%token UNION
%token UNIT
%token SPHERE
%token CYLINDER
%token HEXAGON
%token PENTAGON

(** LambdaCAD *)

%token EQ
%token LT
%token LE
%token GT
%token GE

%token FUN
%token FIX
%token WHERE
%token LET
%token REC
%token IN
%token IF
%token THEN
%token ELSE
%token ELIF

(** SynthBench *)
%token BENCH
%token INPUT
%token FUEL
%token PRIMS

(** common punctuation *)

%token COMMA
%token SEMI
%token PIPE
%token LRCONST
%token RRCONST
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token LCURL
%token RCURL
%token ATAT
%token PIPELN
%token LARR
%token RARR
%token EOF
%token COLON

(*
%left     DISJ
%left     CONJ
%nonassoc NOT
%left     ADD SUB
%left     MUL DIV REM
%nonassoc ABS SQRT SIN COS TAN unary_neg
*)

%nonassoc IN
%nonassoc top_prec
%right    ATAT
%left     PIPELN
%nonassoc FIX
%nonassoc ELSE
%right    COMMA
%left     DISJ
%left     CONJ
%nonassoc EQ LT LE GT GE
%nonassoc NOT
%left     ADD SUB
%left     MUL DIV REM
%nonassoc ABS SQRT SIN COS TAN unary_neg

%start rconst
%type <NumSys.RealConst.t> rconst

%start rconst_top
%type <NumSys.RealConst.t> rconst_top

%start formula
%type <PropLogic.t> formula

%{
  module RC = NumSys.RealConst
  module PL = PropLogic
%}

%%

%public
rconst:
  LRCONST e = rconst_e RRCONST
    { e }

rconst_top:
  e = rconst_e EOF
    { e }

rconst_e:
  | PI     { RC.pi     }
  | NUMLIT { RC.lit $1 }

  | ABS  rconst_e { RC.op1 RC.Abs  $2 }
  | SQRT rconst_e { RC.op1 RC.Sqrt $2 }
  | SIN  rconst_e { RC.op1 RC.Sin  $2 }
  | COS  rconst_e { RC.op1 RC.Cos  $2 }
  | TAN  rconst_e { RC.op1 RC.Tan  $2 }

  | rconst_e ADD rconst_e { RC.op2 RC.Add $1 $3 }
  | rconst_e SUB rconst_e { RC.op2 RC.Sub $1 $3 }
  | rconst_e MUL rconst_e { RC.op2 RC.Mul $1 $3 }
  | rconst_e DIV rconst_e { RC.op2 RC.Div $1 $3 }
  | rconst_e REM rconst_e { RC.op2 RC.Rem $1 $3 }

  | SUB rconst_e %prec unary_neg
      { RC.op1 RC.Neg $2 }
  | ATAN2 LPAREN a = rconst_e COMMA b = rconst_e RPAREN
      { RC.op2 RC.Atan2 a b }
  | LPAREN rc = rconst_e RPAREN
      { rc }

formula:
  form EOF
    { $1 }

form:
  | fs = list(simple_form)
      { List.fold_left PL.mkdisj PL.False fs }
  | NOT f = form
      { PL.Not f }
  | l = form CONJ r = form
      { PL.Conj (l, r) }
  | l = form DISJ r = form
      { PL.Disj (l, r) }

simple_form:
  | TRUE  { PL.True   }
  | FALSE { PL.False  }
  | NAME  { PL.Var $1 }
  | LPAREN form RPAREN
      { $2 }

%public
separated_triple(sep, X, Y, Z):
  x = X; sep; y = Y; sep; z = Z
    { (x, y, z) }

%public
square_semi_list(X):
  xs = delimited(LSQUARE, separated_list(SEMI, X), RSQUARE)
    { xs }

%public
otag_list(TAG, X):
  xs = preceded(option(TAG), square_semi_list(X))
    { xs }

%public
tuple1(X):
  t1 = delimited(LPAREN, X, RPAREN)
    { t1 }

%public
tuple2(X):
  t2 = delimited(LPAREN, separated_pair(X, COMMA, X), RPAREN)
    { t2 }

%public
tuple3(X):
  t3 = delimited(LPAREN, separated_triple(COMMA, X, X, X), RPAREN)
    { t3 }
