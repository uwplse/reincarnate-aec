%{

module RC = NumSys.RealConst

%}

%token <string> NUM
%token <string> VAR

%token LET

%token EQ
%token ADD
%token SUB
%token MUL
%token DIV
%token REM

%token ABS
%token SIN
%token COS
%token TAN
%token ATAN2
%token SQRT

%token LPAREN
%token RPAREN

%token PI
%token UNK

%token EOF
%token COMMA

%start table
%type <unit> table

%%

iter(X) :
  |              { () }
  | iter(X) X    { () }

table :
  iter(entry) EOF
    { $1 }

entry :
  LET x=VAR EQ e=expr
    { RC.register_symbol x e; (x, e) }

rvar :
  x=VAR
    { try RC.get_symbol x
      with e ->
        print_endline x;
        raise e
    }

expr :
  | PI { RC.pi }
  | NUM { RC.lit $1 }

  | UNK { RC.unknown }

  | ABS  LPAREN v=rvar RPAREN { RC.op1 RC.Abs  v }
  | SQRT LPAREN v=rvar RPAREN { RC.op1 RC.Sqrt v }
  | SIN  LPAREN v=rvar RPAREN { RC.op1 RC.Sin  v }
  | COS  LPAREN v=rvar RPAREN { RC.op1 RC.Cos  v }
  | TAN  LPAREN v=rvar RPAREN { RC.op1 RC.Tan  v }

  | rvar ADD rvar { RC.op2 RC.Add $1 $3 }
  | rvar SUB rvar { RC.op2 RC.Sub $1 $3 }
  | rvar MUL rvar { RC.op2 RC.Mul $1 $3 }
  | rvar DIV rvar { RC.op2 RC.Div $1 $3 }
  | rvar REM rvar { RC.op2 RC.Rem $1 $3 }

  | SUB rvar
      { RC.op1 RC.Neg $2 }
  | ATAN2 LPAREN a = rvar COMMA b = rvar RPAREN
      { RC.op2 RC.Atan2 a b }
  | LPAREN e = expr RPAREN
      { e }
