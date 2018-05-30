%parameter <N : NumSys.NUM>

%parameter <M1 : Mesh.MESH1
                 with type num = N.t
                  and type pt  = N.t>

%parameter <C1 : CAD.CAD1
                 with type num  = N.t
                  and type mesh = M1.t>

%start cad1
%type <C1.t> cad1

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

mesh1:
  m1 = otag_list(MESH1, num)
    { M1.mesh m1 }

cad1:
  option(CAD1) c = c1
    { c }

c1:
  | EMPTY
      { C1.Empty }
  | UNIT
      { C1.Unit }
  | MESH LCURL m = mesh1 RCURL
      { C1.Mesh m }
  | op = unop1 LCURL a = c1 RCURL
      { C1.Unop (op, a) }
  | op = binop1 LCURL a = c1 b = c1 RCURL
      { C1.Binop (op, a, b) }

unop1:
  | HULL              { C1.Hull     }
  | TRANS tuple1(num) { C1.Trans $2 }
  | SCALE tuple1(num) { C1.Scale $2 }

binop1:
  | UNION { C1.Union }
  | DIFF  { C1.Diff  }
  | INTER { C1.Inter }

