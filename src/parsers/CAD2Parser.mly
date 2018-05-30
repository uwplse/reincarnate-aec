%parameter <N : NumSys.NUM>

%parameter <M2 : Mesh.MESH2
                 with type num = N.t
                  and type pt  = N.t * N.t>

%parameter <C2 : CAD.CAD2
                 with type num  = N.t
                  and type mesh = M2.t>

%start cad2
%type <C2.t> cad2

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

mesh2:
  m2 = otag_list(MESH2, tuple2(tuple2(num)))
    { M2.mesh m2 }

cad2:
  option(CAD2) c = c2
    { c }

c2:
  | EMPTY
      { C2.Empty }
  | UNIT
      { C2.Unit }
  | MESH LCURL m = mesh2 RCURL
      { C2.Mesh m }
  | op = unop2 LCURL a = c2 RCURL
      { C2.Unop (op, a) }
  | op = binop2 LCURL a = c2 b = c2 RCURL
      { C2.Binop (op, a, b) }

unop2:
  | HULL               { C2.Hull      }
  | TRANS  tuple2(num) { C2.Trans  $2 }
  | HOME   tuple2(num) { C2.Home   $2 }
  | SCALE  tuple2(num) { C2.Scale  $2 }
  | FIT    tuple2(num) { C2.Fit    $2 }
  | ROTATE tuple1(num) { C2.Rotate $2 }

binop2:
  | UNION { C2.Union }
  | DIFF  { C2.Diff  }
  | INTER { C2.Inter }

