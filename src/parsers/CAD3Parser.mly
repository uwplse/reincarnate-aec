%parameter <N : NumSys.NUM>

%parameter <M3 : Mesh.MESH3
                 with type num = N.t
                  and type pt  = N.t * N.t * N.t>

%parameter <C3 : CAD.CAD3
                 with type num  = N.t
                  and type mesh = M3.t>

%start cad3
%type <C3.t> cad3

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

mesh3:
  m3 = otag_list(MESH3, tuple3(tuple3(num)))
    { M3.mesh m3 }

cad3:
  option(CAD3) c = c3
    { c }

c3:
  | EMPTY
      { C3.Empty }
  | UNIT
      { C3.Unit }
  | SPHERE
      { C3.Sphere }
  | CYLINDER
      { C3.Cylinder }
  | HEXAGON
      { C3.Hexagon }
  | PENTAGON
      { C3.Pentagon }
  | MESH LCURL m = mesh3 RCURL
      { C3.Mesh m }
  | op = unop3 LCURL a = c3 RCURL
      { C3.Unop (op, a) }
  | op = binop3 LCURL a = c3 b = c3 RCURL
      { C3.Binop (op, a, b) }

unop3:
  | HULL                { C3.Hull       }
  | TRANS   tuple3(num) { C3.Trans   $2 }
  | HOME    tuple3(num) { C3.Home    $2 }
  | SCALE   tuple3(num) { C3.Scale   $2 }
  | FIT     tuple3(num) { C3.Fit     $2 }
  | ROTATEX tuple1(num) { C3.RotateX $2 }
  | ROTATEY tuple1(num) { C3.RotateY $2 }
  | ROTATEZ tuple1(num) { C3.RotateZ $2 }

binop3:
  | UNION { C3.Union }
  | DIFF  { C3.Diff  }
  | INTER { C3.Inter }

