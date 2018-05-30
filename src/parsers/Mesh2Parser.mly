%parameter <N : NumSys.NUM>

%parameter <M2 : Mesh.MESH2
                 with type num = N.t
                  and type pt  = N.t * N.t>

%start mesh2
%type <M2.t> mesh2

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

mesh2:
  m2 = otag_list(MESH2, tuple2(tuple2(num)))
    { M2.mesh m2 }

