%parameter <N : NumSys.NUM>

%parameter <M3 : Mesh.MESH3
                 with type num = N.t
                  and type pt  = N.t * N.t * N.t>

%start mesh3
%type <M3.t> mesh3

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

mesh3:
  m3 = otag_list(MESH3, tuple3(tuple3(num)))
    { M3.mesh m3 }

