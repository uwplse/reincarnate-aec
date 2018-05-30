%parameter <N : NumSys.NUM>

%parameter <M1 : Mesh.MESH1
                 with type num = N.t
                  and type pt  = N.t>

%start mesh1
%type <M1.t> mesh1

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

mesh1:
  m1 = otag_list(MESH1, num)
    { M1.mesh m1 }

