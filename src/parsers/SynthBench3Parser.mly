%parameter <N : NumSys.NUM>

%parameter <M3 : Mesh.MESH3
                 with type num = N.t
                  and type pt  = N.t * N.t * N.t>

%parameter <SB3 : SynthBench.SYNTHBENCH
                  with type mesh = M3.t>

%start sb3
%type <SB3.t> sb3

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

sb3:
  BENCH COLON b = bench
  INPUT COLON m = mesh3
  FUEL  COLON f = fuel
  PRIMS COLON p = option(square_semi_list(prim))
  EOF
    { SB3.mkbench b m f p }

bench:
  NAME
    { $1 }

mesh3:
  m3 = otag_list(MESH3, tuple3(tuple3(num)))
    { M3.mesh m3 }

fuel:
  NUMLIT
    { int_of_string $1 }

prim:
  STRLIT
    { SB3.prim_of_string $1 }
