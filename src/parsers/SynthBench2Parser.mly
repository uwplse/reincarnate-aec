%parameter <N : NumSys.NUM>

%parameter <M2 : Mesh.MESH2
                 with type num = N.t
                  and type pt  = N.t * N.t>

%parameter <SB2 : SynthBench.SYNTHBENCH
                  with type mesh = M2.t>

%start sb2
%type <SB2.t> sb2

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

sb2:
  BENCH COLON b = bench
  INPUT COLON m = mesh2
  FUEL  COLON f = fuel
  PRIMS COLON p = option(square_semi_list(prim))
  EOF
    { SB2.mkbench b m f p }

bench:
  NAME
    { $1 }

mesh2:
  m2 = otag_list(MESH2, tuple2(tuple2(num)))
    { M2.mesh m2 }

fuel:
  NUMLIT
    { int_of_string $1 }

prim:
  STRLIT
    { SB2.prim_of_string $1 }

