%parameter <N : NumSys.NUM>

%parameter <M1 : Mesh.MESH1
                 with type num = N.t
                  and type pt  = N.t>

%parameter <SB1 : SynthBench.SYNTHBENCH
                  with type mesh = M1.t>

%start sb1
%type <SB1.t> sb1

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

sb1:
  BENCH COLON b = bench
  INPUT COLON m = mesh1
  FUEL  COLON f = fuel
  PRIMS COLON p = option(square_semi_list(prim))
  EOF
    { SB1.mkbench b m f p }

bench:
  NAME
    { $1 }

mesh1:
  m1 = otag_list(MESH1, num)
    { M1.mesh m1 }

fuel:
  NUMLIT
    { int_of_string $1 }

prim:
  STRLIT
    { SB1.prim_of_string $1 }

