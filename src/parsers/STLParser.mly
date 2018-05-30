%parameter <N : NumSys.NUM>

%parameter <M3 : Mesh.MESH3
                   with type num = N.t
                    and type pt  = N.t * N.t * N.t>

%start stl
%type <M3.t> stl

%%

num:
  | nl = NUMLIT
      { N.of_string nl }
  | rc = rconst
      { N.of_const rc }

stl:
  SOLID NAME fs = list(facet) ENDSOLID
    { M3.mesh fs }

facet:
  FACET n = normal
    OUTER LOOP
      v1 = vertex
      v2 = vertex
      v3 = vertex
    ENDLOOP
  ENDFACET
    (* ignore normal *)
    { (v1, v2, v3) }

normal:
  NORMAL vect
    { $2 }

vertex:
  VERTEX vect
    { $2 }

vect:
  num num num
    { ($1, $2, $3) }

