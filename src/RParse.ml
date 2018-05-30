open Util
open NumSys
open Mesh
open CAD
open LambdaCAD
open SynthBench

let ir_of_file   p = ParseCommon.of_file   p RLexer.token
let ir_of_string p = ParseCommon.of_string p RLexer.token

let rconst_of_file       = ir_of_file   ParserLib.rconst
let rconst_of_string     = ir_of_string ParserLib.rconst
let rconst_top_of_file   = ir_of_file   ParserLib.rconst_top
let rconst_top_of_string = ir_of_string ParserLib.rconst_top
let proplogic_of_file    = ir_of_file   ParserLib.formula
let proplogic_of_string  = ir_of_string ParserLib.formula

module MakeMesh1Parser
  (N : NUM)
  (M : MESH1 with type num = N.t
              and type pt  = N.t) :
sig
  val of_file   : string -> M.t
  val of_string : string -> M.t
end = struct
  module P = Mesh1Parser.Make(N)(M)
  let of_file   = ir_of_file   P.mesh1
  let of_string = ir_of_string P.mesh1
end

module MakeMesh2Parser
  (N : NUM)
  (M : MESH2 with type num = N.t
              and type pt  = N.t * N.t) :
sig
  val of_file   : string -> M.t
  val of_string : string -> M.t
end = struct
  module P = Mesh2Parser.Make(N)(M)
  let of_file   = ir_of_file   P.mesh2
  let of_string = ir_of_string P.mesh2
end

module MakeMesh3Parser
  (N : NUM)
  (M : MESH3 with type num = N.t
              and type pt  = N.t * N.t * N.t) :
sig
  val of_file   : string -> M.t
  val of_string : string -> M.t
end = struct
  module P = Mesh3Parser.Make(N)(M)
  let of_file   = ir_of_file   P.mesh3
  let of_string = ir_of_string P.mesh3
end

module MakeSTLParser
  (N : NUM)
  (M : MESH3 with type num = N.t
              and type pt  = N.t * N.t * N.t) :
sig
  val of_file   : string -> M.t
  val of_string : string -> M.t
end = struct
  module P  = STLParser.Make(N)(M)
  module BP = STLBinaryParser.Make(N)(M)

  let of_file path =
    try
      ir_of_file P.stl path
    with ParseCommon.Error msg1 ->
      try
        BP.of_file path
      with e ->
        raise (ParseCommon.Error (Printf.sprintf
          "Could not parse STL as ASCII:\n%s\n\nnor as binary:\n%s\n%s\n"
            msg1
            (Printexc.to_string e)
            (Printexc.get_backtrace ())))

  let of_string = ir_of_string P.stl
end

module MakeCAD1Parser
  (N : NUM)
  (M : MESH1 with type num  = N.t
              and type pt   = N.t)
  (C : CAD1  with type num  = N.t
              and type mesh = M.t) :
sig
  val of_file   : string -> C.t
  val of_string : string -> C.t
end = struct
  module P = CAD1Parser.Make(N)(M)(C)
  let of_file   = ir_of_file   P.cad1
  let of_string = ir_of_string P.cad1
end

module MakeCAD2Parser
  (N : NUM)
  (M : MESH2 with type num  = N.t
              and type pt   = N.t * N.t)
  (C : CAD2  with type num  = N.t
              and type mesh = M.t) :
sig
  val of_file   : string -> C.t
  val of_string : string -> C.t
end = struct
  module P = CAD2Parser.Make(N)(M)(C)
  let of_file   = ir_of_file   P.cad2
  let of_string = ir_of_string P.cad2
end

module MakeCAD3Parser
  (N : NUM)
  (M : MESH3 with type num  = N.t
              and type pt   = N.t * N.t * N.t)
  (C : CAD3  with type num  = N.t
              and type mesh = M.t) :
sig
  val of_file   : string -> C.t
  val of_string : string -> C.t
end = struct
  module P = CAD3Parser.Make(N)(M)(C)
  let of_file   = ir_of_file   P.cad3
  let of_string = ir_of_string P.cad3
end

module MakeLCParser
  (N  : NUM)
  (LC : LAMBDACAD with type num = N.t) :
sig
  val of_file   : string -> LC.expr
  val of_string : string -> LC.expr
end = struct
  module P = LambdaCADParser.Make(N)(LC)

  let prelude =
    ir_of_string P.lib LC.prelude

  let inject_prelude (lets, expr) =
    LC.mklets (prelude @ lets) expr

  let of_file   f = inject_prelude (ir_of_file   P.prog f)
  let of_string s = inject_prelude (ir_of_string P.prog s)
end

module MakeSB1Parser
  (N   : NUM)
  (M1  : MESH1 with type num  = N.t
                and type   pt = N.t)
  (SB1 : SYNTHBENCH with type mesh = M1.t) :
sig
  val of_file   : string -> SB1.t
  val of_string : string -> SB1.t
end = struct
  module P = SynthBench1Parser.Make(N)(M1)(SB1)
  let of_file   = ir_of_file   P.sb1
  let of_string = ir_of_string P.sb1
end

module MakeSB2Parser
  (N   : NUM)
  (M2  : MESH2 with type num  = N.t
                and type   pt = N.t * N.t)
  (SB2 : SYNTHBENCH with type mesh = M2.t) :
sig
  val of_file   : string -> SB2.t
  val of_string : string -> SB2.t
end = struct
  module P = SynthBench2Parser.Make(N)(M2)(SB2)
  let of_file   = ir_of_file   P.sb2
  let of_string = ir_of_string P.sb2
end

module MakeSB3Parser
  (N   : NUM)
  (M3  : MESH3 with type num  = N.t
                and type  pt  = N.t * N.t * N.t)
  (SB3 : SYNTHBENCH with type mesh = M3.t) :
sig
  val of_file   : string -> SB3.t
  val of_string : string -> SB3.t
end = struct
  module P = SynthBench3Parser.Make(N)(M3)(SB3)
  let of_file   = ir_of_file   P.sb3
  let of_string = ir_of_string P.sb3
end

