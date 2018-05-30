#load "unix.cma";;
#load "str.cma";;
#load "bigarray.cma";;
#load "gmp.cma";;
#load "hashcons.cmo";;
#load "zarith.cma";;

#load "Logging.cmo";;
#load "Util.cmo";;
#load "CTree.cmo";;
#load "RQueue.cmo";;
#load "RStream.cmo";;
#load "Graph.cmo";;

#load "ExactArith.cmo";;
#load "NumSys.cmo";;
#load "MPFRNumSys.cmo";;

#load "LinAlg.cmo";;
#load "Geom.cmo";;
#load "Mesh.cmo";;
#load "Primitives.cmo";;
#load "CAD.cmo";;
#load "LambdaCAD.cmo";;

#load "PropLogic.cmo";;

#load "ParseCommon.cmo";;
#load "RLexer.cmo";;
#load "ParserLib.cmo";;
#load "Mesh1Parser.cmo";;
#load "Mesh2Parser.cmo";;
#load "Mesh3Parser.cmo";;
#load "STLParser.cmo";;
#load "STLBinaryParser.cmo";;
#load "CAD1Parser.cmo";;
#load "CAD2Parser.cmo";;
#load "CAD3Parser.cmo";;
#load "LambdaCADParser.cmo";;
#load "SynthBench1Parser.cmo";;
#load "SynthBench2Parser.cmo";;
#load "SynthBench3Parser.cmo";;

#load "TableLexer.cmo";;
#load "TableParser.cmo";;

#load "Table.cmo";;

#load "RParse.cmo";;

#load "SynthBench.cmo";;
#load "Synth.cmo";;

#load_rec "Translator.cmo";;

open NumSys
open MPFRNumSys
open Geom
open Mesh
open CAD
open LambdaCAD

module N = FloatNum

module LA = LinAlg.Matrix(N)

module G1 = Geom1(N)
module M1 = Mesh1(N)(G1)
module C1 = CAD1(N)(M1)

module G2 = Geom2(N)
module M2 = Mesh2(N)(G2)
module C2 = CAD2(N)(M2)

module G3 = Geom3(N)(LA)
module M3 = Mesh3(N)(G3)
module C3 = CAD3(N)(M3)

module LC = LambdaCAD.Make(N)(C1)(C2)(C3)

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx


