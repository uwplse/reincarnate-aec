#!/usr/bin/env bash

# exit on error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# include reincarnate bash library
source "$MYDIR/include.sh"

# ensure output dirs exist
mkdir -p aec/synthed-cads/cad3
mkdir -p aec/synthed-cads/scad

printf "BASIC SYNTHESIS TESTS\n"

echo "unit cube"
./Main.native --src aec/basic-synth/unit.mesh3 --tgt aec/synthed-cads/cad3/unit.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/unit.cad3 --tgt aec/synthed-cads/scad/unit.scad
printf "${PASS}\n\n"

echo "hexagonal prism"
./Main.native --src aec/basic-synth/hexagon.mesh3 --tgt aec/synthed-cads/cad3/hexagon.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/hexagon.cad3 --tgt aec/synthed-cads/scad/hexagon.scad
printf "${PASS}\n\n"

echo "affine transformations and binops"
./Main.native --src aec/basic-synth/affine1.mesh3 --tgt aec/synthed-cads/cad3/affine1.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/affine1.cad3 --tgt aec/synthed-cads/scad/affine1.scad
printf "${PASS}\n\n"

echo "combining affine transformations"
./Main.native --src aec/basic-synth/affine2.mesh3 --tgt aec/synthed-cads/cad3/affine2.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/affine2.cad3 --tgt aec/synthed-cads/scad/affine2.scad
printf "${PASS}\n\n"

echo "combining binops"
./Main.native --src aec/basic-synth/cube-2hole.mesh3 --tgt aec/synthed-cads/cad3/cube-2hole.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/cube-2hole.cad3 --tgt aec/synthed-cads/scad/cube-2hole.scad
printf "${PASS}\n\n"

printf "ALL DONE\n"
