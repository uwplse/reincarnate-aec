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

printf "COMPILER TESTS\n"

echo "unit cube"
./Main.native --src aec/cads-to-compile/cad3/unit.cad3 --tgt aec/compiled-meshes/mesh3/unit.mesh3
./Main.native --src aec/compiled-meshes/mesh3/unit.mesh3 --tgt aec/compiled-meshes/stl/unit.stl
printf "${PASS}\n\n"

echo "hexagonal prism"
./Main.native --src aec/cads-to-compile/cad3/hexagon.cad3 --tgt aec/compiled-meshes/mesh3/hexagon.mesh3
./Main.native --src aec/compiled-meshes/mesh3/hexagon.mesh3 --tgt aec/compiled-meshes/stl/hexagon.stl
printf "${PASS}\n\n"

echo "affine transformations and binops"
./Main.native --src aec/cads-to-compile/cad3/affine1.cad3 --tgt aec/compiled-meshes/mesh3/affine1.mesh3
./Main.native --src aec/compiled-meshes/mesh3/affine1.mesh3 --tgt aec/compiled-meshes/stl/affine1.stl
printf "${PASS}\n\n"

echo "combining affine transformations"
./Main.native --src aec/cads-to-compile/cad3/affine2.cad3 --tgt aec/compiled-meshes/mesh3/affine2.mesh3
./Main.native --src aec/compiled-meshes/mesh3/affine2.mesh3 --tgt aec/compiled-meshes/stl/affine2.stl
printf "${PASS}\n\n"

echo "combining binops"
./Main.native --src aec/cads-to-compile/cad3/cube-2hole.cad3 --tgt aec/compiled-meshes/mesh3/cube-2hole.mesh3
./Main.native --src aec/compiled-meshes/mesh3/cube-2hole.mesh3 --tgt aec/compiled-meshes/stl/cube-2hole.stl
printf "${PASS}\n\n"

printf "ALL DONE\n"
