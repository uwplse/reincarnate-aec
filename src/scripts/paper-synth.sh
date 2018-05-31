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

printf "SYNTHESIS TESTS FROM THE PAPER\n"

echo "Candle holder"
./Main.native --src aec/paper-synth/candle.mesh3 --tgt aec/synthed-cads/cad3/candle.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/candle.cad3 --tgt aec/synthed-cads/scad/candle.scad
printf "${PASS}\n\n"

echo "ICFP-I"
./Main.native --src aec/paper-synth/icfp_i.mesh3 --tgt aec/synthed-cads/cad3/icfp_i.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/icfp_i.cad3 --tgt aec/synthed-cads/scad/icfp_i.scad
printf "${PASS}\n\n"

echo "ICFP-C"
./Main.native --src aec/paper-synth/icfp_c.mesh3 --tgt aec/synthed-cads/cad3/icfp_c.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/icfp_c.cad3 --tgt aec/synthed-cads/scad/icfp_c.scad
printf "${PASS}\n\n"

echo "ICFP-F"
./Main.native --src aec/paper-synth/icfp_f.mesh3 --tgt aec/synthed-cads/cad3/icfp_f.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/icfp_f.cad3 --tgt aec/synthed-cads/scad/icfp_f.scad
printf "${PASS}\n\n"

echo "ICFP-P"
./Main.native --src aec/paper-synth/icfp_p.mesh3 --tgt aec/synthed-cads/cad3/icfp_p.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/icfp_p.cad3 --tgt aec/synthed-cads/scad/icfp_p.scad
printf "${PASS}\n\n"

echo "CFP"
./Main.native --src aec/paper-synth/cfp.mesh3 --tgt aec/synthed-cads/cad3/cfp.cad3 --no-invariants --glue os-mesh --fuel 10
./Main.native --src aec/synthed-cads/cad3/cfp.cad3 --tgt aec/synthed-cads/scad/cfp.scad
printf "${PASS}\n\n"

echo "Hexholder"
./Main.native --src aec/paper-synth/hexholder.mesh3 --tgt aec/synthed-cads/cad3/hexholder.cad3 --no-invariants --glue os-mesh --fuel 12
./Main.native --src aec/synthed-cads/cad3/hexholder.cad3 --tgt aec/synthed-cads/scad/hexholder.scad
printf "${PASS}\n\n"


printf "ALL DONE\n"
