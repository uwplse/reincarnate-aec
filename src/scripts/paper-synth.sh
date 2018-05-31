./Main.native --src aec/paper-synth/candle.mesh3 --tgt aec/synthed-cads/cad3/candle.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/candle.cad3 --tgt aec/synthed-cads/scad/candle.scad

./Main.native --src aec/paper-synth/icfp_i.mesh3 --tgt aec/synthed-cads/cad3/icfp_i.cad3 --no-invariants --glue os-mesh --fuel 5
./Main.native --src aec/synthed-cads/cad3/icfp_i.cad3 --tgt aec/synthed-cads/scad/icfp_i.scad

./Main.native --src aec/paper-synth/icfp_c.mesh3 --tgt aec/synthed-cads/cad3/icfp_c.cad3 --no-invariants --glue os-mesh
./Main.native --src aec/synthed-cads/cad3/icfp_c.cad3 --tgt aec/synthed-cads/scad/icfp_c.scad

./Main.native --src aec/paper-synth/icfp_f.mesh3 --tgt aec/synthed-cads/cad3/icfp_f.cad3 --no-invariants --glue os-meshi --fuel 5
./Main.native --src aec/synthed-cads/cad3/icfp_f.cad3 --tgt aec/synthed-cads/scad/icfp_f.scad

./Main.native --src aec/paper-synth/icfp_p.mesh3 --tgt aec/synthed-cads/cad3/icfp_p.cad3 --no-invariants --glue os-mesh --fuel 5
./Main.native --src aec/synthed-cads/cad3/icfp_p.cad3 --tgt aec/synthed-cads/scad/icfp_p.scad


./Main.native --src aec/paper-synth/cfp.mesh3 --tgt aec/synthed-cads/cad3/cfp.cad3 --no-invariants --glue os-mesh --fuel 10
./Main.native --src aec/synthed-cads/cad3/cfp.cad3 --tgt aec/synthed-cads/scad/cfp.scad

./Main.native --src aec/paper-synth/hexholder.mesh3 --tgt aec/synthed-cads/cad3/hexholder.cad3 --no-invariants --glue os-mesh --fuel 12
./Main.native --src aec/synthed-cads/cad3/hexholder.cad3 --tgt aec/synthed-cads/scad/hexholder.scad


