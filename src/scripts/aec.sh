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

function usage {
  echo "
$(basename $0) -h -c -s -l IR

Run regression tests.

OPTIONS:

  -h      print this usage information and exit
  -c      run only compilation tests
  -s      run only synthesis tests
  -l IR   run only tests starting at IR in {cad, mesh}
"
}

RARGS=""
COMPILE=true
SYNTH=true
IR=""
TOK="$(mktemp "$RROOT/tmp/rcheck.XXXXX")"

# only color output for ttys
PASS="PASS"
FAIL="FAIL"
if [ -t 1 ]; then
  PASS="${GRN}PASS${CLR}"
  FAIL="${RED}FAIL${CLR}"
fi

function parse_args {
  while getopts ":h:csl:" OPT; do
    case "$OPT" in
      "h") usage; exit 0     ;;
      "c") SYNTH=false       ;;
      "s") COMPILE=false     ;;
      "l") IR=$OPTARG        ;;
      ":") usage_error "-$OPTARG requires an argument" ;;
       * ) usage_error "bogus option '-$OPTARG'"       ;;
    esac
  done

  # sanity check args
  case "$IR" in
    ""|cad|mesh) ;;
    *) usage_error "bogus IR '$IR'" ;;
  esac

  # prevent changing arg globals + share with subshells
  readonly RARGS COMPILE SYNTH IRTOK PASS FAIL
  export   RARGS COMPILE SYNTH IRTOK PASS FAIL
}

function main {
  parse_args "$@"
  cd "$RROOT"

  local tick="$($DATE "+%s%N")"
  plan
  local tock="$($DATE "+%s%N")"
  local wallt="$(bc -l <<< "($tock - $tick) / 10^9")"

  local tmp="$(mktemp rc-XXXXX)"
  local tots="$(summarize "$tmp" "$wallt")"
  echo "$tots"
}

function plan {
  echo "echo 'Regression Tests:'"
  echo "echo"
  if $COMPILE; then
    if lang_enabled cad; then
      planner cad3 mesh3
    fi
  fi
  if $SYNTH; then
    if lang_enabled mesh; then
      planner mesh3 cad3
    fi
  fi
}

function lang_enabled {
  [ "$IR" = "" -o "$IR" = "$1" ]
}

function planner {
  src_ir="$1"
  tgt_ir="$2"

  echo "echo '$src_ir ==> $tgt_ir'"
  for f in $RROOT/aec/$src_ir/*; do
    echo "do_test '$src_ir' '$tgt_ir' '$f'"
  done
  echo "echo"
}

function do_test {
  local src_ir="$1"
  local tgt_ir="$2"
  local src="$3"

  local name="$(basename "$src" ".$src_ir")"
  local goal="$name.$src_ir.$tgt_ir"
  local tgt="$RROOT/tmp/$goal"
  local tstats="$tgt.tstats"
  local gh="$tgt.gh"
  local out="$tgt.out"

  local ok="$?"
  local utime="$(get_tm_field "$tstats" "User time")"
  local stime="$(get_tm_field "$tstats" "System time")"
  local memkb="$(get_tm_field "$tstats" "Maximum resident set size")"

  local ttime="$(bc -l <<< "$utime + $stime")"
  local memmb="$(bc -l <<< "$memkb / 1000")"

  # append input and tstats as comments to output
  { echo                    \
  ; echo                    \
  ; sed 's/^/# /' "$src"    \
  ; echo                    \
  ; echo                    \
  ; sed 's/^/# /' "$tstats" \
  ; echo                    \
  ; } >> "$tgt"

  # report to stdout
  local res=""
  [ $ok -eq 0 ]    \
    && res="$PASS" \
    || res="$FAIL"
  printf "%25s : %s %7.2f s\n" \
    "$name" "$res" "$ttime"

  # record for gather to csv
  [ $ok -eq 0 ] \
    && res="1"  \
    || res="0"
  printf "%s,%d,%0.2f,%0.2f\n"       \
    "$goal" "$res" "$ttime" "$memmb" \
    > "$TOK.$goal.csv"
}
export -f do_test

function summarize {
  local csv="$1"
  local wallt="$2"

  local npass="$(csv_sum "$csv" 2)"
  local nfail="$(($(csv_len "$csv") - $npass))"
  local ttime="$(csv_sum "$csv" 3)"

  printf "Pass: %d\n"   "$npass"
  printf "Fail: %d\n"   "$nfail"
  printf "Time: %.2f\n" "$ttime"
  printf "Wall: %.2f\n" "$wallt"
}

main "$@"
