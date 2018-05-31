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
$(basename $0) -h -a ARG -c -s -l IR -d N -j N -w CSV -g HG

Run regression tests.

OPTIONS:

  -h      print this usage information and exit
  -a ARG  append ARG to arguments passed to Reincarnate
  -c      run only compilation tests
  -s      run only synthesis tests
  -l IR   run only tests starting at IR in {lc, cad, mesh}
  -d N    run only tests for dimension N in {1, 2, 3}
  -j N    number of concurrent jobs (default 1)
  -w CSV  write results summary to CSV
  -g HG   run Reincarnate under Herbgrind executable HG
"
}

RARGS=""
COMPILE=true
SYNTH=true
IR=""
DIM=""
JOBS=1
CSV=""
HERBGRIND=""
TOK="$(mktemp "$RROOT/tmp/rcheck.XXXXX")"

# only color output for ttys
PASS="PASS"
FAIL="FAIL"
if [ -t 1 ]; then
  PASS="${GRN}PASS${CLR}"
  FAIL="${RED}FAIL${CLR}"
fi

function parse_args {
  while getopts ":ha:csl:d:j:w:g:" OPT; do
    case "$OPT" in
      "h") usage; exit 0     ;;
      "c") SYNTH=false       ;;
      "s") COMPILE=false     ;;
      "l") IR=$OPTARG        ;;
      "d") DIM=$OPTARG       ;;
      "j") JOBS=$OPTARG      ;;
      "w") CSV=$OPTARG       ;;
      "g") HERBGRIND=$OPTARG ;;
      "a") RARGS="$RARGS $OPTARG" ;;
      ":") usage_error "-$OPTARG requires an argument" ;;
       * ) usage_error "bogus option '-$OPTARG'"       ;;
    esac
  done

  if [ "$HERBGRIND" != "" ] && [ ! -x "$HERBGRIND" ]; then
    error "Herbgrind not executable: '$HERBGRIND'"
  fi

  # sanity check args
  case "$IR" in
    ""|lc|cad|mesh) ;;
    *) usage_error "bogus IR '$IR'" ;;
  esac
  case "$DIM" in
    ""|1|2|3) ;;
    *) usage_error "bogus DIM '$DIM'" ;;
  esac
  assert_nonnegi "jobs" "$JOBS"

  # if set and relative, remember absolute CSV path
  [ "$CSV" != "" ] && [ "$CSV" = "${CSV#/}" ] && \
    CSV="$(pwd)/$CSV"

  # prevent changing arg globals + share with subshells
  readonly RARGS COMPILE SYNTH IR DIM JOBS TOK PASS FAIL CSV HERBGRIND
  export   RARGS COMPILE SYNTH IR DIM JOBS TOK PASS FAIL CSV HERBGRIND
}

function main {
  parse_args "$@"
  cd "$RROOT"

  local tick="$($DATE "+%s%N")"
  plan | parallel --keep-order --jobs "$JOBS"
  local tock="$($DATE "+%s%N")"
  local wallt="$(bc -l <<< "($tock - $tick) / 10^9")"

  local tmp="$(mktemp rc-XXXXX)"
  gather > "$tmp"
  local tots="$(summarize "$tmp" "$wallt")"
  if [ "$CSV" != "" ]; then
    mv "$tmp" "$CSV"
    provenance "$CSV" "$tots"
  else
    rm "$tmp"
  fi
  echo "$tots"
}

function plan {
  echo "echo 'Regression Tests:'"
  echo "echo"
  if $COMPILE; then
    if lang_enabled lc; then
      dim_enabled 1 && \
        planner lc1 mesh1
      dim_enabled 2 && \
        planner lc2 mesh2
      dim_enabled 3 && \
        planner lc3 mesh3
    fi
    if lang_enabled cad; then
      dim_enabled 1 && \
        planner cad1 mesh1
      dim_enabled 2 && \
        planner cad2 mesh2
      dim_enabled 3 && \
        planner cad3 mesh3
    fi
  fi
  if $SYNTH; then
    if lang_enabled mesh; then
      dim_enabled 1 && \
        planner mesh1 cad1
      dim_enabled 2 && \
        planner mesh2 cad2
      dim_enabled 3 && \
        planner mesh3 cad3
    fi
  fi
}

function lang_enabled {
  [ "$IR" = "" -o "$IR" = "$1" ]
}

function dim_enabled {
  [ "$DIM" = "" -o "$DIM" = "$1" ]
}

function planner {
  src_ir="$1"
  tgt_ir="$2"

  echo "echo '$src_ir ==> $tgt_ir'"
  for f in $RROOT/test/$src_ir/*; do
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

  if [ "$HERBGRIND" != "" ]; then
    $TIME -v -o "$tstats" \
      "$HERBGRIND" --outfile="$gh"  \
        $RROOT/Main.native $RARGS   \
          --src "$src" --tgt "$tgt" \
          > "$out" 2>&1
  else
    $TIME -v -o "$tstats" \
      $RROOT/Main.native $RARGS   \
        --src "$src" --tgt "$tgt" \
        > "$out" 2>&1
  fi
  local ok="$?"
  local utime="$(get_tm_field "$tstats" "User time")"
  local stime="$(get_tm_field "$tstats" "System time")"
  local memkb="$(get_tm_field "$tstats" "Maximum resident set size")"

  local ttime="$(bc -l <<< "$utime + $stime")"
  local memmb="$(bc -l <<< "$memkb / 1000")"

  # timing, so provenance separately
  if [ "$HERBGRIND" != "" ]; then
    if [ ! -s "$gh" ]; then
      echo "No Herbgrind output produced." > "$gh"
      ok=1
    fi
    provenance "$gh" "$(cat "$tstats")"
  fi

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

function gather {
  # add csv header (required for csv sort)
  echo "test,pass,time,mem" \
    | cat - $TOK.*.csv  \
    | csv_sort 3

  # clean up
  rm $TOK*
}

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
