#!/bin/bash
set -e

ALL_DAYS=( ["1"]="koka" )

args=("$@")
DAYS=("${args[@]:-${!ALL_DAYS[@]}}")

for day in "${DAYS[@]}"; do
  printf -v padded "%02d" $day
  
  case "${ALL_DAYS[$day]}" in
  "lean")
    echo ""
    echo "Day $day:"
    lean src/day$padded.lean --run
    ;;

  "koka")
    echo ""
    printf "Day $day:"
    koka --execute --verbose=0 src/day$padded.kk
    ;;

  *)
    echo "unknown language"
    exit 1
    ;;
  esac
done
