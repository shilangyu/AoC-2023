#!/bin/bash
set -e

ALL_DAYS=(
  ["1"]="koka" ["2"]="koka" ["3"]="lean"
  ["4"]="lean" ["5"]="koka" ["6"]="lean"
  ["7"]="lean" ["8"]="koka" ["9"]="lean"
  ["11"]="lean" ["12"]="koka" ["13"]="lean"
  ["14"]="lean" ["15"]="koka"
)

args=("$@")
DAYS=("${args[@]:-${!ALL_DAYS[@]}}")

for day in "${DAYS[@]}"; do
  printf -v padded "%02d" $day
  echo ""
  echo "Day $day:"

  case "${ALL_DAYS[$day]}" in
  "lean")
    lake env lean --run src/day$padded.lean
    ;;

  "koka")
    # TODO: koka is outputing some garbage
    koka -i./src/std --execute --verbose=0 src/day$padded.kk
    ;;

  *)
    echo "unknown language"
    exit 1
    ;;
  esac
done
