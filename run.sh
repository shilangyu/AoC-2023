#!/bin/bash
set -e

ALL_DAYS=( ["1"]="lean" ["2"]="koka" )

args=("$@")
DAYS=("${args[@]:-${ALL_DAYS[@]}}")

for day in "${DAYS[@]}"; do
	printf -v padded "%02d" $day
	
	case "${ALL_DAYS[$day]}" in
  "lean")
    lean src/day$padded.lean --run
    ;;

  "koka")
    koka --execute --verbose=0 src/day$padded.kk
    ;;

  *)
		echo "unknown language"
    exit 1
    ;;
	esac
done
