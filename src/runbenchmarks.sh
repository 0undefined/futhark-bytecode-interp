#!/usr/bin/env sh

BACKEND=opencl
FILE=bench_simple.fut

#IFS=$'\n' read -r -d '' -a SIMPLE_ENTRIES < <( sed -E '/^entry/!d;s/^entry ([^ ]+) .*/\1/g' "${FILE}" && printf '\0' )

SIMPLE_ENTRIES=( $(sed -E '/^entry/!d;s/^entry ([^ ]+) .*/\1/g' "${FILE}") )

NUM_ENTRIES=$((${#SIMPLE_ENTRIES[@]}))
for i in "${!SIMPLE_ENTRIES[@]}"; do
  s=${SIMPLE_ENTRIES[$i]}
  printf "[% ${#NUM_ENTRIES}i/$NUM_ENTRIES] $s              \r" "$((${i} + 1))"
  ./sneaker -q --skip-csv --backend $BACKEND "${FILE}" -- -e $s
  mv result-$BACKEND.bench result-${s}.bench
done

./sneaker --skip-benchmarks -r --backend $(echo -n "${SIMPLE_ENTRIES[*]}" | tr ' ' ',') "${FILE}"
