#!/usr/bin/env sh

BACKEND=opencl
FILE=bench_simple.fut

SIMPLE_ENTRIES=( $(sed -E '/^entry/!d;s/^entry ([^ ]+) .*/\1/g' "${FILE}") )
BENCHMARK_DIR=${BENCHMARK_DIR:-.benchmarks}

NUM_ENTRIES=$((${#SIMPLE_ENTRIES[@]}))
LAST_PRINTLEN=$(( 0 ))

! [ -d "${BENCHMARK_DIR}" ] && mkdir -p ${BENCHMARK_DIR}

for i in "${!SIMPLE_ENTRIES[@]}"; do
  s=${SIMPLE_ENTRIES[$i]}

  # Clear the line by writing spaces
  printf "%-${LAST_PRINTLEN}s\r" ""
  STATUS=$(printf "[% ${#NUM_ENTRIES}i/$NUM_ENTRIES] $s\r" "$((${i} + 1))")
  LAST_PRINTLEN=${#STATUS}
  # Give the status counter
  echo -n "$STATUS"

  # Run the Benchmark
  ./sneaker -q --skip-csv --backend $BACKEND "${FILE}" -- -e $s
  # Save the benchmark
  mv ${BENCHMARK_DIR}/result-$BACKEND.bench ${BENCHMARK_DIR}/result-${s}.bench
done
echo ""

./sneaker --skip-benchmarks -r --backend $(echo -n "${SIMPLE_ENTRIES[*]}" | tr ' ' ',') "${FILE}" | tee benchmark.csv
