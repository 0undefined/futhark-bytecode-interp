#!/usr/bin/env sh

# Automatically run `sneaker` on all entries in $FILE and collect them in a csv
# file.

# Parameters
BACKEND=opencl
FILE=bench_sorting.fut
BENCHMARK_DIR=${BENCHMARK_DIR:-.benchmarks}
! [ -d "${BENCHMARK_DIR}" ] && mkdir -p ${BENCHMARK_DIR}

# Get entry names
SIMPLE_ENTRIES=( $(sed -E '/^entry/!d;/rand_10/d;s/^entry ([^ ]+) .*/\1/g' "${FILE}") )

NUM_ENTRIES=$((${#SIMPLE_ENTRIES[@]}))

# Check if it can even be compiled
futhark check $FILE || exit 1

LAST_PRINTLEN=$(( 0 ))
for i in "${!SIMPLE_ENTRIES[@]}"; do
  s=${SIMPLE_ENTRIES[$i]}

  STATUS=$(printf "[%*i/$NUM_ENTRIES] $s\r" "${#NUM_ENTRIES}" "$((${i} + 1))")
  # Clear the line by writing spaces
  printf "%-${LAST_PRINTLEN}s\r" ""
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
