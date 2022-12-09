#!/usr/bin/env bash

set -e

# bench.sh [-k inputcol] [-b BACKEND[,BACKEND...]]  PROGRAM.fut
#
# A script to run benchmarks with several backends on a single futhark program
# and collects the results into a single csv file, sorted by backend.
#
# The input (x-axis/n) parameter in the resulting csv file is by default taken
# from the first input to the benchmark.  Ie. If we have a spec corresponding to
# `input {X Y Z}`, then `X` is taken. This can be changed with the `-k` argument,
# which is zero indexed.
#
# By default, the following backends are run:
BACKENDS=('c' 'ispc' 'multicore' 'opencl')

# Default values
INPUTCOL=0
FUTHARK_ARGS=()
SKIP_BENCHMARKS=""
SKIP_CSV=""
CSV=""

! [ -t 1 ] && QUIET="PIPE"

while [[ $# -gt 0 ]]; do
  case $1 in
    -k)
      INPUTCOL="${2}"
      shift;shift;;

    -b|--backend)
      IFS="," read -r -a BACKENDS <<< "${2}"
      shift;shift;;

    -o|--csv)
      CSV="${2}"
      shift;shift;;

    --skip-benchmarks)
      SKIP_BENCHMARKS="SKIP"
      shift;;

    --skip-csv)
      SKIP_CSV="SKIP"
      shift;;

    -q|--quiet)
      QUIET=STFU
      shift;;

    -v|--verbose)
      QUIET=""
      shift;;

    *.fut)
      FUTHARK_PROGRAM="${1}"
      shift;;

    --)
      shift
      while [[ $# -gt 0 ]]; do
        FUTHARK_ARGS+=("$1");shift
      done;;

    -h|--help)
      [ -n "${QUIET}" ] && exit 0
      echo -e "Usage: ${0} [OPTION] PROGRAM.fut [-- BENCH_OPTION]\n"
      BB="${BACKENDS[*]}"
      echo \
"Options:
  -k ARG              The benchmark entrys ith input argument to use for 'n' in
                      the output csv file.
  -b BACKEND(S)       A comma seperated list of backends to run the benchmark
                      on. Default is: \"${BB// /,}\"
  -o CSV              Name of the output csv file, printed to stdout otherwise.
  --skip-benchmarks   Skip running the actual benchmarks, useful if they have
                      already been run, but not formatted
  --skip-csv          Skip printing/writing the csv output.
  --quiet             Do not print anything.
  --verbose           Do not suppress output when piped.

  --                  all arguments after \"--\" is passed to "futhark-bench"
"
      exit 0
  esac
done

# Some stats regarding the benchmarking machine
CPUMODEL=$(lscpu | sed '/Model name/!d;s/^.*: *//')
NUMPROCS=$(nproc)

# Run the benchmarks
if [ -z "${SKIP_BENCHMARKS}" ]; then
  for b in "${BACKENDS[@]}"; do
    [ -z "${QUIET}" ] && echo "Running benchmarks with '$b' backend..."
    RESULTFILE=result-$b.bench
    echo "${CPUMODEL} with ${NUMPROCS} cores" > "$RESULTFILE"
    echo "Backend: $b" >> "$RESULTFILE"
    futhark bench --backend="$b" "${FUTHARK_ARGS[@]}" "${FUTHARK_PROGRAM}" >> "$RESULTFILE"
  done
fi

if [ -z "${SKIP_CSV}" ]; then
  ## Some common sed regular expressions/commands
  # Removes all lines not starting with '#'
  RE_RESULT_LINES='/^#/!d'
  # strip type suffixes from the input
  RE_TYPES='s/(i|f)(8|16|32|64)//g'
  # Retrieve the average runtime from the input
  RE_RESULTS='s/^.*\(".*"\): *([0-9]+)μs .*/\1/'
  # Retrieve the ${INPUTCOL}th line
  RE_N_COLUMN="s/.*\\(\"([0-9]+\\.?[0-9]+? |true |false ){${INPUTCOL}}([0-9]+\\.?[0-9]+?|true|false) ?.*\"\\).*/\\2/g"

  RESULT="n,$(sed -E "${RE_RESULT_LINES};${RE_TYPES};${RE_N_COLUMN}" "result-${BACKENDS[0]}.bench" | csvcut -K 0 | tr '\n' ',' | sed -E 's/,$//')"
  for b in "${BACKENDS[@]}"; do
    R=$(sed -E "${RE_RESULT_LINES};${RE_TYPES};${RE_RESULTS}" "result-${b}.bench" | tr '\n' ',' | sed -E 's/,$//')
    RESULT="${RESULT}\n${b},${R}"
  done

  transpose() {
    awk 'BEGIN { FS=OFS="," }
    {
      for (rowNr=1;rowNr<=NF;rowNr++) {
        cell[rowNr,NR] = $rowNr
      }
      maxRows = (NF > maxRows ? NF : maxRows)
      maxCols = NR
    } END {
    for (rowNr=1;rowNr<=maxRows;rowNr++) {
      for (colNr=1;colNr<=maxCols;colNr++) {
        printf "%s%s", cell[rowNr,colNr], (colNr < maxCols ? OFS : ORS)
    }}}' -
  }

  if [ -n "${CSV}" ]; then
    echo -en "${RESULT}" | transpose > "${CSV}"
  else
    echo -en "${RESULT}" | transpose
  fi
fi
