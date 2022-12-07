#!/usr/bin/env sh

BACKENDS=('c' 'multicore' 'ispc' 'opencl')

CPUMODEL=$(lscpu | sed '/Model name/!d;s/^.*: *//')

for b in "${BACKENDS[@]}"; do
  echo "Running benchmarks with '$b' backend..."
  RESULTFILE=result-$b.bench
  echo "${CPUMODEL}" > $RESULTFILE
  echo "Backend: $b" >> $RESULTFILE
  futhark bench --backend=$b main.fut >> $RESULTFILE
done
