# Implementing a bytecode interpreter in Futhark

Implementing a bytecode interpreter in Futhark for fast data parallel execution
of arbitrary airthmetic functions during runtime can significantly improve the
user experience.
  Currently, if someone needs to run arbitrary arithmetic functions across a
large dataset, one needs to re-compile the futhark project to an executable
binary which can be a bottleneck in the workflow and is unintuitive for anyone
without a background in coding.

By implementing a bytecode interpreter in futhark, to run across the parallel
execution processors one can benefit from the flexibility of interpreting the
arithmetic functions during runtime at the cost of branching execution paths
when interpreting the arbitrary functions, which can pose a performance problem
if the compiled futhark backend runs in lockstep, eg. on a GPU.
