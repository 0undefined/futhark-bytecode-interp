# Implementing a bytecode interpreter in Futhark

Main focus points:
* Eliminate the need to re-compile
* Dynamically re-calculate sequence


Implementing a bytecode interpreter in Futhark for fast data parallel execution
of arbitrary arithmetic functions during runtime can significantly improve the
the work flow when testing chaos theory.
  Without runtime interpretation of arithmetic functions for data parallel
execution one has to re-compile the whole program in order to check the results,
which can be a bottleneck in the workflow when tuning parameters and testing
hypothesis.

By implementing a bytecode interpreter in futhark, to run across the parallel
execution processors one can benefit from the flexibility of interpreting the
arithmetic functions during runtime at the cost of branching execution paths
when interpreting the arbitrary functions, which can pose a performance problem
if the futhark backend runs in lockstep, eg. on a GPU.

In this project, I will explore, compare, and discuss various features of a
bytecode interpreter along with the performance implications of such features.
These features include conditional branching and loops. The discussion will
cover using the different features for solving various kinds of problems.
