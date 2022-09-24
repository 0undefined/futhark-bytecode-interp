# Parallel bytecode interpretation in Futhark

Parallel bytecode interpretation on a General Purpose Graphical
Processing Unit (GPGPU) can have general useful properties for fast
"read-eval-print-loop" workflows that utilize multivariable arithmetic functions,
such as contract price estimations in the financial sector. It eliminates the
need to recompile a program each time small changes are change made and enables
fast iteration on such functions with multivariable inputs for users.

In this project I will explore, compare, and discuss various features of a
parallel bytecode interpreter, such as branching execution paths and loops, and
employ different techniques to reduce the performance implications that comes
with them. Such techniques include program transformations like if-then-else
flattening as described in the "Programming Massively Parallel Hardware" course
notes, and sorting threads by their respective branch taken to reduce thread
divergence.

I will also compare various configurations of such virtual machine, by
implementing stack memory, registers, and a hybrid implementation, all of which
will be compared performance wise against a native Futhark implementation.
