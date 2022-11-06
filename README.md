# Parallel bytecode interpretation in Futhark

Parallel bytecode interpretation on General Purpose Graphical Processing Units
can be attractive for multiple use case scenarios, including pricing estimations
of user-defined financial contracts and simulations of user-defined scientific
models. In contrast to applying a full compilation strategy for the execution of
user-defined code, using a bytecode execution strategy has the benefit of not
having to recompile a program each time changes are made to the user-defined
code. This workflow enables fast user-iteration workflows with the cost of some
execution overhead.

In this project I will explore, compare, and discuss various features of a
parallel bytecode interpreter written in a pure language, such as branching
execution paths and loops, as well as employ different techniques to reduce the
performance implications accompanies them.

Such methods include program transformations like if-then-else flattening as
described in the "Programming Massively Parallel Hardware" course notes, and
sorting threads by their respective branch chosen to reduce thread divergence.
