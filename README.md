This is a Lua implementation of a binary decision diagram library, as
described in Henrik Reif Andersen's "An Introduction to Binary Decision
Diagrams". For usage examples, look at the test suite (which depends on lunatest).

The code is written for clarity rather than performance (though it does
find all solutions for 10-queens within a couple seconds). I've been
studying BDDs and their application to binary constraint solvers,
Datalog, and the like. Perhaps it will be helpful to someone else.

If you want more info about BDDs, there's lots of info in Knuth's _The
Art of Computer Programming_ vol. 4A, as well as some coverage in Aho,
Sethi, and Ullman's _Compilers: Principles, Techniques, and Tools_ 2nd
ed.
