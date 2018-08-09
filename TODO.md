1. Implement LPO ordering on sketches
1. Properly score folds
1. ~~Print sketches as executable functions~~
1. Rather than implement euclidean div/mod in Racket, directly use SMT2's div/mod
1. Allow fixed constants (as opposed to wild constants)
1. Prune dead instructions from synthesized functions
1. Test case from the gitter channel. Rewrite rule `x * 2 / 2 -> x` exists, question of whether `(x * (-2)) / 2 -> x * (-1)` was valid. What is the more general rule that covers those cases?
