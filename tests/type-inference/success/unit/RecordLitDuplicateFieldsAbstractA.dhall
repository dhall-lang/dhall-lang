{-  This test illustrates that duplicate fields need not be literals in order
    to be properly normalized.  One or both of the duplicate fields can be
    abstract because field duplication delegates its behavior to the ∧ operator

    This particular example succeeds because it desugars to:

        λ(r : { z : Natural }) → { x = { y = 1 } ∧ r }

    ... and the `∧` operator can infer the absence of conflicts due to knowing
    the type of `r`
-}
λ(r : { z : Natural }) → { x = { y = 1 }, x = r }
