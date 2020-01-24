{-  This test illustrates that duplicate fields need not be literals in order
    to be properly normalized.  One or both of the duplicate fields can be
    abstract because field duplication delegates its behavior to the `∧`
    operator

    This particular example fails because it desugars to:

        λ(r : { y : Natural }) → { x = { y = 1 } ∧ r }

    ... and the `∧` operator can infer that the two records collide on the
    field `y`
-}
λ(r : { y : Natural }) → { x = { y = 1 }, x = r }
