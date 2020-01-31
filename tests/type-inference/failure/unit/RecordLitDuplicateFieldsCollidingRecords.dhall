{-  This fails because the expression desugars to:

        { x = { y = 0 } ∧ { y = 0 } }

    ... which is ill-typed due to `∧` forbidding collisions and the two `y`
    fields collide.
-}
{ x = { y = 0 }, x = { y = 0 } }
