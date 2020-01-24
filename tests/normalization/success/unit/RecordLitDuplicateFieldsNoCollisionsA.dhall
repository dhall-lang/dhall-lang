{-  This expression desugars to:

        { x = { y = 0 } ∧ { z = 0 } }

    ... which then β-normalizes to:

        { x = { y = 0, z = 0 } }
-}
{ x = { y = 0 }, x = { z = 0 } }
