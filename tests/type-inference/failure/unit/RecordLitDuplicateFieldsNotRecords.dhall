{-  This fails because the expression desugars to:

    { x = 0 ∧ 0 }

    ... which is ill-typed due to ∧ only working on records
-}
{ x = 0, x = 0 }
