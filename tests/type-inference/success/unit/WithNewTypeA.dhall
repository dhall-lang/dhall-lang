{- This test illustrates that the `with` keyword is permitted to override
   existing fields with a value of a new type (just like the `//` operator)
-}
{ a = 1 } with a = True
