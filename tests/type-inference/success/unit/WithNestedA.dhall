{- This test illustrates that the `with` keyword is permitted to override
   existing fields nested within a child record
-}
{ a = { p = 2, q = "hi" }, b = 5.6 } with a.p = True
