{- The purpose of this test is to demonstrate that chained `with`
   expressions are the same as a single `with` expression with multiple
   updates

   Compare to the `WithMultiple` test in this same folder which encodes to the
   same value
-}
{ a.b = 1, c.d = 2 } with { a.b = 3 } with { c.e = 4 }
