{- This test verifies that an implementation correctly encodes chained
   `with` expressions in a left-associative way.
-}
{ a.b = 1, c.d = 2 } with a.b = 3 with c.e = 4
