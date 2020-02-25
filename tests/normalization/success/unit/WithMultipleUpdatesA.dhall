{-  Note that this test has the same result as the `WithChained` test, in order
    to test that multiple updates give the same behavior as chained `with`
    expressions
-}
{ x = 1 } with { y = 2, z = 3 }
