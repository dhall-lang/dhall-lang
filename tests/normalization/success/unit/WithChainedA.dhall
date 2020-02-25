{-  This test verifies that an implementation correctly handles chained
    `with` expressions

    Note that this test has the same result as the `WithMultipleUpdates` test
-}
{ a = 1 } with { b = 2 } with { c = 3 }
