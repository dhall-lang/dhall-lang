{-  This test ensures that updates are ordered and latter updates take priority
    when the same field is updated more than one time

    In this example, the `a` field is updated twice, and the latter update wins
-}
{ a = 1 } with a = 2 with a = 3
