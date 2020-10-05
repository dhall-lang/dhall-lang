{- This test verifies that an implementation will evaluate a
partially abstract `with` expression correctly
-}
λ(x : { b : { c : {} } }) → { a = x } with a.b.c = 42
