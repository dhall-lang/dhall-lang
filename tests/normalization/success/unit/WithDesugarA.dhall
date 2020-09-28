{- This test verifies that an implementation no longer implements the old desugaring (pre-v19.0.0)
-}
\(x: { a : { b : { c : {} } }}) -> x with a.b.c = 42
