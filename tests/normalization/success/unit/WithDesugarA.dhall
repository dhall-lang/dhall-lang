{- This test verifies that an implementation desugars `with` correctly by
   leaving `x` abstract
-}
\(x: { a : { b : { c : {} } }}) -> x with a.b.c = 42
