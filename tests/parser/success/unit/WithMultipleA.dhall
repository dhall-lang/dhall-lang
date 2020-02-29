{- This test ensures that `with` is purely syntactic sugar, meaning that it is
   desugared before encoding and therefore is encoded as the desugared expression
   in the CBOR encoding.

   This test also ensures that implementations desugar the code exactly as
   specified (e.g. using an intermediate `let` binding)
-}
{ a.b = 1, c.d = 2 } with a.b = 3 with c.e = 4
