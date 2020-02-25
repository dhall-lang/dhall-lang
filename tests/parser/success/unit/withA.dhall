{- This test ensures that `with` is purely syntactic sugar, meaning that it is
   desugared before encoding and therefore is encoded as the desugared expression
   in the CBOR encoding.
-}
{ x = 1 } with { x = 2 }
