{- This test caught a regression in the Haskell implementation's typechecker.

   The typechecker was not sorting the fields of a record type inferred from a
   projection, meaning that it inferred that the fully normalized type of
   the subexpression `{ bar = "", foo = "" }.{ foo, bar }` was
   `{ foo : Text, bar : Text }` when according to the standard the inferred type
   should have been `{ bar : Text, foo : Text }`.  On top of that the Haskell
   implementation was sensitive to field order, leading to a type mismatch
   when applying a function whose input type had the fields sorted.

   This test ensures that implementations correctly handle order-insensitivity
   when projecting fields and/or comparing types, just in case they are
   order-sensitive like the Haskell implementation.
-}
λ(f : { bar : Text, foo : Text } → Bool) → f { bar = "", foo = "" }.{ foo, bar }
