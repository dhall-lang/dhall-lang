{- This test verifies that an implementation correctly permits both the
   "replacement" and the "haystack" to be abstract.
-}
λ(x : Text) → λ(y : Text) → Text/replace "a" "-${x}-" "_a_${y}_a_"
