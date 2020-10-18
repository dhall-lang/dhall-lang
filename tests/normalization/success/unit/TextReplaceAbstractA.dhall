{- This test verifies that an implementation does not beta normalize with an
   abstract "haystack".
-}
λ(x : Text) → λ(y : Text) → Text/replace "a" "-${x}-" "_a_${y}_a_"
