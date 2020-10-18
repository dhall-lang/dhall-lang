{- This test verifies that an implementation works with an abstract "replacement".
-}
λ(x : Text) → Text/replace "a" "-${x}-" "_a__a_"
