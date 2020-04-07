{-  The Haskell implementation was incorrectly normalizing a partially
    saturated `Natural/fold` even though the standard requires built-ins to be
    fully saturated before normalization.

    For example, the Haskell implementation was normalizing the following
    expression to:

          λ(natural : Type)
        → λ(succ : natural → natural)
        → λ(zero : natural)
        → zero

    ... when the correct result should have been `Natural/fold 0` (no change)
-}
Natural/fold 0
