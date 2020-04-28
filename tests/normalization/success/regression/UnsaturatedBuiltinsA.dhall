{-  The Haskell implementation was incorrectly normalizing partially
    saturated `Natural/fold` and `List/fold` built-ins even though the standard
    requires built-ins to be fully saturated before normalization.

    For example, the Haskell implementation was normalizing `Natural/fold 0` to:

          λ(natural : Type)
        → λ(succ : natural → natural)
        → λ(zero : natural)
        → zero

    ... when the correct result should have been `Natural/fold 0` (no change)
-}
{ example0 = Natural/fold 0
, example1 = List/fold Bool [ True ]
}
