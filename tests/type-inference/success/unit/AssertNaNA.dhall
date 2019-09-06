{-
    Equivalence must compare the result of the binary encoding, so in
    particular comparing Doubles should not use standard float equality (where
    NaN != NaN).
-}
assert : NaN === NaN
