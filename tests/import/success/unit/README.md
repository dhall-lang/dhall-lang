These unit tests compare the resolve product of `A` to `B` up to
β-equivalence (see `tests/README.md`).

`NormalizeA.dhall` / `NormalizeB.dhall` import `1 + 1` and expect `2`.  That
checks that the imported expression is β-equivalent to `2` after resolution.
It does **not** require implementations to β-normalize unhashed imports at
resolve time; an implementation may inline `1 + 1` and still pass by
normalizing both sides before comparison.
