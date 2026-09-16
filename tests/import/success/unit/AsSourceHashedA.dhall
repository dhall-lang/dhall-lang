{-
Phase 2 of `as Source` inlines the hashed child (already validated and
normalized to `1`).  The phase-1 cache product would still contain the
hashed import node `./child.dhall sha256:...` inside the `let`.
-}
./AsSourceHashed/parent.dhall as Source
