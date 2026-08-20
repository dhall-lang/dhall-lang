# A27 — Interpret pipeline

Depends on: A26  
Read first: `prompts/haskell/00-shared.md`

## Goal

`standard/Interpret.hs` currently only parses and encodes. Make it a
complete reference interpreter:

```text
read stdin → parse → resolve imports (cwd fake-root) → inferType
          → betaNormalize → (optional) print encode / write .dhallb
```

Printing Dhall source is **not** required. Printing the CBOR `Term` or
diagnostic notation is enough. If type-check fails, exit non-zero.

## Do

Wire `Imports` + `TypeInference` + `BetaNormalization` + `Binary`. Keep
the existing CLI shape (`dhall [out.dhallb] < in.dhall`) or add flags
documented in a comment at the top of `Interpret.hs`. Do not chase
dhall-haskell CLI compatibility.

## Tests that must pass

Same as A26 (`cabal test` full). Manual smoke: interpret
`λ(x : Bool) → x` (type `Bool → Bool`, normal form itself).

## Done when

Interpret uses the real pipeline. Tests still pass.
