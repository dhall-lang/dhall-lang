# A22 — Imports: fetch Code / Text / Bytes, env, missing, `?`

Depends on: A21  
Read first: `prompts/haskell/00-shared.md`

## Spec

Implementation notes: Step 3 fetch modes; **soft vs hard** failures for
`e0 ? e1` (table in the notes). `imports.md` for `as Text` / `as Bytes` /
`missing`.

Integrity hash of `as Text`/`as Bytes` is SHA-256 of **encoded Dhall
literals**, not raw file bytes.

Type-check resolved **Code** imports in the empty context (A19
`inferType`). Normalize Code imports fully (`betaNormalize`).

Do not implement HTTP or `as Source` yet; skip tests that need them.

## Tests that must pass

```text
tests/import/success/unit/EnvA.dhall
tests/import/success/unit/EnvHomeA.dhall
tests/import/success/unit/EnvSetA.dhall
tests/import/success/unit/EnvSetAsTextA.dhall
tests/import/success/unit/AsTextA.dhall
tests/import/success/unit/AsBytesA.dhall
tests/import/success/unit/MissingA.dhall
tests/import/success/unit/Alternative*A.dhall   # local/env only
tests/import/success/unit/SimpleA.dhall
tests/import/success/unit/MixImportModesA.dhall
tests/import/success/unit/DontTryResolvingA.dhall
tests/import/success/unit/NormalizeA.dhall
tests/import/success/asLocationA.dhall          # if not already in A21
```

Failure (must fail; alternatives must **not** recover hard errors):

```text
tests/import/failure/unit/EnvUnset.dhall
tests/import/failure/unit/EnvUnsetAsText.dhall
tests/import/failure/unit/FileMissing.dhall
tests/import/failure/unit/Missing.dhall
tests/import/failure/unit/Cycle.dhall
tests/import/failure/unit/DontRecoverCycle.dhall
tests/import/failure/unit/DontRecoverParseError.dhall
tests/import/failure/unit/DontRecoverTypeError.dhall
tests/import/failure/unit/VarAcrossImportBoundary.dhall
tests/import/failure/unit/alternativeEnv.dhall
tests/import/failure/unit/alternativeEnvMissing.dhall
```

Honor `*ENV.dhall` sidecar maps (`tests/README.md`).

Skip HTTP/CORS/hash-cache tests until A24–A25.

## Done when

Listed local/env import tests pass.
