# A04 — β-normalization simple / simplifications / tutorial / regression

Depends on: A03  
Read first: `prompts/haskell/00-shared.md`

## Goal

Run the remaining normalization tests that **do not** need a live import
system.

## Spec

Same as A03. `tests/README.md`: non-`simple`/`unit` folders may contain
imports — skip those until A21+.

## Do

1. Enable, in order:
   - `tests/normalization/success/simple/**/*A.dhall` (17)
   - `tests/normalization/success/simplifications/**/*A.dhall` (9)
   - `tests/normalization/success/haskell-tutorial/**/*A.dhall` (6)
   - `tests/normalization/success/regression/**/*A.dhall` (5)
   - top-level files under `tests/normalization/success/` that have no
     imports in the parsed AST (try `WithRecordValueA.dhall`; skip
     `remoteSystemsA.dhall` if it imports).
2. Skip (do not fail the suite) any file whose parsed A or B contains
   `Import`. List skipped names in the commit/PR message.

## Tests that must pass

All of the folders above except skipped import cases, plus A01–A03.

## Done when

Every normalization test that is import-free passes.
