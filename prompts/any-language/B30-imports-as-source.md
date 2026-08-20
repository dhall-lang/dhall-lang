# B30 — Imports: `as Source`

Depends on: B29  
Read first: `prompts/any-language/00-shared.md`

## Spec

Notes: two-phase Source; alternatives with Source; cache product vs
parent value. Parser already has `Source` (B05). Encode mode 4 (B09).

Phase 1: inline unhashed children without normalizing; keep hashed
children as import nodes; that is the semantic-cache payload.
Phase 2: expand remaining hashes; import-free, type-checked, not fully
normalized.

## Tests that must pass

as-Source fixtures from the standard PR if present; otherwise add a
minimal unit test as in Haskell A23. All B29 tests still pass.

## Done when

Source mode matches the notes; Code vs Source cache keys differ.
