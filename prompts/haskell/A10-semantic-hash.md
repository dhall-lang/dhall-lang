# A10 — Semantic hash (no imports)

Depends on: A03, A08 (`encode` of normal forms)  
Read first: `prompts/haskell/00-shared.md`

## Goal

Compute the semantic hash: SHA-256 of the CBOR encoding of the
α-normalized β-normal form, formatted like the `*.hash` fixtures
(`sha256:` followed by hex).

Ordinary **Code** imports are not resolved in this slice. Skip any A file
whose AST contains `Import`.

## Spec

- `standard/binary.md` motivation (integrity check = hash of binary of
  normal form)
- `tests/README.md` — “Running semantic-hash tests”
- Multihash prefix `1220` appears in **cache filenames**, not in the
  `*.hash` fixture text. Match the fixture files exactly.

## Tests that must pass

```text
tests/semantic-hash/success/simple/**/*A.dhall
tests/semantic-hash/success/simplifications/**/*A.dhall
tests/semantic-hash/success/haskell-tutorial/**/*A.dhall
```

Skip `tests/semantic-hash/success/prelude/**` and `remoteSystemsA.dhall`
until imports work. Count at planning time: 12 simple + simplifications +
tutorial.

Plus A01–A09 suites already enabled.

## Done when

Import-free semantic-hash tests match `*B.hash`.
