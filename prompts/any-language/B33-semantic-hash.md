# B33 — Semantic hash

Depends on: B13, B17, B09; full suite after B32  
Read first: `prompts/any-language/00-shared.md`

## Spec

`tests/README.md` semantic-hash: parse A, resolve imports if needed, hash
= SHA-256 of `encode(α(β(e)))`. Fixture `*B.hash` is `sha256:` + hex
(not the `1220` multihash prefix used in cache filenames).

Enable simple/simplifications/tutorial as soon as β+encode exist;
prelude after B32.

## Tests that must pass

```text
tests/semantic-hash/success/**/*A.dhall
```

If you do this slice **before** B32, only import-free folders.

## Done when

All enabled semantic-hash tests match `*B.hash`.
