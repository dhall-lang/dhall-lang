# A28 — Generate `.dhallb` and `.diag` from the reference; drop Ruby

Depends on: A08 (`encode`/`decode`), A01 parser  
Read first: `prompts/haskell/00-shared.md`

## Goal

All CBOR fixtures and diagnostic files are produced by **this** package,
not by `cbor2diag.rb`. That ties fixtures to the literate `encode` in
`binary.md`.

## Do

1. Implement `diag :: Term -> Text` in `binary.md` (RFC 8949 diagnostic
   notation for the CBOR subset Dhall uses). Match existing `*.diag`
   files: e.g. `[1, "x", ["T", 0], ["y", 0]]`, half-floats like `5.5`.
   Treat current committed `.diag` as the golden output to match.
2. Extend the `dhall` executable so it can:
   - write CBOR bytes (`*.dhallb`)
   - write `diag` text (`*.diag`)
   - print `diag` of a raw `.dhallb` (for binary-decode fixtures that are
     not parse round-trips)
3. Replace `scripts/generate-test-files.sh` with a **bash** script that
   uses that executable:
   - For each `tests/parser/success/**/*A.dhall`: parse+encode → `*B.dhallb`
     and `*B.diag`
   - For each `tests/binary-decode/**/*.dhallb`: Term → `*.diag`
   - Do **not** rewrite hand-crafted binary-decode `*A.dhallb`
4. Remove Ruby `cbor2diag` from `nixops/overlay.nix` `expected-test-files`.
   Drive generation with the `standard` package’s executable instead.
   Prelude `preludeB.dhall` may still use production `pkgs.dhall` until
   A26 exists; after A26 prefer the reference for that file too if the
   bytes/text match.
5. Update `.github/CONTRIBUTING.md` (“How do I update generated test
   files?”) so it no longer mentions `.cbor` / Ruby.

## Tests that must pass

```text
cabal test
# after regenerating fixtures:
# test-files-lint / scripts/generate-test-files.sh is a no-op (no diff)
```

Parser success encode tests still pass against regenerated `.dhallb`.

## Done when

- No `cbor-diag` / `cbor2diag.rb` in the Nix closure for test fixtures.
- Regenerating fixtures does not change git content (or the new content is
  committed once and then stable).
- CONTRIBUTING and `tests/README.md` describe the reference generator.
