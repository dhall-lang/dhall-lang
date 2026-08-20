# B28 — Imports: chain, canonicalize, `as Location`

Depends on: B27  
Read first: `prompts/any-language/00-shared.md`

## Spec

Notes: parent/child, `</>`, canonicalize. `imports.md` location union
values. Tests README: ancestor path is the relative path from the parent
of the repo to the test file.

Env for import tests: `HOME=tests/import/home`,
`XDG_CACHE_HOME=tests/import/cache`, `DHALL_TEST_VAR=6 * 7`.

## Tests that must pass

```text
tests/import/success/unit/asLocation/**/*A.dhall
tests/import/success/unit/Canonicalize*A.dhall
tests/import/success/unit/Chain*A.dhall
tests/import/success/unit/QuotedPathA.dhall
tests/import/success/unit/Relative*A.dhall
tests/import/success/unit/AbsoluteA.dhall
tests/import/success/unit/HomeA.dhall
tests/import/success/unit/FilenameWithSpacesA.dhall
```

## Done when

Those tests pass (`resolve(A)` equals parsed B).
