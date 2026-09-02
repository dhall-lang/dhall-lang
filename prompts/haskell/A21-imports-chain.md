# A21 — Imports: stack, chain, canonicalize, `as Location`

Depends on: A19, A20  
Read first: `prompts/haskell/00-shared.md`

## Goal

Start `Imports` (literate Haskell). Prefer putting code in
`imports-implementation-notes.md` **or** a new `imports-implementation.md`
that the notes can include — do **not** dump a wall of Haskell into the
unclear parts of `imports.md` unless you are also clarifying those
judgments. Export a resolver the tests can call.

## Spec (algorithm)

`imports-implementation-notes.md` on
`feature/winitzki/lang-1185-import-as-source` (copy into `standard/` if
missing):

- Import stack is never empty; initial element is a fake root for cwd.
- Parent vs child; chain (`</>`) then canonicalize (`..` / `.`).
- `as Location`: do not fetch; return the Location union value described
  in `imports.md`.

Also `imports.md`: Directories and files, Canonicalization, Chaining.

For this slice, the “here” path for a test file is the relative path from
the parent of this repository to the test file, as in `tests/README.md`
(example: `./dhall-lang/tests/import/success/asLocationA.dhall` — use the
actual relative path used by other implementations / the README).

## Environment for tests

- `HOME` = absolute `tests/import/home/`
- `XDG_CACHE_HOME` = absolute `tests/import/cache` (read-only / reset)
- `DHALL_TEST_VAR` = `6 * 7`

## Tests that must pass

`as Location` and path-only unit tests (no HTTP required):

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

Compare resolved A to parsed B (`tests/README.md`).

## Done when

Those cases pass. Resolver API exists for later slices to extend.
