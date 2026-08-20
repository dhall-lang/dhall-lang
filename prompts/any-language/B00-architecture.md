# B00 — Architecture and test driver

Depends on: nothing  
Read first: `prompts/any-language/00-shared.md`

## Goal

Create the project skeleton and a test harness that can discover files
under `tests/` of **this** dhall-lang checkout. Do not parse Dhall yet
beyond a stub.

## Do

1. Choose a module layout that maps to later slices: `Syntax`, `Parser`,
   `Binary`, `Shift`, `Substitute`, `Alpha`, `Beta`, `Equiv`, `FunctionCheck`,
   `TypeInfer`, `Import`, `Hash`, `Bindings`.
2. Implement directory walking and the comparison helpers you will need:
   read text/bytes; compare byte strings; compare hex hashes.
3. Document how to point the harness at `dhall-lang/tests` (env var or
   flag). Do not vendor a copy of the tests.
4. List suites as skip/fail-empty except a smoke test that the tree exists
   (`tests/parser/success` is non-empty).

## Tests that must pass

Harness starts; no false greens on parser tests yet.

## Done when

A contributor can run “all tests” and see parser tests **fail** (not
skipped silently) until B02–B08, unless you explicitly mark them pending
with a name. Prefer pending/skip with a message over claiming success.
