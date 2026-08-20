# A01 — Expand tasty: helpers + parser failures

Depends on: A00  
Read first: `prompts/haskell/00-shared.md`

## Goal

Turn `standard/tasty/Main.hs` into a driver that can run **all** acceptance
suites described in `tests/README.md`, and implement the parser **failure**
suite now.

## Spec

- `tests/README.md` — “Running parser tests”
- Success behavior is already implemented: parse `*A.dhall`, `Binary.encode`,
  compare to `*B.dhallb` with NaN-aware CBOR equality.

## Do

1. Factor helpers: walk a directory; pair `*A.dhall` / `*B.dhall` /
   `*B.dhallb` / `*.hash`; parse a file with `Parser.completeExpression` +
   `eof`.
2. Keep parser **success**: `tests/parser/success/**/*A.dhall` (300 cases,
   including `unit/`, `unit/operators/`, `unit/import/`, `text/`, `time/`).
3. Add parser **failure**: each `tests/parser/failure/**/*.dhall` (94 cases:
   `unit/`, `spacing/`, `time/`, top-level) must fail to parse (or fail
   before producing a complete expression). Do not require a specific error
   message.
4. Leave other suites registered but empty or skipped until later slices
   implement them — or add them as `skip` groups. Do not start
   α-normalization tests in this slice.

## Do not

- Change `Parser.hs` unless a failure test currently **succeeds** at
  parsing; if that happens, report it (possible spec/test bug) rather than
  silently loosening the grammar.
- Touch `.dhallb` / `.diag` files.

## Tests that must pass

```text
tests/parser/success/**/*A.dhall     # encode matches *B.dhallb
tests/parser/failure/**/*.dhall      # parse fails
```

Counts at planning time: 300 success, 94 failure.

## Done when

`cabal test` runs both parser groups and they all pass. Subsequent slices
can add groups without rewriting discovery.
