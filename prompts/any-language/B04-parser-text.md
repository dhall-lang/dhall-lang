# B04 — Parser: Text (double-quoted, single-quoted, interpolation)

Depends on: B02  
Read first: `prompts/any-language/00-shared.md`

## Spec

- `dhall.abnf` text rules
- `standard/multiline.md` — `to-double-quotes`: single-quoted (`''…''`)
  desugars at parse time to double-quoted text. Implement that judgment.
  Escape `'''` and `''${`.

Interpolations parse a complete expression (mutual recursion with B07).
You may stub the inner expression as “identifier only” until B07 if you
keep the knot untied; finish interpolations in B07.

## Tests that must pass after B07+B09 (list them now; enable later)

```text
tests/parser/success/text/**/*A.dhall     # 28
```

This slice: internal tests for indentation stripping from `multiline.md`
examples.

## Done when

`to-double-quotes` is implemented and unit-tested against the examples in
`multiline.md`.
