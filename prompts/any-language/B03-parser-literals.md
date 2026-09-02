# B03 — Parser: numeric, temporal, and bytes literals

Depends on: B02  
Read first: `prompts/any-language/00-shared.md`

## Spec

`dhall.abnf` rules for Natural (including hex), Integer, Double, Date,
Time, TimeZone, Bytes (`0x"..."`).

Natural vs Integer: the language uses `+n`/`-n` for Integer and unadorned
naturals for Natural (`docs/howtos/migrations/Swapped-syntax-for-Natural-numbers-and-Integers.md`
is historical; implement **current** ABNF).

## Tests that must pass

Internal tests for edge cases (hex naturals, `-0.0`, dates). Suite files
wait for B07+encode:

```text
tests/parser/success/time/**/*A.dhall
tests/parser/success/hexadecimalA.dhall
tests/parser/success/naturalA.dhall
tests/parser/success/bytesA.dhall
```

## Done when

Literal parsers exist and match ABNF, including failure cases you can
already run from `tests/parser/failure/time/` as “must not parse” once
the top-level parser exists; if complete-expression is still incomplete,
keep them internal.
