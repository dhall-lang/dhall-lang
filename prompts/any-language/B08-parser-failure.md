# B08 — Parser failures (if any remain)

Depends on: B07  
Read first: `prompts/any-language/00-shared.md`

## Goal

Green `tests/parser/failure/**` (94): `unit/` (39), `spacing/` (27),
`time/` (9), plus top-level. No required error strings.

## Do

Fix ABNF fidelity (keyword reserved as labels, spacing around operators,
invalid times). Do not special-case filenames.

## Tests that must pass

```text
tests/parser/failure/**/*.dhall
tests/parser/success/**/*A.dhall   # still parse
```

## Done when

Parser failure suite is green.
