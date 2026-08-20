# A09 — Binary decode failures

Depends on: A08  
Read first: `prompts/haskell/00-shared.md`

## Goal

`decode` must reject illegal CBOR. No particular error type is required.

## Tests that must pass

Each file under `tests/binary-decode/failure/**/*.dhallb` (9 cases at
planning time) must fail to decode to an `Expression`:

```text
ApplyNoArgs
LambdaExplicitlyNamedUnderscore
ListOneWithAnnotation
NaturalNegativeOne
OperatorOrTooFewArgs
OperatorOrTooManyArgs
OperatorUnknownOpcode
PiExplicitlyNamedUnderscore
VariableExplicitlyNamedUnderscore
```

Plus all binary-decode success tests.

## Done when

Failure group is green. Do not “decode anyway” to make a test pass.
