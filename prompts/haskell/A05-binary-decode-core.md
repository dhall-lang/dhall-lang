# A05 — Binary decode: core expression labels

Depends on: A01 (parser success already encodes)  
Read first: `prompts/haskell/00-shared.md`

## Goal

Implement `decode :: Term -> Maybe Expression` (or `Either`) in `binary.md`
for variables, builtins, application, λ, ∀, and operators. Decode is
specified in markdown today and **has no Haskell**.

## Spec

`standard/binary.md` sections:

- Decoding judgment
- Built-in constants
- Variables
- Function application
- Functions
- Operators

Follow the same literate style as `encode`. Reject CBOR that does not match
a rule (do not invent nodes).

## Do

1. Add `decode` next to `encode` in `binary.md`. You may decode through
   `Codec.CBOR.Term` (already used by encode).
2. Tasty: for each matching `tests/binary-decode/success/unit/` fixture,
   deserialise `*A.dhallb` to `Term`, `decode`, parse `*B.dhall`, compare
   via `encode` (or structural equality of expressions after encode).
3. Enable these success files in this slice:

```text
VariableNamed, VariableUnderscore,
VariableNamedOversizedInt, VariableUnderscoreOversizedInt
BuiltinNaturalShow, BuiltinNaturalSubtract
Application, ApplicationMultiple
LambdaNamedX, LambdaUnderscore
PiNamedX, PiUnderscore
OperatorAnd, OperatorCombine, OperatorCombineTypes, OperatorEq,
OperatorEquiv, OperatorImportAlt, OperatorListAppend, OperatorNeq,
OperatorOr, OperatorPlus, OperatorPrefer, OperatorTextAppend, OperatorTimes
```

Self-describe CBOR tags (`CBORTag 55799`) may be implemented now (the spec
says ignore the tag and decode the payload) even if the SelfDescribeCBOR*
tests wait until A08.

## Tests that must pass

The files listed above under `tests/binary-decode/success/unit/`, plus A01.

## Done when

`decode` is compiled from `binary.md` and those unit tests pass.
