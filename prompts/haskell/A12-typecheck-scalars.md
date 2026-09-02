# A12 — Type inference: Bool, numbers, Text, Bytes, Date/Time/TimeZone

Depends on: A11  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/type-inference.md` sections: Bool, Natural, Text,
Date/Time/TimeZone, Integer, Double (and Bytes if specified there or as a
builtin typed `Bytes`).

## Tests that must pass

Success, under `tests/type-inference/success/unit/`:

```text
Bool*, True, False, If*
Natural*
Integer*
Double*
Text*
Bytes*
Date*, Time*  (including tests/type-inference/success/unit/time/ if present)
```

Failure:

```text
tests/type-inference/failure/unit/IfNotBool.dhall
tests/type-inference/failure/unit/IfBranchesNotMatch.dhall
tests/type-inference/failure/unit/IfBranchesNotTermTypeOrKind.dhall
tests/type-inference/failure/unit/OperatorAndNotBool.dhall
tests/type-inference/failure/unit/OperatorEqualNotBool.dhall
tests/type-inference/failure/unit/NaturalSubtractNotNatural.dhall
tests/type-inference/failure/DateApplyTime.dhall
```

Plus A11. Timeout on failure tests.

## Done when

Those globs pass. Operators on these types that are in `Operator*` unit
success files may wait for A17 if they need function types; include
`Operator*` success tests that only involve Bool/Natural/Text/List append
if they already type-check with rules from this slice
(`type-inference.md` operator rules live next to each type).
