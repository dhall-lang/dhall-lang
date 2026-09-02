# B20 — Type inference: scalars

Depends on: B19  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md`: Bool, Natural, Text, Date/Time/TimeZone, Integer,
Double, Bytes, and their operators/builtins.

## Tests that must pass

Success unit globs: `Bool*`, `True`, `False`, `If*`, `Natural*`,
`Integer*`, `Double*`, `Text*`, `Bytes*`, `Date*`, `Time*`.

Failure: `IfNotBool`, `IfBranchesNotMatch`, `IfBranchesNotTermTypeOrKind`,
`OperatorAndNotBool`, `OperatorEqualNotBool`, `NaturalSubtractNotNatural`,
`DateApplyTime.dhall`.

## Done when

Those tests pass (timeout on failures).
