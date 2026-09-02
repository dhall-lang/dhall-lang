# B14 — β-normalization: universes, Bool, Natural, Integer, Double, time

Depends on: B12, B07  
Read first: `prompts/any-language/00-shared.md`

## Spec

`standard/beta-normalization.md` sections: Constants, Variables, Bool,
Natural, Integer, Double, Date/Time/TimeZone.

Normalize under binders. Saturated builtins reduce; unsaturated stay
applied. Natural builtins (`Natural/fold`, `build`, `isZero`, `even`,
`odd`, `toInteger`, `show`, `subtract`) and Integer/Double/time `show`
family: follow the spec even if slow.

## Tests that must pass

`tests/normalization/success/unit/` files whose names start with:

```text
Bool, True, False, If
Natural
Integer
Double
Time, Date   # if present
OperatorAnd, OperatorOr, OperatorEq, OperatorNeq, OperatorPlus, OperatorTimes
```

Skip files with `Import` nodes.

## Done when

Those unit tests pass (`beta(A)` equals parsed B).
