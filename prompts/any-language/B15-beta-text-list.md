# B15 — β-normalization: Text, List, Optional, Bytes

Depends on: B14  
Read first: `prompts/any-language/00-shared.md`

## Spec

`beta-normalization.md`: Text, List, Optional (and Bytes if present).
Implement `Text/show`, `Text/replace`, list `build`/`fold`/`length`/`head`/
`last`/`indexed`/`reverse`, `Some`/`None` reductions, `++` on text, `#` on
lists. Use the `Text` chunk monoid from `syntax.md` (empty interpolations
collapse).

## Tests that must pass

`tests/normalization/success/unit/` names starting with:

```text
Text, BareInterpolation
List
Some, None, Optional
Bytes
OperatorTextAppend, OperatorListAppend
ToMap
```

## Done when

Those unit tests pass.
