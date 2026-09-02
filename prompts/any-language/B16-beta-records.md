# B16 — β-normalization: records, unions, `with`, merge, completion

Depends on: B15  
Read first: `prompts/any-language/00-shared.md`

## Spec

`beta-normalization.md`: Records, Unions, `with` expressions. Field
selection, projection, `∧` `⫽` `⩓`, `merge`, `showConstructor`,
completion `T::r` (sugar for `(T.default ⫽ r) : T.Type`). Sort record
fields as specified.

## Tests that must pass

`tests/normalization/success/unit/` names starting with:

```text
Record, Field, Project, NestedRecord
Recursive, RightBiased, Prefer, Combine
Union, Merge, EmptyAlternative, ShowConstructor
With, Completion
```

Then `tests/normalization/success/simple/**` if still import-free.

## Done when

Those tests pass.
