# B22 — Type inference: records

Depends on: B21  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md` Records (types, literals, `∧` `⫽` `⩓`, field,
projection, completion). Dotted keys already desugared.

## Tests that must pass

Success: `Record*`, `RecursiveRecord*`, `RightBiasedRecord*`, `Completion*`.
Failure: same prefixes plus `AnnotationRecord*`.

## Done when

Those unit tests pass.
