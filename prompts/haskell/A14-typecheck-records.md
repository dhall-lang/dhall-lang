# A14 — Type inference: records, merges, field, project, completion

Depends on: A13  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/type-inference.md` — Records. Also record operators `∧` `⫽` `⩓`
and completion `T::r`. Parser already desugars dotted fields and puns
(`record.md`).

## Tests that must pass

Success `tests/type-inference/success/unit/`:

```text
Record*
RecursiveRecordMerge*
RecursiveRecordTypeMerge*
RightBiasedRecordMerge*
Completion*
```

Failure `tests/type-inference/failure/unit/`:

```text
Record*
AnnotationRecord*
RecursiveRecordMerge*
RecursiveRecordTypeMerge*
RightBiasedRecordMerge*
Completion*
```

Plus previous type-inference tests.

## Done when

All record-prefixed unit success/failure tests pass.
