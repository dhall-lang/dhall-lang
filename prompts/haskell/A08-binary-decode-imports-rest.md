# A08 — Binary decode: imports, let, annotation, with, datetime, CBOR tags

Depends on: A07  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/binary.md`: Imports, let-expressions, type annotations,
with-expressions, Date/Time/TimeZone, CBOR tag 55799 (self-describe).

If `as Source` is already in `ImportMode` (A23), decode mode `4` (or the
PR-assigned integer) here; otherwise keep a hole and reject unknown modes
until A23.

## Tests that must pass

```text
tests/binary-decode/success/unit/imports/**/*A.dhallb
tests/binary-decode/success/unit/LetOneTypedA.dhallb
tests/binary-decode/success/unit/LetOneUntypedA.dhallb
tests/binary-decode/success/unit/LetMultipleA.dhallb
tests/binary-decode/success/unit/AnnotationA.dhallb
tests/binary-decode/success/unit/AssertA.dhallb
tests/binary-decode/success/unit/DateA.dhallb
tests/binary-decode/success/unit/TimeA.dhallb
tests/binary-decode/success/unit/TimeZoneA.dhallb
tests/binary-decode/success/unit/SelfDescribeCBORXA.dhallb
tests/binary-decode/success/unit/SelfDescribeCBORX2A.dhallb
tests/binary-decode/success/unit/SelfDescribeCBORX3A.dhallb
```

Plus A05–A07. That should be the entire `tests/binary-decode/success/` tree
(~82 cases).

## Done when

All binary-decode **success** tests pass.
