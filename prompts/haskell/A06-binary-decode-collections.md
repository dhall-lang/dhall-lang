# A06 — Binary decode: lists, Optional, merge, toMap, showConstructor

Depends on: A05  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/binary.md`: List, Optional, merge-expressions, and the `toMap` /
`showConstructor` encoding labels used by encode (labels 4, 5, 6, 27, 28, 34).

## Tests that must pass

```text
tests/binary-decode/success/unit/ListEmpty0A.dhallb
tests/binary-decode/success/unit/ListEmpty1A.dhallb
tests/binary-decode/success/unit/ListOneA.dhallb
tests/binary-decode/success/unit/SomeA.dhallb
tests/binary-decode/success/unit/MergeAnnotatedA.dhallb
tests/binary-decode/success/unit/MergeUnannotatedA.dhallb
tests/binary-decode/success/unit/ToMapA.dhallb
tests/binary-decode/success/unit/ToMapAnnotatedA.dhallb
```

If `ShowConstructor` appears as a binary-decode fixture, include it. Plus A05.

## Done when

Those decode tests pass. Invalid list shapes are not required until A09.
