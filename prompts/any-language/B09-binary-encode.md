# B09 — Binary encode

Depends on: B07  
Read first: `prompts/any-language/00-shared.md`

## Spec

`standard/binary.md` **Encoding judgment** (all subsections through Date/
Time/TimeZone). Produce RFC 7049/8949 CBOR matching `*B.dhallb`.

Important details already in the spec:

- Import label 24; modes 0=Code, 1=Text, 2=Location, 3=Bytes, **4=Source**
  (unless the as-Source PR picks another integer)
- Application encoded as a flattened list
- Let encoded as a flattened list
- Doubles: shortest of half/single/double that preserves the value
- Natural/Integer bignums when they do not fit in CBOR ints
- Record/union maps sorted as specified

You may encode via an intermediate CBOR term type (recommended) then
serialize.

## Tests that must pass

```text
tests/parser/success/**/*A.dhall
# encode(parse(A)) bytes == *B.dhallb
```

Use NaN-aware comparison if you compare decoded CBOR terms instead of
raw bytes (NaN ≠ NaN). Prefer **byte equality** with the committed
`.dhallb` once encode matches.

## Done when

All 300 parser success tests pass as specified in `tests/README.md`.
