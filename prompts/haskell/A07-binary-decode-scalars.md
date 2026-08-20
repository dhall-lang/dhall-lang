# A07 — Binary decode: records, unions, Bool, numbers, Text, Bytes

Depends on: A06  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/binary.md`: Records, Unions, Bool, Natural, Integer, Double, Text,
Bytes. Pay attention to:

- shortest float encoding (half / single / double)
- `Infinity` / `-Infinity` / NaN
- bignums for large Natural/Integer
- map key order when decoding records/unions

## Tests that must pass

```text
tests/binary-decode/success/unit/RecordLiteralA.dhallb
tests/binary-decode/success/unit/RecordTypeA.dhallb
tests/binary-decode/success/unit/RecordFieldAccessA.dhallb
tests/binary-decode/success/unit/RecordProjectFieldsA.dhallb
tests/binary-decode/success/unit/recordProjectionByExpressionA.dhallb
tests/binary-decode/success/unit/UnionTypeA.dhallb
tests/binary-decode/success/unit/BoolTrueA.dhallb
tests/binary-decode/success/unit/BoolFalseA.dhallb
tests/binary-decode/success/unit/BoolIfA.dhallb
tests/binary-decode/success/unit/NaturalZeroA.dhallb
tests/binary-decode/success/unit/NaturalTwentyFourA.dhallb
tests/binary-decode/success/unit/NaturalBigA.dhallb
tests/binary-decode/success/unit/IntegerZeroA.dhallb
tests/binary-decode/success/unit/IntegerNegativeOneA.dhallb
tests/binary-decode/success/unit/IntegerBigPositiveA.dhallb
tests/binary-decode/success/unit/IntegerBigNegativeA.dhallb
tests/binary-decode/success/unit/DoubleHalfA.dhallb
tests/binary-decode/success/unit/DoubleSingleA.dhallb
tests/binary-decode/success/unit/DoubleDoubleA.dhallb
tests/binary-decode/success/unit/DoubleInfinityA.dhallb
tests/binary-decode/success/unit/DoubleNegativeInfinityA.dhallb
tests/binary-decode/success/unit/TextSimpleA.dhallb
tests/binary-decode/success/unit/TextInterpolatedA.dhallb
tests/binary-decode/success/unit/BytesA.dhallb
tests/binary-decode/success/unit/CompletionA.dhallb
```

Plus previous binary-decode tests.

## Done when

All listed files decode to the corresponding `*B.dhall`.
