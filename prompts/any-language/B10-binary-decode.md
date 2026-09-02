# B10 — Binary decode

Depends on: B09  
Read first: `prompts/any-language/00-shared.md`

## Spec

`standard/binary.md` **Decoding judgment**. Ignore CBOR tag 55799.
Reject anything that does not match a rule.

## Tests that must pass

```text
tests/binary-decode/success/**/*A.dhallb    # decode A, parse B, equal
tests/binary-decode/failure/**/*.dhallb     # decode fails
```

~82 success, 9 failure.

## Done when

Entire binary-decode suite passes.
