# B02 — Parser: whitespace, comments, identifiers

Depends on: B01  
Read first: `prompts/any-language/00-shared.md`

## Spec

`standard/dhall.abnf` from the start through whitespace, comments
(nested block comments), shebangs, labels, quoted labels, identifiers,
keywords.

**No lexer.** Characters are the tokens. Nested `{- -}` and interpolations
make a lexer incorrect (see the comments at the top of `dhall.abnf`).

Backtrack; alternatives are left-to-right.

## Do

Implement `complete-expression` far enough to parse identifiers and
discard leading/trailing whitespace and comments. You do not need full
expressions yet.

## Tests that must pass

Parser **success** files that are only comments/whitespace/simple
identifiers if they exist; otherwise no suite tests. Parser **failure**
that are comment/label errors under `tests/parser/failure/` may wait for
B08.

Useful files to try manually:

```text
tests/parser/success/blockCommentA.dhall
tests/parser/success/lineCommentA.dhall
tests/parser/success/nestedBlockCommentA.dhall
tests/parser/success/whitespaceA.dhall
```

These still need a full expression parser to encode CBOR — mark them
pending until B07, but your combinators should already accept those
comment forms inside a trivial program once B07 exists. In this slice,
add **internal** tests for nested comments and identifier vs keyword.

## Done when

Comment/identifier combinators exist and have internal tests. ABNF names
are traceable in the code (function names ≈ rule names).
