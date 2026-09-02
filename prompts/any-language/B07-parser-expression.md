# B07 — Parser: complete expression (operators, λ, let, if, …)

Depends on: B03–B06  
Read first: `prompts/any-language/00-shared.md`

## Spec

`dhall.abnf` `expression` and operator precedence layers (`import-alt-expression`
through `application-expression`, etc.). Unicode and ASCII synonyms
(`λ`/`\`, `∀`/`forall`, `→`/`->`, `∧`/`/\`, `⫽`/`//`, `===`/`≡`, …).

`complete-expression`: optional shebangs, whitespace, expression,
optional trailing line comment.

Application is left-associative. Operator associativity is in the ABNF
(do not guess).

## Tests that must pass

**Cannot fully pass until B09 (encode).** In this slice, parse must
succeed on every parser success A file (300) and fail on every parser
failure file (94). Encode comparison waits for B09.

```text
tests/parser/success/**/*A.dhall     # parse succeeds
tests/parser/failure/**/*.dhall      # parse fails
```

If encode is not ready, assert parse-only.

## Done when

All parser success files parse; all parser failure files fail. Remaining
work is CBOR equality (B09).
