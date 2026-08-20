# B06 — Parser: records, unions, `with` path desugaring

Depends on: B02  
Read first: `prompts/any-language/00-shared.md`

## Spec

`dhall.abnf` record type/literal, union type. `standard/record.md`:

1. Pun `{ x }` → `{ x = x }`
2. Dotted `{ x.y = a }` → nested record `{ x = { y = a } }`
3. Duplicate keys `{ x = a, x = b }` → `{ x = a ∧ b }` (`CombineRecordTerms`)

These run at parse time. Later judgments never see dotted keys or
duplicate fields.

`with e.k.ks = v` is an AST node with a non-empty path; `?` path
components are allowed (`syntax.md`).

## Tests that must pass after B07+B09

Parser unit tests whose names contain `Record`, `Union`, `with`, `ToMap`,
`Completion`.

## Done when

Desugaring matches `record.md` examples. Empty record type `{}` vs empty
literal `{=}` are distinct.
