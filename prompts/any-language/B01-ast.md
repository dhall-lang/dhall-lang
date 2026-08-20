# B01 — Abstract syntax

Depends on: B00  
Read first: `prompts/any-language/00-shared.md`

## Goal

Define the expression AST from `standard/syntax.md` (and the `Expression`
constructors described there). No parser yet.

## Spec

`standard/syntax.md` — every production of `a, b, f, …`. Include:

- Variables `x@n` (`n` defaults to 0)
- λ, ∀, let (optional annotation), if, merge, toMap, empty/non-empty lists
- annotation, all operators including `?`
- application, field, project-by-labels, project-by-type, completion
- assert, `with` (non-empty path; path components are labels or `?`)
- Double, Natural, Integer, Text chunks, Bytes, Date, Time (+precision),
  TimeZone
- record type/literal, union type
- showConstructor
- Import: type (missing / remote URL / path with prefix / env), mode
  (Code, RawText, RawBytes, Location, **Source**), optional SHA-256
- builtins and constants listed in syntax.md

Record **literals in the AST have unique keys and no dotted paths** —
that desugaring is parse-time (`record.md`).

Text literals: list of `(prefix, interpolation)` plus a final suffix
(`syntax.md` / existing Haskell `Chunks` comments are a fine model).

## Tests that must pass

None from `tests/` yet. Add a tiny unit test that you can construct
`λ(x : Bool) → x` and `Natural/show` as AST nodes.

## Done when

Every `syntax.md` construct has a constructor. `ImportMode` includes
`Source`.
