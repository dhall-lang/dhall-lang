# B27 — Imports: model (stack, Γ, modes)

Depends on: B26, B05  
Read first: `prompts/any-language/00-shared.md`

## Goal

Define the resolver’s data structures. No network yet.

## Spec

`imports-implementation-notes.md` (PR branch if missing locally):

- Kinds: Code, Source, Text, Bytes, Location, void `missing`
- Stack never empty; fake root = process cwd (tests override HOME/cwd)
- Γ roles: external world vs in-memory reuse vs **semantic** cache
- Key in-memory entries by canonical import **including mode**
- Do not implement `.cache/dhall-haskell/` semi-semantic cache

API sketch (any names):

```text
resolve(expr, stack, env) -> expr'
```

where `env` can fetch files, env vars, and later HTTP.

## Tests that must pass

None yet. Internal: stack push/pop; Code vs Source are different keys.

## Done when

Types and a recursive walk that leaves non-import nodes intact exist.
