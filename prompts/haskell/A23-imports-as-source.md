# A23 — Imports: `as Source`

Depends on: A22  
Read first: `prompts/haskell/00-shared.md`

## Goal

Implement upcoming `as Source` even if it is not on `master` yet.

## Spec

`imports-implementation-notes.md` sections:

- Supported import modes (`Source` constructor)
- Ordinary code vs `as Source`
- Caching behavior with `as Source` (two phases)
- Import alternatives with `as Source`
- Summary algorithm steps 11–13

Behavior:

1. Fetch and parse as Dhall; recursively resolve children.
2. **Phase 1 (cache product):** unhashed children inlined without
   normalization; **hash-protected children left as import nodes** (after
   validating them if required). Store that product in the semantic cache
   when the parent import is frozen.
3. **Phase 2 (value for parent):** expand remaining hashed imports;
   result is import-free, type-checked, **not** fully normalized.

Parser: extend `dhall.abnf` and `Parser.hs`:

```text
import = import-hashed [ whsp1 as whsp1 (Text / Location / Bytes / Source) ]
```

`Syntax.ImportMode`: add `Source`. Binary: encode/decode mode integer **4**
unless the as-Source PR’s `binary.md` says otherwise (comment next to
`encode`).

In-memory reuse key includes mode: `./foo.dhall` ≠ `./foo.dhall as Source`.

## Tests that must pass

Whatever `as Source` fixtures exist on the standard PR / issue 1428. If
none are in this checkout, add a **minimal** pair under
`tests/import/success/unit/` illustrating:

- unhashed child inlined without normalizing `let x = 1 in x`
- hashed child preserved in the cache product (document how you observe
  that if the public B file is the phase-2 result)

Also keep A22 tests green. Parser success/failure for `as Source` syntax
once ABNF changes: add or enable matching parser tests from the PR.

## Done when

`as Source` round-trips through parse, encode/decode, and resolution as
specified. Existing import tests still pass.
