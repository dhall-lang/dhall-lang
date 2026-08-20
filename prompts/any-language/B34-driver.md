# B34 — Driver (CLI or library entry)

Depends on: B32  
Read first: `prompts/any-language/00-shared.md`

## Goal

One entry point:

```text
parse → resolve (cwd fake-root) → type-check → β-normalize
```

Optional: dump CBOR / diagnostic notation / semantic hash. Pretty-print
of Dhall source is optional and untested.

Non-zero exit on parse, import, or type error.

## Tests that must pass

Full `tests/` still green. Manual: evaluate `True && False` → `False`.

## Done when

The driver is documented (README in your project, not dhall-lang).
