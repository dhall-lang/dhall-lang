# A15 — Type inference: unions, merge, toMap, showConstructor

Depends on: A14  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/type-inference.md` — Unions; merge; toMap (and showConstructor
if specified there or as a builtin). `freeVars` for merge handlers is
**A18** — if a merge test fails for that reason, skip it until A18 and
name it in the log.

## Tests that must pass

Success `tests/type-inference/success/unit/`:

```text
Union*
Merge*
ToMap*
ShowConstructor*
```

Failure `tests/type-inference/failure/unit/`:

```text
Union*
Merge*                 # except MergeHandlerFreeVar until A18
*ToMap*
ShowConstructor*
```

Plus A11–A14.

## Done when

Those tests pass, with `MergeHandlerFreeVar` optionally deferred to A18.
