# A18 — `freeVars` and remaining merge tests

Depends on: A17  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/type-inference.md` — “Detecting free variables”. The document
uses `…` ellipses: fill them by the closing rule (union of `freeVars` of
immediate subterms) plus the special cases already written (literals,
builtins, constants, imports without headers, variables, λ/∀/let + shift).

There is a likely typo in the unannotated-let conclusion (`V₃` vs `V₄`);
implement the **premises** (union of body-minus-x and bound expression),
not the inconsistent label.

## Tests that must pass

```text
tests/type-inference/failure/unit/MergeHandlerFreeVar.dhall
```

and any merge tests skipped in A15. Plus all type-inference unit tests
from A11–A17.

## Done when

`freeVars` lives in `type-inference.md`. MergeHandlerFreeVar passes.
