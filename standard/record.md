# Record syntactic sugar

Dhall records permit duplicate record fields, like this:

```dhall
{ x = { y = 1 }, x = { z = 1 } }
```

Duplicate record fields like the above desugar to the use of the `∧` operator
to merge those duplicate occurrences.  For example, the above Dhall
expression desugars to:

```dhall
{ x = { y = 1 } ∧ { z = 1 } }
```

... which in turn β-normalizes to:

```dhall
{ x = { y = 1, z = 1 } }
```

Desugaring to the `∧` operator ensures that duplicate fields may not collide.
For example, these expression are all ill-typed:

```dhall
-- Desugars to { x = 0 ∧ 0 } which is ill-typed
{ x = 0, x = 0 }

-- Desugars to { x = 0 ∧ { y = 1 } } which is ill-typed
{ x = 0, x = { y = 1 } }

-- Desugars to { x = { y = 1 } ∧ { y = 1 } } which is ill-typed
{ x = { y = 1 }, x = { y = 1 } }
```

This desugaring behavior is governed by the following judgment that translates
duplicate field occurrences to the `∧` operator:

    desugar-duplicates(r₀) = r₁

... where:

* `r₀` (the input) is a record literal potentially containing duplicate fields
* `r₁` (the output) is a record literal free of duplicate fields

The desugaring behavior operates as follows:


    desugar-duplicates({ x = v₀ ∧ v₁, xs₀… }) = r
    ────────────────────────────────────────────────  ; Inductive case for when
    desugar-duplicates({ x = v₀, x = v₁, xs₀… }) = r  ; duplicates are present


    desugar-duplicates({ xs₀… }) = { xs₁… }                ; Base case for when
    ─────────────────────────────────────────────────────  ; `x` has no more
    desugar-duplicates({ x = v, xs₀… }) = { x = v, xs₁… }  ; duplicates left


    ─────────────────────────────  ; Base case for when the record has no more
    desugar-duplicates({=}) = {=}  ; keys left


Take care to combine duplicate keys in the correct order and the correct
associativity.  For example, the following expression:

```dhall
  λ(a : { x : Natural })
→ λ(b : { y : Natural })
→ λ(c : { z : Natural })
→ { k = a, k = b, k = c }
```

... should desugar to this:

```dhall
  λ(a : { x : Natural })
→ λ(b : { y : Natural })
→ λ(c : { z : Natural })
→ { k = (a ∧ b) ∧ c }
{-  Note that the parentheses are not necessary since all operators are
    left-associative, but they are included for clarity
-}
```
