# Record syntactic sugar

There are two desugaring steps applied to records as they parsed, before they
are further interpreted:

* First, dotted syntax like `{ x.y = a }` desugars to `{ x = { y = a } }`
* Second, duplicate fields like `{ x = a, x = b }` desugar to `{ x = a ∧ b }`

The following example illustrates how these two features interact:

```dhall
{ x.y = a, x.z = b }
```

The above expression first desugars to:

```dhall
{ x = { y = a }, x = { z = b } }
```

... which then further desugars to:

```dhall
{ x = { y = a } ∧ { z = b } }
```

... which eventually β-normalizes to:

```dhall
{ x = { y = a, z = b } }
```

Additionally, the language supports *record-like* syntax for updating a record
using the `with` keyword, like this:

```dhall
record with { x.y.z = a }
```

... which desugars to:

```dhall
record // { x = record.x // { y = record.x.y // { z = a } } }
```

The right argument to the `with` keyword resembles the same syntax as a record,
but the similarity is only superficial.  For example, the following expression:

```dhall
record with { x = { y = { z = a } } }
```

... is not the same as the previous example, since it desugars to:

```dhall
record // { x = { y = { z = a } } }
```

... which is not the same thing.

In other words, expanding out the dotted label syntax is not a
behavior-preserving change because the `with` keyword gives special treatment
to the dotted labels.

## Dotted syntax

Dhall records permit dot-separated field labels to specify nested records, like
this:

```dhall
{ x.y.z = a }
```

Dot-separated fields like the above desugar to nested records.  For example,
the above Dhall expression desugars to:

```dhall
{ x = { y = { z = a } } }
```

The above translation is governed by the following judgment which converts
a record containing dot-separated fields to a record without dot-separated
fields:

    desugar-dotted-fields(r₀) = r₁

... where:

* `r₀` (the input) is a record literal potentially containing dot-separated
  fields
* `r₁` (the output) is a record literal free of dot-separated fields

The rules for the translation are:

    desugar-dotted-fields({ xs… = { x = e }, ys… }) = r   ; Induction: More than
    ────────────────────────────────────────────────────  ; one dot-separated
    desugar-dotted-fields({ xs….x = e, ys… }) = r         ; field


    desugar-dotted-fields({ ys₀… }) = { ys₁… }                ; Base case for
    ────────────────────────────────────────────────────────  ; no more
    desugar-dotted-fields({ x = e, ys₀… }) = { x = e, ys₁… }  ; remaining dots


    ───────────────────────────────────────────────────────  ; Base care for an
    desugar-dotted-fields({=}) = {=}                         ; empty record


## Duplicate fields

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

## `with` keyword

You can use the `with` keyword for ergonomic updates to deeply-nested records,
like this:

```dhall
let record = { a.b = { c = 1, d = True } }

in  record with { a.b.d = False, a.b.e = 2.0 }
```

... which evaluates to:

```dhall
{ a.b = { c = 1, d = False, e = 2.0 } }
```

This desugaring uses the following judgment that translates the `with` keyword to
the `//` operator:

    desugar-with(e₀ with us) = e₁

... where:

* `e₀` (an input) is an expression representing a record to update
* `us` (an input) is a record-like update clause
* `e₁` (the output) is the desugared expression

Desugaring a `with` keyword with multiple updates is the same as multiple
chained uses of the `with` keyword:


    desugar-with(e₀ with { ks₀ = v₀}) = e₁
    desugar-with(e₁ with { ks₁ = v₁, kvs… }) = e₂            ; Inductive case for
    ───────────────────────────────────────────────────────  ; more than one
    desugar-with(e₀ with { ks₀ = v₀, ks₁ = v₁, kvs… }) = e₂  ; update


Or informally, this expression:

```dhall
r with { x = a, y = b }
```

... is the same as this expression:

```dhall
r with { x = a } with { y = b }
```

A `with` expression with a single update but multiple dotted labels is equivalent
to chained uses of the `//` operator:


    desugar-with(e₀ // { k₀ = e₀.k₀ with { k₁.ks… = v₀} }) = e₁  ; Inductive case
    ──────────────────────────────────────────────────────────   ; for more than
    desugar-with(e₀ with { k₀.k₁.ks… = v₀ }) = e₁                ; one label


... and if there is only one update with one label then the `with` keyword is a
synonym for the `//` operator:


    ─────────────────────────────────────────────────  ; Base case for exactly
    desugar-with(e₀ with { k = v }) = e₀ // { k = v }  ; one label
