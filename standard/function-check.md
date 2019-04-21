# Function check

The function check governs the types of functions that our pure type system
permits.  This is based on [CCω][ccw] with only three universes:

* `Type` is an impredicative universe at the bottom of the hierarchy
  (equivalent to `*` from the linked paper)
* `Kind` is the first predicate universe (equivalent to `□₀`)
* `Sort ` is the second predicate universe (equivalent to `□₁`)

This function check is a judgment of the form:

    c₀ ↝ c₁ : c₂

... where:

* `c₀` (an input constant, either `Type`, `Kind`, or `Sort`) is the type of the
  function's input type
* `c₁` (an input constant, either `Type`, `Kind`, or `Sort`) is the type of the
  function's output type
* `c₂` (an output constant, either `Type`, `Kind`, or `Sort`) is the type of
  the function's type

Functions that return terms are impredicative:


    ───────────────
    c ↝ Type : Type


When `c = Type` you get functions from terms to terms (i.e.  "term-level"
functions):


    ──────────────────
    Type ↝ Type : Type


For example, these are term-level functions permitted by the above rule:

    Natural/even

    λ(x : Bool) → x != False

When `c = Kind` you get functions from types to terms (i.e.  "type-polymorphic"
functions):


    ──────────────────
    Kind ↝ Type : Type


For example, these are type-polymorphic functions permitted by the above rule:

    List/head

    λ(a : Type) → λ(x : a) → x

When `c = Sort` you get functions from sorts to terms:


    ──────────────────
    Sort ↝ Type : Type


For example, this is a (trivial) function from a sort to a term:

    λ(k : Kind) → 1

All the remaining function types are predicative:


    ────────────  ; c₁ ≥ c₀, c₂ = max(c₀, c₁), Type < Kind < Sort
    c₀ ↝ c₁ : c₂


When `c₀ = Kind` and `c₁ = Kind` you get functions from types to types (i.e.
"type-level" functions):


    ──────────────────
    Kind ↝ Kind : Kind


For example, these are type-level functions permitted by the above rule:

    List

    λ(m : Type) → [ m ] → m

When `c₀ = Sort` and `c₁ = Kind` you get functions from kinds to types (i.e.
"kind-polymorphic" functions):


    ──────────────────
    Sort ↝ Kind : Sort


For example, this is a kind-polymorphic function permitted by the above rules:

    λ(k : Kind) → λ(a : k) → a

When `c₀ = Sort` and `c₁ = Sort` you get functions from kinds to kinds (i.e.
"kind-level" functions):


    ──────────────────
    Sort ↝ Sort : Sort


For example, this is a kind-level function permitted by the above rule:

    λ(a : Kind) → a → a

However, Dhall does not support dependently-typed functions, so there are no
rules for `Type ↝ Kind`, `Kind → Sort`, or `Type → Sort`.  Dhall omits support
for dependent function types because that would entail robustly detecting
non-trivial type-level equivalences.

[ccw]: https://hal.inria.fr/hal-01445835
