# Semantics

This document formalizes the semantics for resolving, type-checking and
normalizing Dhall expressions.

Note that this document *does not* specify how a language binding marshals an
normalized Dhall expression into a matching expression in the host language.
The details of how to do so are left open to each implementation, including
supported integer ranges or how to idiomatically encode unions.

## Table of contents

* [Summary](#summary)
* [Expressions](#expressions)
* [Notation for induction](#notation-for-induction)
* [Shift](#shift)
* [Contexts](#contexts)
* [Shift context](#shift-context)
* [Substitution](#substitution)
* [α-normalization](#α-normalization)
* [β-normalization](#β-normalization)
* [Equivalence](#equivalence)
* [Function check](#function-check)
* [Type inference](#type-inference)
* [Binary encoding and decoding](#binary-encoding-and-decoding)
* [Import resolution](#import-resolution)

## Summary

Dhall's type system is a variation on [CCω][ccw], implemented using a pure type
system (see the ["Function check"](#function-check) section for more details).
Type abstraction and type application are explicit and not inferred.  Dhall also
supports additional built-in functions, operators, and constants for efficiency.

Dhall also supports referencing shadowed variables through the use of DeBruijn
indices.  This document spells out in detail how to implement these
DeBruijn-like variable references.

## Expressions

The following notation is a simplified version of the syntax found in
`./dhall.abnf`.  This simplified notation is used for all of the following
judgments:

```
m, n = 0 / 1 + n  ; Natural numbers

d = ±n            ; Integers

x, y              ; Variables

; Mnemonics for the most commonly used labels:
;
; Terms are lowercase:
;
;     a    = input term whose type is "A"
;     b    = output term whose type is "B"
;     f    = "f"unction
;     l, r = "l"eft and "r"ight term that share the same type
;     e    = term whose type is "E"
;     t    = term whose type is "T"
;     u    = term whose type is "U"
;
; Types are uppercase:
;
;     A  = type of the input term "a"
;     B  = type of the output term "b"
;     E  = type of the term "e"
;     T  = type of the term "t"
;     U  = type of the term "u"
;
; Constants that are `Type`, `Kind`, or `Sort` are lowercase:
;
;     c = "c"onstant
;     i = function's "i"nput type
;     o = function's "o"utput type
;
; Similar terms are distinguished by subscripts like `a₀`, `a₁`, …
;
; A term that represents zero or more values or key-value pairs ends with `s…`,
; such as `as…`
;
; Note that these are only informal mnemonics.  Dhall is a pure type system,
; which means that many places in the syntax permit terms, types, kinds, and
; sorts. The typing judgments are the authoritative rules for what expressions
, are permitted and forbidden.
a, b, f, l, r, e, t, u, A, B, E, T, U, c, i, o
  = x@n                               ; Identifier
                                      ; (`x` is short-hand for `x@0`)
  / λ(x : A) → b                      ; Anonymous function
  / ∀(x : A) → B                      ; Function type
                                      ; (`A → B` is short-hand for `∀(_ : A) → B`)
  / let x : A = a in b                ; Let expression with type annotation
  / let x     = a in b                ; Let expression without type annotation
  / if t then l else r                ; if-then-else expression
  / merge t u : T                     ; Union elimination with type annotation
  / merge t u                         ; Union elimination
  / [] : List T                       ; Empty list literals with type annotation
  / [ t, ts… ]                        ; Non-empty list literals
  / [   ] : Optional T                ; Empty optional literal
  / [ t ] : Optional T                ; Non-empty optional literal
  / t : T                             ; Type annotation
  / l || r                            ; Boolean or
  / l + r                             ; Natural addition
  / l ++ r                            ; Text append
  / l # r                             ; List append
  / l && r                            ; Boolean and
  / l ∧ r                             ; Recursive record merge
  / l ⫽ r                             ; Non-recursive right-biased record merge
  / l ⩓ r                             ; Recursive record type merge
  / l * r                             ; Natural multiplication
  / l == r                            ; Boolean equality
  / l != r                            ; Boolean inequality
  / f a                               ; Function application
  / t.x                               ; Field selection
  / t.{ xs… }                         ; Field projection
  / n.n                               ; Double-precision floating point literal
  / n                                 ; Natural number literal
  / ±n                                ; Integer literal
  / "s"                               ; Uninterpolated text literal
  / "s${t}ss…"                        ; Interpolated text literal
  / {}                                ; Empty record type
  / { x : T, xs… }                    ; Non-empty record type
  / {=}                               ; Empty record literal
  / { x = t, xs… }                    ; Non-empty record literal
  / <>                                ; Empty union type
  / < x : T | xs… >                   ; Union type with at least one non-empty
                                      ; alternative
  / < x | xs… >                       ; Union type with at least one empty
                                      ; alternative
  / < x = t >                         ; Union literal with one alternative
  / < x₀ = t₀ | x₁ : T₁ | xs… >       ; Union literal with more than one
                                      ; alternative
  / missing                           ; Identity for import alternatives,
                                      ; will always fail to resolve
  / l ? r                             ; Alternative imports resolution
  / https://authority directory file  ; URL import
  / path file                         ; Absolute file path import
  / . path file                       ; Relative file path import
  / .. path file                      ; Relative file path import
  / ~ path file                       ; Home-anchored file path import
  / env:x                             ; Environment variable import
  / Some a                            ; Constructor for a present Optional value

                                      ; Reserved identifiers for builtins
  / Natural/build                     ; Natural introduction
  / Natural/fold                      ; Natural elimination
  / Natural/isZero                    ; Test if zero
  / Natural/even                      ; Test if even
  / Natural/odd                       ; Test if odd
  / Natural/toInteger                 ; Convert Natural to Integer
  / Natural/show                      ; Convert Natural to Text representation
  / Integer/toDouble                  ; Convert Integer to Double
  / Integer/show                      ; Convert Integer to Text representation
  / Double/show                       ; Convert Double to Text representation
  / List/build                        ; List introduction
  / List/fold                         ; List elimination
  / List/length                       ; Length of list
  / List/head                         ; First element of list
  / List/last                         ; Last element of list
  / List/indexed                      ; Tag elements with index
  / List/reverse                      ; Reverse list
  / Optional/build                    ; Optional introduction
  / Optional/fold                     ; Optional elimination
  / Text/show                         ; Convert Text to its own representation
  / Bool                              ; Bool type
  / Optional                          ; Optional type
  / Natural                           ; Natural type
  / Integer                           ; Integer type
  / Double                            ; Double type
  / Text                              ; Text type
  / List                              ; List type
  / True                              ; True term
  / False                             ; False term
  / None                              ; Absent Optional value
  / Type                              ; Type of terms
  / Kind                              ; Type of types
  / Sort                              ; Type of kinds
```

## Notation for induction

This document uses a non-standard `…` notation for distinguishing list elements
or key-value pairs in records/unions from other expression types,
records, and unions:

```
t          : Naked label which could be any type of expression.

[ ts…  ]   : A list with 0 or more values.
[ t, ts… ] : A list with 1 or more values.  The first value is `t`.

{ xs… }        : A record type or record value with 0 or more fields.
{ x : T, xs… } : A record type with 1 or more field-type pairs.  At least one
                 field is named `x` with a type of `T`.
{ x = t, xs… } : A record value with 1 or more field-value pairs.  At least one
                 field is named `x` with a value of `t`.

< xs… >                    : A union type with 0 or more alternative-type pairs.
< x : T | xs… >            : A union type with 1 or more alternative-type pairs.
                             At least one alternative is named `x` with a type
                             of `T`.
< x | xs… >                : A union type with 1 or more empty alternatives
                             At least one alternative is named `x`, which is an
                             empty alternative
< x = t | xs… >            : A union literal with 0 or more alternative-type
                             pairs.  The specified alternative is named `x` with
                             value of `t`.
< x₀ = t₀ | x₁ : T₁, xs… > : A union literal with 1 or more alternative-type
                             pairs.  The specified alternative is named `x₀ with
                             value of `t₀`.  At least one alternative is named
                             `x₁` with a type of `T₁`.


let xs… in b                : A `let` definition with at least one bindings
let x : A = a let xs… in b  : A `let` definition with at least two bindings

"s"           : A `Text` literal without any interpolated expressions
"s${t}ss…"    : A `Text` literal with at least one interpolated expression

''
s''           : A multi-line `Text` literal with only one line

''
ss
s''           : A multi-ine `Text` literal with more than one line
```

You will see this notation in judgments that perform induction on lists,
records, or unions.  For example, the following judgment for normalizing a
non-empty list says that to normalize a list you normalize the head of the list
and then normalize the tail:


    t₀ ⇥ t₁   [ ts₀… ] ⇥ [ ts₁… ]
    ─────────────────────────────
    [ t₀, ts₀… ] ⇥ [ t₁, ts₁… ]


Note that this notation does not imply that implementations must use induction
or inductive data structures (like linked lists) to implement lists, records, or
unions.  Implementations may freely use more efficient data structures like
arrays or dictionaries, so long as they behave the same.

## Multi-line string literals

Dhall's grammar supports multi-line string literals, such as:

    ''
    foo
    bar
    ''

These multi-line string literals are syntactic sugar for ordinary double-quoted
string literals and the conversion from multi-line string literals double-quoted
string literals occurs at parse time.

For example, the above multi-line string literal is parsed as:

    "foo\nbar\n"

Because this conversion occurs at parse-time all of the following judgments
only deal with double-quoted string literals.  Consequently, there are no
separate rules for type-checking or normalizing multi-line string literals.

You can find the logic for desugaring multi-line string literals to
double-quoted string literals in the following separate document:

* [Multi-line literal semantics](./multiline.md)

## Shift

You can find the shift semantics in the following
separate document:

* [Shift](./shift.md)


## Contexts

The syntax of contexts is:

    Γ = ε         ; The empty context
      / Γ, x : T  ; A context extended with a type annotation for a variable

Contexts are ordered and there can be multiple type annotations in the context
for the same variable.  The DeBruijn index associated with each variable
disambiguates which variable to refer to in the context.

## Shift context

You can also shift a context by shifting each expression in that context:


    ─────────────────
    ↑(d, x, m, ε) = ε


    ↑(d, x, m, Γ₀) = Γ₁   ↑(d, x, m, T₀) = T₁
    ─────────────────────────────────────────
    ↑(d, x, m, (Γ₀, y : T₀)) = Γ₁, y : T₁


## Substitution

You can find the substitution semantics in the following
separate document:

* [Substitution](./substitution.md)


## α-normalization

You can find the α-normalization semantics in the following
separate document:

* [α-normalization](./α-normalization.md)


## β-normalization

You can find the β-normalization semantics in the following
separate document:

* [β-normalization](./β-normalization.md)


## Equivalence

Equivalence is a relationship between two expression of the form:


    l ≡ r


Two expressions are equivalent if they are identical after β-normalization,
α-normalization, and binary encoding:


    l₀ ⇥ l₁   l₁ ↦ x   encode(x) = b   r₀ ⇥ r₁   r₁ ↦ y   encode(y) = b
    ───────────────────────────────────────────────────────────────────
    l₀ ≡ r₀


Note that this definition of equivalence does not include η-equivalence, so
`λ(f : Bool → Bool) → λ(x : Bool) → f x` and `λ(f : Bool → Bool) → f` are not
equivalent.

Note also that this means that `Double`s should not be compared using standard float equality.

## Function check

The function check governs which types of functions that our pure type system
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

## Type inference

You can find the type inference semantics in the following
separate document:

* [Type inference](./type-inference.md)


## Binary encoding and decoding

You can find the binary encoding and decoding semantics in the following
separate document:

* [Binary semantics](./binary.md)


## Import resolution

You can find the import resolution semantics in the following
separate document:

* [Import resolution semantics](./imports.md)


[ccw]: https://hal.inria.fr/hal-01445835
