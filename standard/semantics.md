# Semantics

This document formalizes the semantics for type-checking and normalizing Dhall
expressions.

## Table of contents

* [Summary](#summary)
* [Expressions](#expressions)
* [Notation for induction](#notation-for-induction)
* [Shift](#shift)
    * [Variables](#variables)
    * [Bound variables](#bound-variables)
    * [Imports](#imports)
    * [Other](#other)
* [Contexts](#contexts)
* [Shift context](#shift-context)
* [Substitution](#substitution)
    * [Variables](#variables-1)
    * [Bound variables](#bound-variables-1)
    * [Imports](#imports-1)
    * [Other](#other-1)
* [α-normalization](#α-normalization)
    * [Bound variables](#bound-variables-2)
    * [Variables](#variables-2)
    * [Imports](#imports-2)
    * [Other](#other-2)
* [β-normalization](#β-normalization)
    * [Constants](#constants)
    * [Variables](#variables-3)
    * [`Bool`](#bool)
    * [`Natural`](#natural)
    * [`Text`](#text)
    * [`List`](#list)
    * [`Optional`](#optional)
    * [Records](#records)
    * [Unions](#unions)
    * [`Integer`](#integer)
    * [`Double`](#double)
    * [Functions](#functions)
    * [`let` expressions](#let-expressions)
    * [Type annotations](#type-annotations)
    * [Imports](#imports-3)
* [Text literals](#text-literals)
    * [Interpolation](#interpolation)
    * [Canonicalization of text](#canonicalization-of-text)
    * [Single-quote text literals](#single-quote-text-literals)
* [Equivalence](#equivalence)
* [Function check](#function-check)
* [Type inference](#type-inference)
    * [Reduction](#reduction)
    * [Constants](#constants-1)
    * [Variables](#variables-4)
    * [`Bool`](#bool-1)
    * [`Natural`](#natural-1)
    * [`Text`](#text-1)
    * [`List`](#list-1)
    * [`Optional`](#optional-1)
    * [Records](#records-1)
    * [Unions](#unions-1)
    * [`Integer`](#integer-1)
    * [`Double`](#double-1)
    * [Functions](#functions-1)
    * [`let` expressions](#let-expressions-1)
    * [Type annotations](#type-annotations-1)
    * [Imports](#imports-4)
* [Import resolution](#import-resolution)
    * [Directories and files](#directories-and-files)
    * [Canonicalization of directories](#canonicalization-of-directories)
    * [Canonicalization of imports](#canonicalization-of-imports)
    * [Chaining directories](#chaining-directories)
    * [Chaining imports](#chaining-imports)
    * [Duplicate imports](#duplicate-imports)
    * [Import resolution judgment](#import-resolution-judgment)

## Summary

Dhall's type system is System Fω implemented using a pure type system.  Type
abstraction and type application are explicit and not inferred.  Dhall also
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
; Constants that are either `Type` or `Kind` are lowercase:
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
; which means that many places in the syntax permit types, terms, and kinds.
; The typing judgments are the authoritative rules for what expressions are
; permitted and forbidden.
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
  / "…"                               ; Text literal
  / {}                                ; Empty record type
  / { x : T, xs… }                    ; Non-empty record type
  / {=}                               ; Empty record literal
  / { x = t, xs… }                    ; Non-empty record literal
  / <>                                ; Empty union type
  / < x : T | xs… >                   ; Non-empty union type
  / < x = t >                         ; Union literal with one alternative
  / < x₀ = t₀ | x₁ : T₁	| xs… >       ; Union literal with more than one
                                      ; alternative
  / constructors u                    ; Make record of constructors from union
                                      ; type
  / Natural/build                     ; Natural introduction
  / Natural/fold                      ; Natural elimination
  / Natural/isZero                    ; Test if zero
  / Natural/even                      ; Test if even
  / Natural/odd                       ; Test if odd
  / Natural/toInteger                 ; Convert Natural to Integer
  / Natural/show                      ; Convert Natural to Text
  / Integer/toDouble                  ; Convert Integer to Double
  / Integer/show                      ; Convert Integer to Text
  / Double/show                       ; Convert Double to Text
  / List/build                        ; List introduction
  / List/fold                         ; List elimination
  / List/length                       ; Length of list
  / List/head                         ; First element of list
  / List/last                         ; Last element of list
  / List/indexed                      ; Tag elements with index
  / List/reverse                      ; Reverse list
  / Optional/fold                     ; Optional introduction
  / Optional/build                    ; Optional elimination
  / Bool                              ; Bool type
  / Optional                          ; Optional type
  / Natural                           ; Natural type
  / Integer                           ; Integer type
  / Double                            ; Double type
  / Text                              ; Text type
  / List                              ; List type
  / True                              ; True term
  / False                             ; False term
  / Type                              ; Type of terms
  / Kind                              ; Type of types
  / missing                           ; Identity for import alternatives,
                                      ; will always fail to resolve
  / l ? r                             ; Alternative imports resolution
  / https://authority directory file  ; URL import
  / path file                         ; Absolute file path import
  / . path file                       ; Relative file path import
  / .. path file                      ; Relative file path import
  / ~ path file                       ; Home-anchored file path import
  / env:x                             ; Environment variable import

```

You can treat string interpolation as syntactic sugar for `Text` concatenation.
In other words, the following string:

```
"foo${bar}baz"
```

... would be equivalent to:

```
"foo" ++ bar ++ "baz"
```

... for both type-checking and normalization purposes.

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
< x = t | xs… >            : A union literal with 0 or more alternative-type
                             pairs.  The specified alternative is named `x` with
                             value of `t`.
< x₀ = t₀ | x₁ : T₁, xs… > : A union literal with 1 or more alternative-type
                             pairs.  The specified alternative is named `x₀ with
                             value of `t₀`.  At least one alternative is named
                             `x₁` with a type of `T₁`.
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

## Shift

Dhall allows variables to reference shadowed variables of the same name using De
Bruijn indices.  For example:


                                  ┌──refers to──┐
                                  │             │
                                  ↓             │
    λ(x : Type) → λ(y : Type) → λ(x : Type) → x@0


      ┌────────────────refers to────────────────┐
      │                                         │
      ↓                                         │
    λ(x : Type) → λ(y : Type) → λ(x : Type) → x@1


`x@n` refers to the "nth" bound variable named `x` counting outwards from where
the variable is referenced.

If a variable does not specify the De Bruijn index (i.e. just `x`) then the De
Bruijn index defaults to 0 (i.e. `x@0`), like this:


                                  ┌─refers to─┐
                                  │           │
                                  ↓           │
    λ(x : Type) → λ(y : Type) → λ(x : Type) → x



Dhall uses a shift function internally to avoid variable capture in the
implementation of De Bruijn indices.  This function increases or decreases the
indices of free variables within an expression.

This shift function has the form:

    ↑(d, x, m, e₀) = e₁

... where:

* `d` (an input integer) is the amount to add to the variable indices
    * `d` is always `-1` or `1`
* `x` (an input variable name) is the name of the free variable(s) to shift
    * variables with a different name are unaffected by the shift function
* `m` (an input natural number) is the minimum index to shift
    * `m` always starts out at `0`
    * `m` increases by one when descending past a bound variable named `x`
    * variables with an index lower than `m` are unaffected by the shift
      function
* `e₀` (an input expression) is the expression to shift
* `e₁` (the output expression) is the shifted expression

### Variables

The first rule is that the shift function will increase the index of any
variable if the variable name matches and the variable's index is greater than
or equal to the minimum index to shift.  For example:

    ↑(1, x, 0, x) = x@1   ; `x = x@0` and increasing the index by 1 gives `x@1`

    ↑(1, x, 1, x) = x     ; No shift, because the index was below the cutoff

    ↑(1, x, 0, y) = y     ; No shift, because the variable name did not match

    ↑(-1, x, 0, x@1) = x  ; Example of negative shift

Formally, shift the variable if the name matches and the index is at least as
large as the lower bound:


    ───────────────────────────  ; m <= n
    ↑(d, x, m, x@n) = x@(n + d)


Don't shift the index if the index falls short of the lower bound:


    ─────────────────────  ; m > n
    ↑(d, x, m, x@n) = x@n


Also, don't shift the index if the variable name does not match:


    ─────────────────────  ; x ≠ y
    ↑(d, x, m, y@n) = y@n


### Bound variables

The shift function is designed to shift only free variables when `m = 0`.  For
example, the following shift usage has no effect because `m = 0` and the
expression is "closed" (i.e. no free variables):

    ↑(1, x, 0, λ(x : Type) → x) = λ(x : Type) → x

    ↑(1, x, 0, ∀(x : Type) → x) = ∀(x : Type) → x

    ↑(1, x, 0, let x = 1 in x) = let x = 1 in x

... whereas the following shift usage has an effect on expressions with free
variables:

    ↑(1, x, 0, λ(y : Type) → x) = λ(y : Type) → x@1

    ↑(1, x, 0, ∀(y : Type) → x) = ∀(y : Type) → x@1

    ↑(1, x, 0, let y = 1 in x) = let y = 1 in x@1

Increment the minimum bound when descending into a λ-expression that binds a
variable of the same name in order to avoid shifting the bound variable:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m + 1, b₀) = b₁
    ─────────────────────────────────────────────
    ↑(d, x, m, λ(x : A₀) → b₀) = λ(x : A₁) → b₁


Note that the bound variable, `x`, is not in scope for its own type, `A₀`, so
do not increase the lower bound, `m`, when shifting the bound variable's type.

Descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, λ(y : A₀) → b₀) = λ(y : A₁) → b₁


Function types also introduce bound variables, so increase the minimum bound
when descending past such a bound variable:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m + 1, B₀) = B₁
    ─────────────────────────────────────────────
    ↑(d, x, m, ∀(x : A₀) → B₀) = ∀(x : A₁) → B₁


Again, the bound variable, `x`, is not in scope for its own type, `A₀`, so do
not increase the lower bound, `m`, when shifting the bound variable's type.

Descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, B₀) = B₁
    ───────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, ∀(y : A₀) → B₀) = ∀(y : A₁) → B₁


`let` expressions also introduce bound variables, so increase the minimum bound
when descending past such a bound variable:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, a₀) = a₁   ↑(d, x, m + 1, b₀) = b₁
    ───────────────────────────────────────────────────────────────────
    ↑(d, x, m, let x : A₀ = a₀ in b₀) = let x : A₁ = a₁ in b₁


    ↑(d, x, m, a₀) = a₁   ↑(d, x, m + 1, b₀) = b₁
    ───────────────────────────────────────────────
    ↑(d, x, m, let x = a₀ in b₀) = let x = a₁ in b₁


Again, the bound variable, `x`, is not in scope for its own type, `A₀`, so do
not increase the lower bound, `m`, when shifting the bound variable's type.

Note that Dhall's `let` expressions do not allow recursive definitions so the
bound variable, `x`, is not in scope for the right-hand side of the assignment,
`a₀`, so do not increase the lower bound, `m`, when shifting the right-hand side
of the assignment.

Descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, a₀) = a₁   ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, let y : A₀ = a₀ in b₀) = let y : A₁ = a₁ in b₁


    ↑(d, x, m, a₀) = a₁   ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, let y = a₀ in b₀) = let y = a₁ in b₁


### Imports

You can shift expressions with unresolved imports because the language enforces
that imported values must be closed (i.e. no free variables) and shifting a
closed expression has no effect:


    ─────────────────────────────────
    ↑(d, x, m, path file) = path file


    ─────────────────────────────────────
    ↑(d, x, m, . path file) = . path file


    ───────────────────────────────────────
    ↑(d, x, m, .. path file) = .. path file


    ─────────────────────────────────────
    ↑(d, x, m, ~ path file) = ~ path file


    ─────────────────────────────────────────────────────────────────────
    ↑(d, x, m, https://authority path file) = https://authority path file


    ─────────────────────────
    ↑(d, x, m, env:x) = env:x


### Other

No other Dhall expressions bind variables, so the shift function descends into
sub-expressions in those cases, like this:

    ↑(1, x, 0, List x) = List x@1

The remaining rules are:


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ───────────────────────────────────────────────────────────────
    ↑(d, x, m, if t₀ then l₀ else r₀) = if t₁ then l₁ else r₁


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, u₀) = u₁   ↑(d, x, m, T₀) = T₁
    ───────────────────────────────────────────────────────────────
    ↑(d, x, m, merge t₀ u₀ : T₀) = merge t₁ u₁ : T₁


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, u₀) = u₁
    ─────────────────────────────────────────
    ↑(d, x, m, merge t₀ u₀) = merge t₁ u₁


    ↑(d, x, m, T₀) = T₁
    ───────────────────────────────────────
    ↑(d, x, m, [] : List T₀) = [] : List T₁


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, [ ts₀… ]) = [ ts₁… ]
    ─────────────────────────────────────────────────────
    ↑(d, x, m, [ t₀, ts₀… ]) = [ t₁, ts₁… ]


    ↑(d, x, m, T₀) = T₁
    ───────────────────────────────────────────────
    ↑(d, x, m, [] : Optional T₀) = [] : Optional T₁


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, T₀) = T₁
    ───────────────────────────────────────────────────────
    ↑(d, x, m, [ t₀ ] : Optional T₀) = [ t₁ ] : Optional T₁


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, T₀) = T₁
    ─────────────────────────────────────────
    ↑(d, x, m, t₀ : T₀) = t₁ : T₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ || r₀) = l₁ || r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ + r₀) = l₁ + r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ ++ r₀) = l₁ ++ r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ # r₀) = l₁ # r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ && r₀) = l₁ && r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ ∧ r₀) = l₁ ∧ r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ ⫽ r₀) = l₁ ⫽ r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ ⩓ r₀) = l₁ ⩓ r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ * r₀) = l₁ * r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ == r₀) = l₁ == r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ != r₀) = l₁ != r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ ? r₀) = l₁ ? r₁


    ↑(d, x, m, f₀) = f₁   ↑(d, x, m, a₀) = a₁
    ─────────────────────────────────────────
    ↑(d, x, m, f₀ a₀) = f₁ a₁


    ↑(d, x, m, t₀) = t₁
    ───────────────────────
    ↑(d, x, m, t₀.x) = t₁.x


    ↑(d, x, m, t₀) = t₁
    ───────────────────────────────────
    ↑(d, x, m, t₀.{ xs… }) = t₁.{ xs… }


    ─────────────────────
    ↑(d, x, m, n.n) = n.n


    ─────────────────
    ↑(d, x, m, n) = n


    ───────────────────
    ↑(d, x, m, ±n) = ±n


    ─────────────────────
    ↑(d, x, m, "…") = "…"


    ───────────────────
    ↑(d, x, m, {}) = {}


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, { xs₀… }) = { xs₁… }
    ─────────────────────────────────────────────────────
    ↑(d, x, m, { x : T₀, xs₀… }) = { x : T₁, xs₁… }


    ─────────────────────
    ↑(d, x, m, {=}) = {=}


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, { xs₀… }) = { xs₁… }
    ─────────────────────────────────────────────────────
    ↑(d, x, m, { x = t₀, xs₀… }) = { x = t₁, xs₁… }


    ───────────────────
    ↑(d, x, m, <>) = <>


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, < xs₀… >) = < xs₁… >
    ─────────────────────────────────────────────────────
    ↑(d, x, m, < x : T₀ | xs₀… >) = < x : T₁ | xs₁… >


    ↑(d, x, m, t₀) = t₁
    ───────────────────────────────────
    ↑(d, x, m, < x = t₀ >) = < x = t₁ >


    ↑(d, x, m, T₁₀) = T₁₁
    ↑(d, x, m, < x₀ = t₀₀ | xs₀… > ) = < x₀ = t₀₁ | xs₁… >
    ───────────────────────────────────────────────────────────────────────────
    ↑(d, x, m, < x₀ = t₀₀ | x₁ : T₁₀ | xs₀… >) = < x₀ = t₀₁ | x₁ : T₁₁ | xs₁… >


    ↑(d, x, m, u₀) = u₁
    ─────────────────────────────────────────────
    ↑(d, x, m, constructors u₀) = constructors u₁


    ─────────────────────────────────────────
    ↑(d, x, m, Natural/build) = Natural/build


    ───────────────────────────────────────
    ↑(d, x, m, Natural/fold) = Natural/fold


    ───────────────────────────────────────────
    ↑(d, x, m, Natural/isZero) = Natural/isZero


    ───────────────────────────────────────
    ↑(d, x, m, Natural/even) = Natural/even


    ─────────────────────────────────────
    ↑(d, x, m, Natural/odd) = Natural/odd


    ─────────────────────────────────────────────────
    ↑(d, x, m, Natural/toInteger) = Natural/toInteger


    ───────────────────────────────────────
    ↑(d, x, m, Natural/show) = Natural/show


    ───────────────────────────────────────────────
    ↑(d, x, m, Integer/toDouble) = Integer/toDouble


    ───────────────────────────────────────
    ↑(d, x, m, Integer/show) = Integer/show


    ─────────────────────────────────────
    ↑(d, x, m, Double/show) = Double/show


    ───────────────────────────────────
    ↑(d, x, m, List/build) = List/build


    ─────────────────────────────────
    ↑(d, x, m, List/fold) = List/fold


    ─────────────────────────────────────
    ↑(d, x, m, List/length) = List/length


    ─────────────────────────────────
    ↑(d, x, m, List/head) = List/head


    ─────────────────────────────────
    ↑(d, x, m, List/last) = List/last


    ───────────────────────────────────────
    ↑(d, x, m, List/indexed) = List/indexed


    ───────────────────────────────────────
    ↑(d, x, m, List/reverse) = List/reverse


    ─────────────────────────────────────────
    ↑(d, x, m, Optional/fold) = Optional/fold


    ───────────────────────────────────────────
    ↑(d, x, m, Optional/build) = Optional/build


    ───────────────────────
    ↑(d, x, m, Bool) = Bool


    ───────────────────────────────
    ↑(d, x, m, Optional) = Optional


    ─────────────────────────────
    ↑(d, x, m, Natural) = Natural


    ─────────────────────────────
    ↑(d, x, m, Integer) = Integer


    ───────────────────────────
    ↑(d, x, m, Double) = Double


    ───────────────────────
    ↑(d, x, m, Text) = Text


    ───────────────────────
    ↑(d, x, m, List) = List


    ───────────────────────
    ↑(d, x, m, True) = True


    ─────────────────────────
    ↑(d, x, m, False) = False


    ───────────────────────
    ↑(d, x, m, Type) = Type


    ───────────────────────
    ↑(d, x, m, Kind) = Kind


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
    ↑(d, x, m, (Γ₀, x : T₀)) = Γ₁, x : T₁


## Substitution

β-reduction requires support for substitution, which has the following form:

    e₀[x@n ≔ a] = e₁

... where:

* `e₀` (an input expression) is the expression that you want to transform
* `x@n` (an input variable) is the variable that you want to substitute with
  another expression
* `a` (an input expression) is the expression that you want to substitute `x@n`
  with
* `e₁` (the output expression) is transformed expression where all occurrences
  of `x@n` have been replaced with `a`

For example:

    x[x ≔ Bool] = Bool

    y[x ≔ Bool] = y

    x[x@1 ≔ Bool] = x

    (List x)[x ≔ Bool] = List Bool

Note that `e[x ≔ a]` is short-hand for `e[x@0 ≔ a]`

### Variables

Like shifting, pay special attention to the cases that bind variables or
reference variables.

The first two rules govern when to substitute a variable with the specified
expression:


    ────────────────
    x@n[x@n ≔ e] = e


    ──────────────────  ; x@n ≠ y@m
    y@m[x@n ≔ e] = y@m


In other words, substitute the expression if the variable name and index exactly
match, but otherwise do not substitute and leave the variable as-is.

### Bound variables

The substitution function is designed to only substitute free variables and
ignore bound variables.  The following few examples can help build an intuition
for how substitution uses the numeric index of the variable to substitute:

    ; Substitution has no effect on closed expressions without free variables
    (λ(x : Text) → x)[x ≔ True] = λ(x : Text) → x

    ; Substitution can replace free variables
    (λ(y : Text) → x)[x ≔ True] = λ(y : Text) → True

    ; A variable can be still be free if the DeBruijn index is large enough
    (λ(x : Text) → x@1)[x ≔ True] = λ(x : Text) → True

    ; Descending past a matching bound variable increments the index to
    ; substitute
    (λ(x : Text) → x@2)[x@1 ≔ True] = λ(x : Text) → True

Substitution avoids bound variables by increasing the index when a new bound
variable of the same name is in scope, like this:


    …   b₀[x@(1 + n) ≔ e₁] = b₁   …
    ───────────────────────────────
    …


Substitution also avoids variable capture, like this:

    (λ(x : Type) → y)[y ≔ x] = λ(x : Type) → x@1

Substitution prevents variable capture by shifting the expression to substitute
in when *any* new bound variable (not just the variable to substitute) is in
scope, like this:


    …   ↑(1, y, 0, e₀) = e₁   …
    ───────────────────────────
    …


All of the following rules cover expressions that can bind variables:


    A₀[x@n ≔ e₀] = A₁   ↑(1, x, 0, e₀) = e₁   b₀[x@(1 + n) ≔ e₁] = b₁
    ─────────────────────────────────────────────────────────────────
    (λ(x : A₀) → b₀)[x@n ≔ e₀] = λ(x : A₁) → b₁


    A₀[x@n ≔ e₀] = A₁   ↑(1, y, 0, e₀) = e₁   b₀[x@n ≔ e₁] = b₁
    ───────────────────────────────────────────────────────────  ; x ≠ y
    (λ(y : A₀) → b₀)[x@n ≔ e₀] = λ(y : A₁) → b₁


    A₀[x@n ≔ e₀] = A₁   ↑(1, x, 0, e₀) = e₁   B₀[x@(1 + n) ≔ e₁] = B₁
    ─────────────────────────────────────────────────────────────────
    (∀(x : A₀) → B₀)[x@n ≔ e₀] = ∀(x : A₁) → B₁


    A₀[x@n ≔ e₀] = A₁   ↑(1, y, 0, e₀) = e₁   B₀[x@n ≔ e₁] = B₁
    ───────────────────────────────────────────────────────────  ; x ≠ y
    (∀(y : A₀) → B₀)[x@n ≔ e₀] = ∀(y : A₁) → B₁


    A₀[x@n ≔ e₀] = A₁
    a₀[x@n ≔ e₀] = a₁
    ↑(1, x, 0, e₀) = e₁
    b₀[x@(1 + n) ≔ e₁] = b₁
    ─────────────────────────────────────────────────────────
    (let x : A₀ = a₀ in b₀)[x@n ≔ e₀] = let x : A₁ = a₁ in b₁


    A₀[x@n ≔ e₀] = A₁
    a₀[x@n ≔ e₀] = a₁
    ↑(1, y, 0, e₀) = e₁
    b₀[x@n ≔ e₁] = b₁
    ─────────────────────────────────────────────────────────  ; x ≠ y
    (let y : A₀ = a₀ in b₀)[x@n ≔ e₀] = let y : A₁ = a₁ in b₁


    a₀[x@n ≔ e₀] = a₁   ↑(1, x, 0, e₀) = e₁   b₀[x@(1 + n) ≔ e₁] = b₁
    ─────────────────────────────────────────────────────────────────
    (let x = a₀ in b₀)[x@n ≔ e₀] = let x = a₁ in b₁


    a₀[x@n ≔ e₀] = a₁   ↑(1, y, 0, e₀) = e₁   b₀[x@n ≔ e₁] = b₁
    ───────────────────────────────────────────────────────────  ; x ≠ y
    (let y = a₀ in b₀)[x@n ≔ e₀] = let y = a₁ in b₁


### Imports

You can substitute expressions with unresolved imports because the language
enforces that imported values must be closed (i.e. no free variables) and
substitution of a closed expression has no effect:


    ────────────────────────────────
    (path file)[x@n ≔ e] = path file


    ────────────────────────────────────
    (. path file)[x@n ≔ e] = . path file


    ──────────────────────────────────────
    (.. path file)[x@n ≔ e] = .. path file


    ────────────────────────────────────
    (~ path file)[x@n ≔ e] = ~ path file


    ────────────────────────────────────────────────────────────────────
    (https://authority path file)[x@n ≔ e] = https://authority path file


    ────────────────────────
    (env:x)[x@n ≔ e] = env:x


### Other

No other Dhall expressions bind variables, so the substitution function descends
into sub-expressions in those cases, like this:

    (List x)[x ≔ Bool] = List Bool

The remaining rules are:


    t₀[x@n ≔ e] = t₁   l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ────────────────────────────────────────────────────────
    (if t₀ then l₀ else r₀)[x@n ≔ e] = if t₁ then l₁ else r₁


    t₀[x@n ≔ e] = t₁   u₀[x@n ≔ e] = u₁   T₀[x@n ≔ e] = T₁
    ──────────────────────────────────────────────────────
    (merge t₀ u₀ : T₀)[x@n ≔ e] = merge t₁ u₁ : T₁


    t₀[x@n ≔ e] = t₁   u₀[x@n ≔ e] = u₁
    ────────────────────────────────────
    (merge t₀ u₀)[x@n ≔ e] = merge t₁ u₁


    T₀[x@n ≔ e] = T₁
    ──────────────────────────────────────
    ([] : List T₀)[x@n ≔ e] = [] : List T₁


    t₀[x@n ≔ e] = t₁   [ ts₀… ][x@n ≔ e] = [ ts₁… ]
    ───────────────────────────────────────────────
    ([ t₀, ts₀… ])[x@n ≔ e] = [ t₁, ts₁… ]


    T₀[x@n ≔ e] = T₁
    ──────────────────────────────────────────────
    ([] : Optional T₀)[x@n ≔ e] = [] : Optional T₁


    t₀[x@n ≔ e] = t₁   T₀[x@n ≔ e] = T₁
    ──────────────────────────────────────────────────────
    ([ t₀ ] : Optional T₀)[x@n ≔ e] = [ t₁ ] : Optional T₁


    t₀[x@n ≔ e] = t₁   T₀[x@n ≔ e] = T₁
    ───────────────────────────────────
    (t₀ : T₀)[x@n ≔ e] = t₁ : T₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ || r₀)[x@n ≔ e] = l₁ || r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ + r₀)[x@n ≔ e] = l₁ + r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ ++ r₀)[x@n ≔ e] = l₁ ++ r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ # r₀)[x@n ≔ e] = l₁ # r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ && r₀)[x@n ≔ e] = l₁ && r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ ∧ r₀)[x@n ≔ e] = l₁ ∧ r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ ⫽ r₀)[x@n ≔ e] = l₁ ⫽ r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ ⩓ r₀)[x@n ≔ e] = l₁ ⩓ r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ * r₀)[x@n ≔ e] = l₁ * r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ == r₀)[x@n ≔ e] = l₁ == r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ != r₀)[x@n ≔ e] = l₁ != r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ ? r₀)[x@n ≔ e] = l₁ ? r₁


    f₀[x@n ≔ e] = f₁   a₀[x@n ≔ e] = a₁
    ───────────────────────────────────
    (f₀ a₀)[x@n ≔ e] = f₁ a₁


    t₀[x@n ≔ e] = t₁
    ────────────────────────
    (t₀.x₀)[x@n ≔ e] = t₁.x₀


    t₀[x@n ≔ e] = t₁
    ──────────────────────────────────
    (t₀.{ xs… })[x@n ≔ e] = t₁.{ xs… }


    ──────────────────
    n.n[x@n ≔ e] = n.n


    ──────────────
    n[x@n ≔ e] = n


    ────────────────
    ±n[x@n ≔ e] = ±n


    ──────────────────
    "…"[x@n ≔ e] = "…"


    ────────────────
    {}[x@n ≔ e] = {}


    T₀[x@n ≔ e] = T₁   { xs₀… }[x@n ≔ e] = { xs₁… }
    ───────────────────────────────────────────────
    { x₀ : T₀, xs₀… }[x@n ≔ e] = { x₀ : T₁, xs₁… }


    ──────────────────
    {=}[x@n ≔ e] = {=}


    t₀[x@n ≔ e] = t₁   { xs₀… }[x@n ≔ e] = { xs₁… }
    ───────────────────────────────────────────────
    { x₀ = t₀, xs₀… }[x@n ≔ e] = { x₀ = t₁, xs₁… }


    ────────────────
    <>[x@n ≔ e] = <>


    T₀[x@n ≔ e] = T₁   < xs₀… >[x@n ≔ e] = < xs₁… >
    ────────────────────────────────────────────────
    < x₀ : T₀ | xs₀… >[x@n ≔ e] = < x₀ : T₁ | xs₁… >


    t₀[x@n ≔ e] = t₁
    ──────────────────────────────────
    < x₀ = t₀ >[x@n ≔ e] = < x₀ = t₁ >


    T₁₀[x@n ≔ e] = T₁₁   < x₀ = t₀₀ | xs₀… >[x@n ≔ e] = < x₀ = t₀₁ | xs₁… >
    ────────────────────────────────────────────────────────────────────────
    < x₀ = t₀₀ | x₁ : T₁₀ | xs₀… >[x@n ≔ e] = < x₀ = t₀₁ | x₁ : T₁₁ | xs₁… >


    u₀[x@n ≔ e] = u₁
    ──────────────────────────────────
    (constructors u₀)[x@n ≔ e] = constructors u₁


    ──────────────────────────────────────
    Natural/build[x@n ≔ e] = Natural/build


    ────────────────────────────────────
    Natural/fold[x@n ≔ e] = Natural/fold


    ────────────────────────────────────────
    Natural/isZero[x@n ≔ e] = Natural/isZero


    ────────────────────────────────────
    Natural/even[x@n ≔ e] = Natural/even


    ──────────────────────────────────
    Natural/odd[x@n ≔ e] = Natural/odd


    ──────────────────────────────────────────────
    Natural/toInteger[x@n ≔ e] = Natural/toInteger


    ────────────────────────────────────
    Natural/show[x@n ≔ e] = Natural/show


    ────────────────────────────────────────────
    Integer/toDouble[x@n ≔ e] = Integer/toDouble


    ────────────────────────────────────
    Integer/show[x@n ≔ e] = Integer/show


    ──────────────────────────────────
    Double/show[x@n ≔ e] = Double/show


    ────────────────────────────────
    List/build[x@n ≔ e] = List/build


    ──────────────────────────────
    List/fold[x@n ≔ e] = List/fold


    ──────────────────────────────────
    List/length[x@n ≔ e] = List/length


    ──────────────────────────────
    List/head[x@n ≔ e] = List/head


    ──────────────────────────────
    List/last[x@n ≔ e] = List/last


    ────────────────────────────────────
    List/indexed[x@n ≔ e] = List/indexed


    ────────────────────────────────────
    List/reverse[x@n ≔ e] = List/reverse


    ──────────────────────────────────────
    Optional/fold[x@n ≔ e] = Optional/fold


    ────────────────────────────────────────
    Optional/build[x@n ≔ e] = Optional/build


    ────────────────────
    Bool[x@n ≔ e] = Bool


    ────────────────────────────
    Optional[x@n ≔ e] = Optional


    ──────────────────────────
    Natural[x@n ≔ e] = Natural


    ──────────────────────────
    Integer[x@n ≔ e] = Integer


    ────────────────────────
    Double[x@n ≔ e] = Double


    ────────────────────
    Text[x@n ≔ e] = Text


    ────────────────────
    List[x@n ≔ e] = List


    ────────────────────
    True[x@n ≔ e] = True


    ──────────────────────
    False[x@n ≔ e] = False


    ────────────────────
    Type[x@n ≔ e] = Type


    ────────────────────
    Kind[x@n ≔ e] = Kind


## α-normalization

α-normalization is a function of the following form:

    t₀ ↦ t₁

... where:

* `t₀` (the input) is the expression to α-normalize
* `t₁` (the output) is the α-normalized expression

α-normalization renames all bound variables within an expression to use De
Bruijn indices.  For example, the following expression:

    λ(a : Type) → λ(b : Type) → λ(x : a) → λ(y : b) → x

... α-normalizes to:

    λ(_ : Type) → λ(_ : Type) → λ(_ : _@1) → λ(_ : _@1) → _@1

In other words, all bound variables are renamed to `_` and they used the
variable index to disambiguate which variable they are referring to.  This is
equivalent to De Bruijn indices:

    λ(a : Type) → λ(b : Type) → a ↦ λ(_ : Type) → λ(_ : Type) → _@1

    λ(x : Type) → _ ↦ λ(_ : Type) → _@1

If two expressions are α-equivalent then they will be identical after
α-normalization.  For example:

    λ(a : Type) → a ↦ λ(_ : Type) → _

    λ(b : Type) → b ↦ λ(_ : Type) → _

Note that free variables are not transformed by α-normalization.  For
example:

    λ(x : Type) → y ↦ λ(_ : Type) → y

### Bound variables

The only interesting part of α-normalization is expressions with bound
variables.  Each of the following normalization rules renames the bound variable
to `_`, substituting and shifting as necessary in order to avoid variable
capture:


    A₀ ↦ A₁
    b₀ ↦ b₁
    ───────────────────────────────
    λ(_ : A₀) → b₀ ↦ λ(_ : A₁) → b₁


    A₀ ↦ A₁
    ↑(1, _, 0, b₀) = b₁
    b₁[x ≔ _] = b₂
    ↑(-1, x, 0, b₂) = b₃
    b₃ ↦ b₄
    ───────────────────────────────  x ≠ _
    λ(x : A₀) → b₀ ↦ λ(_ : A₁) → b₄


    A₀ ↦ A₁
    B₀ ↦ B₁
    ───────────────────────────────
    ∀(_ : A₀) → B₀ ↦ ∀(_ : A₁) → B₁


    A₀ ↦ A₁
    ↑(1, _, 0, B₀) = B₁
    B₁[x ≔ _] = B₂
    ↑(-1, x, 0, B₂) = B₃
    B₃ ↦ B₄
    ───────────────────────────────  x ≠ _
    ∀(x : A₀) → B₀ ↦ ∀(_ : A₁) → B₄


    a₀ ↦ a₁
    A₀ ↦ A₁
    b₀ ↦ b₁
    ─────────────────────────────────────────────
    let _ = a₀ : A₀ in b₀ ↦ let _ = a₁ : A₁ in b₁


    a₀ ↦ a₁
    A₀ ↦ A₁
    ↑(1, _, 0, b₀) = b₁
    b₁[x ≔ _] = b₂
    ↑(-1, x, 0, b₂) = b₃
    b₃ ↦ b₄
    ─────────────────────────────────────────────  x ≠ _
    let x = a₀ : A₀ in b₀ ↦ let _ = a₁ : A₁ in b₄


    a₀ ↦ a₁
    b₀ ↦ b₁
    ───────────────────────────────────
    let _ = a₀ in b₀ ↦ let _ = a₁ in b₁


    a₀ ↦ a₁
    ↑(1, _, 0, b₀) = b₁
    b₁[x ≔ _] = b₂
    ↑(-1, x, 0, b₂) = b₃
    b₃ ↦ b₄
    ───────────────────────────────────  x ≠ _
    let x = a₀ in b₀ ↦ let _ = a₁ in b₄


### Variables

Variables are already in α-normal form:


    ─────────
    x@n ↦ x@n


If they are free variables then there is nothing to do because α-normalization
does not affect free variables.  If they were originally bound variables there
is still nothing to do because would have been renamed to `_` along the way by
one of the preceding rules.

### Imports

An expression with unresolved imports cannot be α-normalized.

### Other

No other Dhall expressions bind variables, so α-normalization just descends into
sub-expressions for the remaining rules:


    t₀ ↦ t₁   l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────────────────────────────────
    if t₀ then l₀ else r₀ ↦ if t₁ then l₁ else r₁



    t₀ ↦ t₁   u₀ ↦ u₁   T₀ ↦ T₁
    ───────────────────────────────────
    merge t₀ u₀ : T₀ ↦ merge t₁ u₁ : T₁


    t₀ ↦ t₁   u₀ ↦ u₁
    ─────────────────────────
    merge t₀ u₀ ↦ merge t₁ u₁


    T₀ ↦ T₁
    ───────────────────────────
    [] : List T₀ ↦ [] : List T₁


    t₀ ↦ t₁   [ ts₀… ] ↦ [ ts₁… ]
    ─────────────────────────────
    [ t₀, ts₀… ] ↦ [ t₁, ts₁… ]


    T₀ ↦ T₁
    ───────────────────────────────────
    [] : Optional T₀ ↦ [] : Optional T₁


    t₀ ↦ t₁   T₀ ↦ T₁
    ───────────────────────────────────────────
    [ t₀ ] : Optional T₀ ↦ [ t₁ ] : Optional T₁


    t₀ ↦ t₁   T₀ ↦ T₁
    ─────────────────
    t₀ : T₀ ↦ t₁ : T₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ || r₀ = l₁ || r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ + r₀ = l₁ + r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ ++ r₀ = l₁ ++ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ # r₀ = l₁ # r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ && r₀ = l₁ && r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ ∧ r₀ = l₁ ∧ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ ⫽ r₀ = l₁ ⫽ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ ⩓ r₀ = l₁ ⩓ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ * r₀ = l₁ * r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ == r₀ = l₁ == r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ != r₀ = l₁ != r₁


    f₀ ↦ f₁   a₀ ↦ a₁
    ─────────────────
    f₀ a₀ = f₁ a₁


    t₀ ↦ t₁
    ───────────
    t₀.x = t₁.x


    t₀ ↦ t₁
    ───────────────────────
    t₀.{ xs… } ↦ t₁.{ xs… }


    ─────────
    n.n ↦ n.n


    ─────
    n ↦ n


    ───────
    ±n ↦ ±n


    ─────────
    "…" ↦ "…"


    ───────
    {} ↦ {}


    T₀ ↦ T₁   { xs₀… } ↦ { xs₁… }
    ───────────────────────────────────
    { x : T₀, xs₀… } ↦ { x : T₁, xs₁… }


    ─────────
    {=} ↦ {=}


    t₀ ↦ t₁   { xs₀… } ↦ { xs₁… }
    ───────────────────────────────────
    { x = t₀, xs₀… } ↦ { x = t₁, xs₁… }


    ───────
    <> ↦ <>


    T₀ ↦ T₁   < xs₀… > ↦ < xs₁… >
    ─────────────────────────────────────
    < x : T₀ | xs₀… > ↦ < x : T₁ | xs₁… >


    t₀ ↦ t₁
    ───────────────────────
    < x = t₀ > ↦ < x = t₁ >


    t₀ ↦ t₁   < xs₀… > ↦ < xs₁… >
    ─────────────────────────────────────
    < x = t₀ | xs₀… > ↦ < x = t₁ | xs₁… >


    u₀ ↦ u₁
    ─────────────────────────────────
    constructors u₀ ↦ constructors u₁


    ─────────────────────────────
    Natural/build ↦ Natural/build


    ───────────────────────────
    Natural/fold ↦ Natural/fold


    ───────────────────────────────
    Natural/isZero ↦ Natural/isZero


    ───────────────────────────
    Natural/even ↦ Natural/even


    ─────────────────────────
    Natural/odd ↦ Natural/odd


    ─────────────────────────────────────
    Natural/toInteger ↦ Natural/toInteger


    ───────────────────────────
    Natural/show ↦ Natural/show


    ───────────────────────────────────
    Integer/toDouble ↦ Integer/toDouble


    ───────────────────────────
    Integer/show ↦ Integer/show


    ─────────────────────────
    Double/show ↦ Double/show


    ───────────────────────
    List/build ↦ List/build


    ─────────────────────
    List/fold ↦ List/fold


    ─────────────────────────
    List/length ↦ List/length


    ─────────────────────
    List/head ↦ List/head


    ─────────────────────
    List/last ↦ List/last


    ───────────────────────────
    List/indexed ↦ List/indexed


    ───────────────────────────
    List/reverse ↦ List/reverse


    ─────────────────────────────
    Optional/fold ↦ Optional/fold


    ───────────────────────────────
    Optional/build ↦ Optional/build


    ───────────
    Bool ↦ Bool


    ───────────────────
    Optional ↦ Optional


    ─────────────────
    Natural ↦ Natural


    ─────────────────
    Integer ↦ Integer


    ───────────────
    Double ↦ Double


    ───────────
    Text ↦ Text


    ───────────
    List ↦ List


    ───────────
    True ↦ True


    ─────────────
    False ↦ False


    ───────────
    Type ↦ Type


    ───────────
    Kind ↦ Kind


## β-normalization

β-normalization is a function of the following form:

    t₀ ⇥ t₁

... where:

* `t₀` (the input) is the expression to normalize
* `t₁` (the output) is the normalized expression

β-normalization evaluates all β-reducible expressions:

    (λ(x : Bool) → x == False) True ⇥ False

β-normalization evaluates all built-in functions if they are fully saturated
(i.e.  no missing arguments):

    List/length Integer [1, 2, 3] ⇥ +3

β-normalization does not simplify partially applied built-in functions:

    List/length Integer ⇥ List/length Integer

β-normalization works under λ, meaning that the body of an unapplied
λ-expression can be normalized:

    λ(x : Integer) → List/length Integer [x, x, x] ⇥ λ(x : Integer) → +3

Dhall is a total language that is strongly normalizing, so evaluation order has
no effect on the language semantics and a conforming implementation can select
any evaluation strategy.

Also, note that the semantics specifies several built-in types, functions and
operators that conforming implementations must support.  Implementations are
encouraged to implement the following functions and operators in more efficient
ways than the following reduction rules so long as the result of normalization
is the same.

### Constants

Type-checking constants are in normal form:


    ───────────
    Type ⇥ Type


    ───────────
    Kind ⇥ Kind


### Variables

Variables are in normal form:


    ─────────
    x@n ⇥ x@n


### `Bool`


The `Bool` type is in normal form:


    ───────────
    Bool ⇥ Bool


The `Bool` constructors are in normal form:


    ───────────
    True ⇥ True


    ─────────────
    False ⇥ False


Simplify an `if` expression if the predicate normalizes to a `Bool` literal:


    t ⇥ True   l₀ ⇥ l₁
    ────────────────────────
    if t then l₀ else r ⇥ l₁


    t ⇥ False   r₀ ⇥ r₁
    ────────────────────────
    if t then l else r₀ ⇥ r₁


Also, simplify an `if` expression if the expression trivially returns the
predicate:


    l ⇥ True   r ⇥ False   t₀ ⇥ t₁
    ──────────────────────────────
    if t₀ then l else r ⇥ t₁


Simplify `if` expressions where both alternatives are the same:


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    if t then l₀ else r₀ ⇥ l₁


Otherwise, normalize the predicate and both branches of the `if` expression:


    t₀ ⇥ t₁   l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────────────────────────────────  ; If no other rule matches
    if t₀ then l₀ else r₀ ⇥ if t₁ then l₁ else r₁


Even though `True`, `False`, and `if` expressions suffice for all `Bool` logic,
Dhall also supports logical operators for convenience.

Simplify the logical "or" operator so long as at least one argument normalizes
to a `Bool` literal:


    l ⇥ False   r₀ ⇥ r₁
    ───────────────────
    l || r₀ ⇥ r₁


    r ⇥ False   l₀ ⇥ l₁
    ───────────────────
    l₀ || r ⇥ l₁


    l ⇥ True
    ─────────────
    l || r ⇥ True


    r ⇥ True
    ─────────────
    l || r ⇥ True


Normalize arguments that are equivalent


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ || r₀ ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ || r₀ ⇥ l₁ || r₁


Simplify the logical "and" operator so long as at least one argument normalizes
to a `Bool` literal:


    l ⇥ True   r₀ ⇥ r₁
    ──────────────────
    l && r₀ ⇥ r₁


    r ⇥ True   l₀ ⇥ l₁
    ──────────────────
    l₀ && r ⇥ l₁


    l ⇥ False
    ──────────────
    l && r ⇥ False


    r ⇥ False
    ──────────────
    l && r ⇥ False


Normalize arguments that are equivalent


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ && r₀ ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ && r₀ ⇥ l₁ && r₁


Simplify the logical "equal" operator if one argument normalizes to a `True`
literal:


    l ⇥ True   r₀ ⇥ r₁
    ──────────────────
    l == r₀ ⇥ r₁


    r ⇥ True   l₀ ⇥ l₁
    ──────────────────
    l₀ == r ⇥ l₁


... or if both arguments are equivalent:


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ == r₀ ⇥ True


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ == r₀ ⇥ l₁ == r₁


Simplify the logical "not equal" operator if one argument normalizes to a
`False` literal:


    l ⇥ False   r₀ ⇥ r₁
    ───────────────────
    l != r₀ ⇥ r₁


    r ⇥ False   l₀ ⇥ l₁
    ───────────────────
    l₀ != r ⇥ l₁


... or if both arguments are equivalent:


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ != r₀ ⇥ False


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ != r₀ ⇥ l₁ != r₁


### `Natural`

The `Natural` number type is in normal form:


    ─────────────────
    Natural ⇥ Natural


`Natural` number literals are in normal form:


    ─────
    n ⇥ n


`Natural/build` and `Natural/fold` are inverses of one another, which leads to
the following fusion rule:


    f ⇥ Natural/build   a ⇥ Natural/fold b
    ──────────────────────────────────────
    f a ⇥ b


Otherwise, fall back on each function's respective implementation.

`Natural/build` is the canonical introduction function for `Natural` numbers:


    f ⇥ Natural/build   g Natural (λ(x : Natural) → x + 1) 0 ⇥ b
    ────────────────────────────────────────────────────────────
    f g ⇥ b


`Natural/fold` function is the canonical elimination function for `Natural`
numbers:


    f ⇥ Natural/fold 0 B g   b ⇥ t₁
    ───────────────────────────────
    f b ⇥ t₁


    f ⇥ Natural/fold (1 + n) B g
    g (Natural/fold n B g b) ⇥ t₁
    ─────────────────────────────  ; "1 + n" means "a `Natural` literal greater
    f b ⇥ t₁                       ; than `0`"


Even though `Natural/fold` and `Natural/build` suffice for all `Natural` number
programming, Dhall also supports `Natural` number literals and built-in
functions and operators on `Natural` numbers, both for convenience and
efficiency.

Use machine addition to simplify the "plus" operator if both arguments normalize
to `Natural` literals:


    l ⇥ m   r ⇥ n
    ─────────────  ; "m + n" means "use machine addition"
    l + r ⇥ m + n


Also, simplify the "plus" operator if either argument normalizes to a `0`
literal:


    l ⇥ 0   r₀ ⇥ r₁
    ───────────────
    l + r₀ ⇥ r₁


    r ⇥ 0   l₀ ⇥ l₁
    ───────────────
    l₀ + r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ + r₀ ⇥ l₁ + r₁


Use machine multiplication to simplify the "times" operator if both arguments
normalize to a `Natural` literal:


    l ⇥ m   r ⇥ n
    ─────────────  ; "m * n" means "use machine multiplication"
    l + r ⇥ m * n


Also, simplify the "plus" operator if either argument normalizes to either a
`0` literal:


    l ⇥ 0
    ─────────
    l * r ⇥ 0


    r ⇥ 0
    ─────────
    l * r ⇥ 0



... or a `1` literal:


    l ⇥ 1   r₀ ⇥ r₁
    ───────────────
    l * r₀ ⇥ r₁


    r ⇥ 1   l₀ ⇥ l₁
    ───────────────
    l₀ * r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ * r₀ ⇥ l₁ * r₁


`Natural/isZero` detects whether or not a `Natural` number is `0`:


    f ⇥ Natural/isZero   a ⇥ 0
    ───────────────────────────
    f a ⇥ True


    f ⇥ Natural/isZero   a ⇥ 1 + n
    ──────────────────────────────  ; "1 + n" means "a `Natural` literal greater
    f a ⇥ False                     ; than `0`"


`Natural/even` detects whether or not a `Natural` number is even:


    f ⇥ Natural/even   a ⇥ 0
    ────────────────────────
    f a ⇥ True


    f ⇥ Natural/even   a ⇥ 1
    ────────────────────────
    f a ⇥ False


    f ⇥ Natural/even
    a ⇥ 1 + n
    Natural/odd n ⇥ b
    ─────────────────  ; "1 + n" means "a `Natural` literal greater than `0`"
    f a ⇥ b


`Natural/odd` detects whether or not a `Natural` number is odd:


    f ⇥ Natural/odd   a ⇥ 0
    ───────────────────────
    f a ⇥ False


    f ⇥ Natural/odd   a ⇥ 1
    ───────────────────────
    f a ⇥ True


    f ⇥ Natural/odd
    a ⇥ 1 + n
    Natural/even n ⇥ b
    ──────────────────  ; "1 + n" means "a `Natural` literal greater than `0`"
    f a ⇥ b


`Natural/toInteger` transforms a `Natural` number into the corresponding
`Integer`:


    f ⇥ Natural/toInteger   a ⇥ n
    ─────────────────────────────
    f a ⇥ +n


`Natural/show` transforms a `Natural` number into a `Text` literal representing
valid Dhall code for representing that `Natural` number:


    f ⇥ Natural/show   a ⇥ n
    ────────────────────────
    f a ⇥ "n"


All of the built-in functions on `Natural` numbers are in normal form:


    ─────────────────────────────
    Natural/build ⇥ Natural/build


    ───────────────────────────
    Natural/fold ⇥ Natural/fold


    ───────────────────────────────
    Natural/isZero ⇥ Natural/isZero


    ───────────────────────────
    Natural/even ⇥ Natural/even


    ─────────────────────────
    Natural/odd ⇥ Natural/odd


    ─────────────────────────────────────
    Natural/toInteger ⇥ Natural/toInteger


    ───────────────────────────
    Natural/show ⇥ Natural/show


### `Text`

The `Text` type is in normal form:


    ───────────
    Text ⇥ Text


`Text` literals normalize to their
[canonical encoding](#canonicalization-of-text).

Use machine concatenation to simplify the "text concatenation" operator if both
arguments normalize to `Text` literals:


    l ⇥ "…"₀   r ⇥ "…"₁
    ─────────────────────  ; "…"₀ ++ "…"₁ means "use machine concatenation"
    l ++ r ⇥ "…"₀ ++ "…"₁


Also, simplify the "text concatenation" operator if either argument normalizes
to a `""` literal:


    l ⇥ ""   r₀ ⇥ r₁
    ────────────────
    l ++ r₀ ⇥ r₁


    r ⇥ ""   l₀ ⇥ l₁
    ────────────────
    l₀ ++ r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ ++ r₀ ⇥ l₁ ++ r₁


### `List`

The `List` type-level function is in normal form:


    ───────────
    List ⇥ List


Normalizing a `List` normalizes each field and the type annotation:


    T₀ ⇥ T₁
    ──────────────────────────
    [] : List T₀ ⇥ [] : List T₁


    t₀ ⇥ t₁   [ ts₀… ] ⇥ [ ts₁… ]
    ─────────────────────────────
    [ t₀, ts₀… ] ⇥ [ t₁, ts₁… ]


Lists are defined here via induction as if they were linked lists, but a real
implementation might represent them using another data structure under the hood.
Dhall does not impose time complexity requirements on list operations.

`List/build` and `List/fold` are inverses of one another, which leads to the
following fusion rule:


    f ⇥ List/build A₀   a ⇥ List/fold A₁ b
    ──────────────────────────────────────
    f a ⇥ b


Otherwise, fall back on each function's respective implementation.

`List/build` is the canonical introduction function for `List`s:


    f ⇥ List/build A₀
    ↑(1, a, 0, A₀) = A₁
    g (List A₀) (λ(a : A₀) → λ(as : List A₁) → [ a ] # as) ([] : List A₀) ⇥ b
    ─────────────────────────────────────────────────────────────────────────
    f g ⇥ b


`List/fold` is the canonical elimination function for `List`s:


    f ⇥ List/fold A₀ ([] : List A₁) B g   b₀ ⇥ b₁
    ─────────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ List/fold A₀ [ a, as… ] B g   g a (List/fold A₀ [ as… ] B g b₀) ⇥ b₁
    ────────────────────────────────────────────────────────────────────────
    f b₀ ⇥ b₁


Even though `List/build` and `List/fold` suffice for all `List` operations,
Dhall also supports built-in functions and operators on `List`s, both for
convenience and efficiency.

Use machine concatenation to simplify the "list concatenation" operator if both
arguments normalize to `List` literals:


    ls₀ ⇥ [ ls₁… ]
    rs₀ ⇥ [ rs₁… ]
    [ ls₁… ] # [ rs₁… ] ⇥ t
    ───────────────────────   ;  "[ ls₁… ] # [ rs₁… ]" means "use machine
    ls₀ # rs₀ ⇥ t             ;  concatenation"


Also, simplify the "list concatenation" operator if either argument normalizes
to an empty `List`:


    ls ⇥ [] : List T   rs₀ ⇥ rs₁
    ────────────────────────────
    ls # rs₀ ⇥ rs₁


    rs ⇥ [] : List T   ls₀ ⇥ ls₁
    ────────────────────────────
    ls₀ # rs ⇥ ls₁


Otherwise, normalize each argument:


    ls₀ ⇥ ls₁   rs₀ ⇥ rs₁
    ─────────────────────   ; If no other rule matches
    ls₀ # rs₀ ⇥ ls₁ # rs₁


`List/length` returns the length of a list:


    f ⇥ List/length A₀   a ⇥ [] : List A₁
    ─────────────────────────────────────
    f a ⇥ 0


    f ⇥ List/length A₀   as₀ ⇥ [ a, as₁… ]   1 + List/length A₀ [ as₁… ] ⇥ n
    ────────────────────────────────────────────────────────────────────────
    f as₀ ⇥ n


`List/head` returns the first element of a list:


    f ⇥ List/head A₀   as ⇥ [] : List A₁
    ────────────────────────────────────
    f as ⇥ [] : Optional A₀


    f ⇥ List/head A₀   as ⇥ [ a, … ]
    ────────────────────────────────
    f as ⇥ [ a ] : Optional A₀


`List/last` returns the last element of a list:


    f ⇥ List/last A₀   as ⇥ [] : List A₁
    ────────────────────────────────────
    f as ⇥ [] : Optional A₀


    f ⇥ List/last A₀   as ⇥ [ …, a ]
    ────────────────────────────────
    f as ⇥ [ a ] : Optional A₀


`List/indexed` tags each element of the list with the element's index:


    f ⇥ List/indexed A₀   as ⇥ [] : List A₁
    ───────────────────────────────────────────────
    f as ⇥ [] : List { index : Natural, value : A₀ }


    f ⇥ List/indexed A₀   as ⇥ [ a₀, a₁, …, ]
    ──────────────────────────────────────────────────────────────────
    f as ⇥ [ { index = 0, value = a₀ }, { index = 1, value = a₁ }, … ]


`List/reverse` reverses the elements of the list:


    f ⇥ List/reverse A₀   as ⇥ [] : List A₁
    ───────────────────────────────────────
    f as ⇥ [] : List A₁


    f ⇥ List/reverse A₀   as ⇥ [ a₀, a₁, … ]
    ────────────────────────────────────────
    f as ⇥ [ …, a₁, a₀ ]


All of the built-in functions on `List`s are in normal form:


    ───────────────────────
    List/build ⇥ List/build


    ─────────────────────
    List/fold ⇥ List/fold


    ─────────────────────────
    List/length ⇥ List/length


    ─────────────────────
    List/head ⇥ List/head


    ─────────────────────
    List/last ⇥ List/last


    ───────────────────────────
    List/indexed ⇥ List/indexed


    ───────────────────────────
    List/reverse ⇥ List/reverse


### `Optional`

The `Optional` type-level function is in normal form:


    ───────────────────
    Optional ⇥ Optional


Normalizing an `Optional` literal normalizes the type annotation and the value,
if present:


    T₀ ⇥ T₁
    ───────────────────────────────────
    [] : Optional T₀ ⇥ [] : Optional T₁


    t₀ ⇥ t₁   T₀ ⇥ T₁
    ───────────────────────────────────────────
    [ t₀ ] : Optional T₀ ⇥ [ t₁ ] : Optional T₁


`Optional/build` and `Optional/fold` are inverses of one another, which leads to
the following fusion rule:


    f ⇥ Optional/build A₀   a ⇥ Optional/fold A₁ b
    ──────────────────────────────────────────────
    f a ⇥ b


`Optional/build` is the canonical introduction function for `Optional` values:


    f ⇥ Optional/build A₀
    ↑(1, a, 0, A₀) = A₁
    g (Optional A₀) (λ(a : A₀) → [ a ] : Optional A₁) ([] : Optional A₀) ⇥ b
    ────────────────────────────────────────────────────────────────────────
    f g ⇥ b


`Optional/fold` is the canonical elimination function for `Optional` values:


    f ⇥ Optional/fold A₀ ([ a ] : Optional A₁) B₀ g   g a ⇥ b₁
    ──────────────────────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ Optional/fold A₀ ([] : Optional A₁) B₀ g   b₀ ⇥ b₁
    ─────────────────────────────────────────────────────
    f b₀ ⇥ b₁


All of the built-in functions on `Optional` values are in normal form:


    ─────────────────────────────
    Optional/fold ⇥ Optional/fold


    ───────────────────────────────
    Optional/build ⇥ Optional/build


### Records

Normalizing a record type normalizes the type of each field:


    ───────
    {} ⇥ {}


    T₀ ⇥ T₁   { xs₀… } ⇥ { xs₁… }
    ───────────────────────────────────
    { x : T₀, xs₀… } ⇥ { x : T₁, xs₁… }


Normalizing a record value normalizes each field:


    ─────────
    {=} ⇥ {=}


    t₀ ⇥ t₁   { xs₀… } ⇥ { xs₁… }
    ───────────────────────────────────
    { x = t₀, xs₀… } ⇥ { x = t₁, xs₁… }


Simplify a record selection if the argument is a record literal:


    t ⇥ { x = v, … }
    ────────────────
    t.x ⇥  v


You can also project out more than one field into a new record:


    ──────────
    t.{} ⇥  {}


    t ⇥ { x = v, ts… }   { ts… }.{ xs… } ⇥  { ys… }
    ───────────────────────────────────────────────
    t.{ x, xs… } ⇥  { x = v, ys… }


The type system ensures that the selected field(s) must be present.

Otherwise, normalize the argument:


    t₀ ⇥ t₁
    ───────────  ; If no other rule matches
    t₀.x ⇥ t₁.x


Recursive record merge combines two records, recursively merging any fields that
collide.  The type system ensures that colliding fields must be records:


    l ⇥ {=}   r₀ ⇥ r₁
    ─────────────────
    l ∧ r₀ ⇥ r₁


    r ⇥ {=}   l₀ ⇥ l₁
    ─────────────────
    l₀ ∧ r ⇥ l₁


    ls₀ ⇥ { x = l₁, ls₁… }
    rs₀ ⇥ { x = r₁, rs₁… }
    l₁ ∧ r₁ ⇥ t
    { ls₁… } ∧ { rs₁… } ⇥ { ts… }
    ─────────────────────────────
    ls₀ ∧ rs₀ ⇥ { x = t, ts… }


    ls₀ ⇥ { x = l₁, ls₁… }   { ls₁… } ∧ rs ⇥ { ls₂… }
    ─────────────────────────────────────────────────  ; x ∉ rs
    ls₀ ∧ rs ⇥ { x = l₁, ls₂… }


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ∧ r₀ ⇥ l₁ ∧ r₁


Right-biased record merge is non-recursive.  Field collisions are resolved by
preferring the field from the right record and discarding the colliding field
from the left record:


    l ⇥ e
    ───────────
    l ⫽ {=} ⇥ e


    r ⇥ e
    ───────────
    {=} ⫽ r ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }
    rs₀ ⇥ { x = r₁, rs₁… }
    { ls₁… } ⫽ { rs₁… } ⇥ { ts… }
    ─────────────────────────────
    ls₀ ⫽ rs₀ ⇥ { x = r₁, ts… }


    ls₀ ⇥ { x = l₁, ls₁… }   { ls₁… } ⫽ rs ⇥ { ls₂… }
    ─────────────────────────────────────────────────  ; x ∉ rs
    ls₀ ⫽ rs ⇥ { x = l₁, ls₂… }


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ⫽ r₀ ⇥ l₁ ⫽ r₁


Recursive record type merge combines two record types, recursively merging any
fields that collide.  The type system ensures that colliding fields must be
record types:


    l ⇥ {}   r₀ ⇥ r₁
    ────────────────
    l ⩓ r₀ ⇥ r₁


    r ⇥ {}   l₀ ⇥ l₁
    ────────────────
    l₀ ⩓ r ⇥ l₁


    ls₀ ⇥ { x : l₁, ls₁… }
    rs₀ ⇥ { x : r₁, rs₁… }
    l₁ ⩓ r₁ ⇥ t
    { ls₁… } ⩓ { rs₁… } ⇥ { ts… }
    ─────────────────────────────
    ls₀ ⩓ rs₀ ⇥ { x : t, ts… }


    ls₀ ⇥ { x : l₁, ls₁… }   { ls₁… } ⩓ rs ⇥ { ls₂… }
    ─────────────────────────────────────────────────  ; x ∉ rs
    ls₀ ⩓ rs ⇥ { x : l₁, ls₂… }


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ⩓ r₀ ⇥ l₁ ⩓ r₁


### Unions

Normalizing a union type normalizes the type of each alternative:


    ───────
    <> ⇥ <>


    T₀ ⇥ T₁   < xs₀… > ⇥ < xs₁… >
    ─────────────────────────────────────
    < x : T₀ | xs₀… > ⇥ < x : T₁ | xs₁… >


Normalizing a union value is the same as normalizing the specified value and
the type of each alternative:


    t₀ ⇥ t₁
    ───────────────────────
    < x = t₀ > ⇥ < x = t₁ >


    T₁₀ ⇥ T₁₁   < x₀ = t₀₀ | xs₀… > ⇥ < x₁ = t₀₁ | xs₁… >
    ────────────────────────────────────────────────────────────────
    < x₀ = t₀₀ | x₁ : T₁₀ | xs₀… > ⇥ < x₀ = t₀₁ | x₁ : T₁₁ | xs₁… >


`merge` expressions are the canonical way to eliminate a union literal.  The
first argument to `merge` is a record of handlers and the second argument is a
union value.  Apply the handler of the same label to the selected value of the
union literal:


    t ⇥ { x = f, … }   u ⇥ < x = a | … >   f a ⇥ b
    ──────────────────────────────────────────────
    merge t u : T ⇥ b


    t₀ ⇥ t₁   u₀ ⇥ u₁   T₀ ⇥ T₁
    ───────────────────────────────────  ; If no other rule matches
    merge t₀ u₀ : T₀ ⇥ merge t₁ u₁ : T₁


    t ⇥ { x = f, … }   u ⇥ < x = a | … >   f a ⇥ b
    ──────────────────────────────────────────────
    merge t u ⇥ b


    t₀ ⇥ t₁   u₀ ⇥ u₁
    ─────────────────────────  ; If no other rule matches
    merge t₀ u₀ ⇥ merge t₁ u₁


`constructors` converts a union type literal to a record of constructors for
each alternative:


    u ⇥ <>
    ────────────────────
    constructors u ⇥ {=}


    u ⇥ < x₀ : T₀ | x₁ : T₁ | xs… >
    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    constructors u ⇥ { x₀ = λ(x₀ : T₀) → < x₀ = x₀ | x₁ : T₁ | xs… >, x₁ = λ(x₁ : T₁) → < x₀ : T₀ | x₁ = x₁ | xs… >, … }


    u₀ ⇥ u₁
    ─────────────────────────────────  ; If no other rule matches
    constructors u₀ ⇥ constructors u₁


### `Integer`

The `Integer` type is in normal form:


    ─────────────────
    Integer ⇥ Integer


An `Integer` literal is in normal form:


    ───────
    ±n ⇥ ±n


`Integer/toDouble` transforms an `Integer` into the corresponding `Double`:


    f ⇥ Natural/toDouble   a ⇥ ±n
    ─────────────────────────────
    f a ⇥ ±n.0


`Integer/show` transforms an `Integer` into a `Text` literal representing valid
Dhall code for representing that `Integer` number:


    f ⇥ Integer/show   a ⇥ ±n
    ─────────────────────────
    f a ⇥ "±n"


Note that the `Text` representation of the rendered `Integer` should include
a leading `+` sign if the number is non-negative and a leading `-` sign if
the number is negative.

All of the built-in functions on `Integer`s are in normal form:


    ───────────────────────────
    Integer/show ⇥ Integer/show


    ───────────────────────────────────
    Integer/toDouble ⇥ Integer/toDouble



### `Double`

The `Double` type is in normal form:


    ───────────────
    Double ⇥ Double


A `Double` literal is in normal form:


    ─────────
    n.n ⇥ n.n


`Double/show` transforms a `Double` into a `Text` literal representing valid
Dhall code for representing that `Double` number:


    f ⇥ Double/show   a ⇥ n.n
    ─────────────────────────
    f a ⇥ "n.n"


The `Double/show` function is in normal form:


    ─────────────────────────
    Double/show ⇥ Double/show


### Functions

Normalizing a function type normalizes the types of the input and output:


    A₀ → A₁   B₀ → B₁
    ───────────────────────────────
    ∀(x : A₀) → B₀ ⇥ ∀(x : A₁) → B₁


You can introduce an anonymous function using a λ:


    A₀ → A₁   b₀ → b₁
    ───────────────────────────────
    λ(x : A₀) → b₀ ⇥ λ(x : A₁) → b₁


You can eliminate an anonymous function through β-reduction:


    f ⇥ λ(x : A) → b₀
    ↑(1, x, 0, a₀) = a₁
    b₀[x ≔ a₁] = b₁
    ↑(-1, x, 0, b₁) = b₂
    b₂ ⇥ b₃
    ──────────────────────
    f a₀ ⇥ b₃


Function application falls back on normalizing both sub-expressions if none of
the preceding function application rules apply:


    f₀ ⇥ f₁   a₀ ⇥ a₁
    ─────────────────  ; If no other rule matches
    f₀ a₀ ⇥ f₁ a₁


### `let` expressions

For the purposes of normalization, an expression of the form:

    let x : A = a₀ in b₀

... is semantically identical to:

    (λ(x : A) → b₀) a₀

... and the normalization rules for `let` expressions reflect that semantic
equivalence:


    ↑(1, x, 0, a₀) = a₁
    b₀[x ≔ a₁] = b₁
    ↑(-1, x, 0, b₁) = b₂
    b₂ ⇥ b₃
    ─────────────────────────
    let x : A = a₀ in b₀ ⇥ b₃


    ↑(1, x, 0, a₀) = a₁
    b₀[x ≔ a₁] = b₁
    ↑(-1, x, 0, b₁) = b₂
    b₂ ⇥ b₃
    ─────────────────────
    let x = a₀ in b₀ ⇥ b₃


### Type annotations

Simplify a type annotation by removing the annotation:


    t₀ ⇥ t₁
    ───────────
    t₀ : T ⇥ t₁


### Imports

An expression with unresolved imports cannot be β-normalized.

## Text literals

### Interpolation

Dhall supports text `"${expr}"`-style text interpolation. Text interpolation is
a primitive language feature that survives normalization.

### Canonicalization of text

Multiple text literals can encode the same sequence of code points, but every
sequence of code points can be encoded as one unique canonical text literal. The
canonical encoding of a code point is that code point itself, apart from the
following code points:

 * U+0008 (backspace) encodes to U+005C, U+0062 (`\b`).
 * U+0009 (tab) encodes to U+005C, U+0074 (`\t`).
 * U+000C (form feed) encodes to U+005C, U+0066 (`\f`).
 * U+000A (line feed) encodes to U+005C, U+006E (`\n`).
 * U+000D (carriage return) encodes to U+005C, U+0072 (`\r`).
 * U+0022 (`"`) encodes to U+005C, U+0022 (`\"`).
 * U+0024 (`$`) encodes to U+005C, U+0024 (`\$`).
 * U+005C (<code>\</code>) encodes to U+005C, U+005C (<code>\\</code>).

The canonical encoding of a sequence of code points is that sequence of code
points surrounded by double quotes (`"`).


    ──────────────  ; Example of normalizing a \uXXXX escape sequence.
    "\u00E9" ⇥ "é"


    ───────────────  ; Newlines still need to be escaped.
    "\u000A" ⇥ "\n"


    ────────────  ; Single-quote text literals normalize to double-quote.
    ''"'' ⇥ "\""


### Single-quote text literals

Single-quote text literals (surrounded by `''`) are subject to indentation
stripping after interpreting escape codes and before substituting interpolated
values. The sequence of code points that the literal represents is the one after
stripping indents, and after removing a leading newline, if one exists. The
canonical encoding of a single-quote text literal is the canonical double-quote
encoding of the sequence of code points it represents.

The *content start* of a sequence of code points is the first code point that is
not a newline (U+000A) or a space (U+0020). For the purposes of indentation,
string interpolation counts as content.

The *indent* of a sequence of code points, is the shortest subsequence of spaces
(U+0020) points after a newline (U+000A), or after the content start, across the
text literal. The newline is not included in the indent. The indent might be
empty.

Stripping indents is done by, after every newline (U+000A), removing the indent.


    ────────────  ; The indent is empty, nothing is stripped.
    ''...
    '' ⇥ "...\n"


    ────────────  ; The leading newline is stripped. The indent is empty.
    ''
    1
    2'' ⇥ "1\n2"


    ────────────  ; The leading newline is stripped. The indent is two spaces.
    ''
      1
      2'' ⇥ "1\n2"


    ────────────  ; The leading newline is stripped. The indent is two spaces.
    ''
      1
        2
    '' ⇥ "1\n  2\n"


    ────────────  ; The leading newline is stripped. The indent is two spaces.
    ''
        1
      2
    '' ⇥ "  1\n2\n" ; The two spaces in front of "2" were a prefix of the indent.


## Equivalence

Equivalence is a relationship between two expression of the form:


    l ≡ r


Two expressions are equivalent if they are identical when both are β-normalized
and then α-normalized:


    l₀ ⇥ l₁   l₁ ↦ e   r₀ ⇥ r₁   r₁ ↦ e
    ───────────────────────────────────
    l₀ ≡ r₀


Note that this definition of equivalence does not include η-equivalence, so
`λ(f : Bool → Bool) → λ(x : Bool) → f x` and `λ(f : Bool → Bool) → f` are not
equivalent.

## Function check

The function check governs which types of functions that our pure type system
permits.

This function check is a judgment of the form:

    c₀ ↝ c₁

... where:

* `c₀` (an input constant, either `Type` or `Kind`) is the type of the
  function's input type
* `c₁` (an input constant, either `Type` or `Kind`) is the type of the
  function's output type

Dhall forbids dependent function types, but permits all other function
types.

The following rule enables support for functions from terms to terms (i.e.
"term-level" functions):


    ───────────
    Type ↝ Type


For example, these are term-level functions permitted by the above rule:

    Natural/even

    λ(x : Bool) → x != False

The following rule enables support for functions from types to terms (i.e.
"polymorphic" functions):


    ───────────
    Kind ↝ Type


For example, these are polymorphic functions permitted by the above rule:

    List/head

    λ(a : Type) → λ(x : a) → x

The following rule enables support for functions from types to types (i.e.
"type-level" functions):


    ───────────
    Kind ↝ Kind


For example, these are type-level functions permitted by the above rule:

    List

    λ(m : Type) → [ m ] → m

However, Dhall does not support dependently-typed functions, so there is no rule
for `Type ↝ Kind`.  Dhall omits support for dependent function types because
that would entail robustly detecting non-trivial type-level equivalences.

## Type inference

Type inference is a judgment of the form:

    Γ ⊢ t : T

... where:

* `Γ` (an input context) is the type inference context which relates
  identifiers to their types
* `t` (an input expression) is the term to infer the type of
* `T` (the output expression) is the inferred type

Type inference also type-checks the input expression, too, to ensure that it is
well-typed.

To infer the type of a closed expression, supply an empty context:

    ε ⊢ t : T

This judgment guarantees the invariant that the inferred type is safe to
normalize.

### Reduction

Additionally, there is a separate helper judgment for inferring a type reduced
to normal form:


    Γ ⊢ a : A₀   A₀ ⇥ A₁
    ────────────────────
    Γ ⊢ a :⇥ A₁


This judgment is identical to the judgment for type inference except that this
judgment returns the inferred type in normal form.

### Constants

The first rule is that the inferred type of `Type` is `Kind`:


    ───────────────
    Γ ⊢ Type : Kind


In other words, `Kind` is the "type of types" and `Kind` serves as the
foundation of the type system.

Note that you cannot infer the type of `Kind` as there is nothing above `Kind`
in the type system's hierarchy.  Inferring the type of `Kind` is a type error.

### Variables

Infer the type of a variable by looking up the variable's type in the context:


    Γ ⊢ T : k
    ──────────────────
    Γ, x : T ⊢ x@0 : T


Since `x` is a synonym for `x@0`, you can shorten this rule to:


    Γ ⊢ T : k
    ──────────────────
    Γ, x : T ⊢ x : T


The order of types in the context matters because there can be multiple type
annotations in the context for the same variable.  The DeBruijn index associated
with each variable disambiguates which type annotation in the context to use:


    Γ ⊢ x@n : T
    ────────────────────────  ; 0 < n
    Γ, x : A ⊢ x@(1 + n) : T


    Γ ⊢ x@n : T
    ───────────────────────  ; x ≠ y
    Γ, y : A ⊢ x@n : T


If the natural number associated with the variable is greater than or equal to
the number of type annotations in the context matching the variable then that is
a type error.

### `Bool`

`Bool` is a `Type`:


    ───────────────
    Γ ⊢ Bool : Type


`True` and `False` have type `Bool`:


    ───────────────
    Γ ⊢ True : Bool


    ────────────────
    Γ ⊢ False : Bool


An `if` expression takes a predicate of type `Bool` and returns either the
`then` or `else` branch of the expression, both of which must be the same type:


    Γ ⊢ t :⇥ Bool
    Γ ⊢ l : L
    Γ ⊢ r : R
    Γ ⊢ L :⇥ Type
    Γ ⊢ R :⇥ Type
    L ≡ R
    ──────────────────────────
    Γ ⊢ if t then l else r : L


Note that an `if` expression can only return a term.  More generally, if the
`if` expression returns a value whose type is not a `Type` then that is a type
error.

If the predicate is not a `Bool` then that is a type error.

If the two branches of the `if` expression do not have the same type then that
is a type error.

All of the logical operators take arguments of type `Bool` and return a result
of type `Bool`:


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l || r : Bool


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l && r : Bool


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l == r : Bool


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l != r : Bool


If the operator arguments do not have type `Bool` then that is a type error.

### `Natural`

`Natural` is a type:


    ──────────────────
    Γ ⊢ Natural : Type


`Natural` number literals have type `Natural`:


    ───────────────
    Γ ⊢ n : Natural


The arithmetic operators take arguments of type `Natural` and return a result of
type `Natural`:


    Γ ⊢ x :⇥ Natural   Γ ⊢ y :⇥ Natural
    ─────────────────────────────────
    Γ ⊢ x + y : Natural


    Γ ⊢ x :⇥ Natural   Γ ⊢ y :⇥ Natural
    ─────────────────────────────────
    Γ ⊢ x * y : Natural


If the operator arguments do not have type `Natural` then that is a type error.

The built-in functions on `Natural` numbers have the following types:


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Natural/build : (∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural) → Natural


    ──────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Natural/fold : Natural → ∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural


    ───────────────────────────────────
    Γ ⊢ Natural/isZero : Natural → Bool


    ─────────────────────────────────
    Γ ⊢ Natural/even : Natural → Bool


    ────────────────────────────────
    Γ ⊢ Natural/odd : Natural → Bool


    ─────────────────────────────────────────
    Γ ⊢ Natural/toInteger : Natural → Integer


    ─────────────────────────────────
    Γ ⊢ Natural/show : Natural → Text


### `Text`


`Text` is a type:


    ───────────────
    Γ ⊢ Text : Type


`Text` literals have type `Text`:


    ──────────────
    Γ ⊢ "…" : Text


The `Text` concatenation operator takes arguments of type `Text` and returns a
result of type `Text`:


    Γ ⊢ x :⇥ Text   Γ ⊢ y :⇥ Text
    ─────────────────────────────
    Γ ⊢ x ++ y : Text


If the operator arguments do not have type `Text`, then that is a type error.

### `List`

`List` is a function from a `Type` to another `Type`:


    ──────────────────────
    Γ ⊢ List : Type → Type


A `List` literal's type is inferred either from the type of the elements (if
non-empty) or from the type annotation (if empty):


    Γ ⊢ T :⇥ Type
    ──────────────────────────
    Γ ⊢ ([] : List T) : List T


    t : T₀   T₀ :⇥ Type   [ ts… ] :⇥ List T₁   T₀ ≡ T₁
    ──────────────────────────────────────────────────
    Γ ⊢ [t, ts…] : List T₀


Note that the above rules forbid `List` elements that are `Type`s.  More
generally, if the element type is not a `Type` then that is a type error.

If the list elements do not all have the same type then that is a type error.

If an empty list does not have a type annotation then that is a type error.

The `List` concatenation operator takes arguments that are both `List`s of the
same type and returns a `List` of the same type:


    Γ ⊢ x :⇥ List A₀
    Γ ⊢ y :⇥ List A₁
    A₀ ≡ A₁
    ───────────────────
    Γ ⊢ x # y : List A₀


If the operator arguments are not `List`s, then that is a type error.

If the arguments have different element types, then that is a type error.

The built-in functions on `List`s have the following types:


    ───────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ List/build : ∀(a : Type) → (∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list) → List a


    ────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ List/fold : ∀(a : Type) → List a → ∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list


    ────────────────────────────────────────────────
    Γ ⊢ List/length : ∀(a : Type) → List a → Natural


    ───────────────────────────────────────────────
    Γ ⊢ List/head ∀(a : Type) → List a → Optional a


    ─────────────────────────────────────────────────
    Γ ⊢ List/last : ∀(a : Type) → List a → Optional a


    ─────────────────────────────────────────────────────────────────────────────
    Γ ⊢ List/indexed : ∀(a : Type) → List a → List { index : Natural, value : a }


    ────────────────────────────────────────────────
    Γ ⊢ List/reverse : ∀(a : Type) → List a → List a


### `Optional`

`Optional` is a function from a `Type` to another `Type`:


    ──────────────────────────
    Γ ⊢ Optional : Type → Type


An `Optional` literal's type is inferred from the mandatory type annotation:


    Γ ⊢ A : Type
    ──────────────────────────────────
    Γ ⊢ ([] : Optional A) : Optional A


    Γ ⊢ A₀ : Type   Γ ⊢ a : A₁   A₀ ≡ A₁
    ───────────────────────────────────────
    Γ ⊢ ([ a ] : Optional A₀) : Optional A₀


Note that the above rules forbid an `Optional` element that is a `Type`.  More
generally, if the element type is not a `Type` then that is a type error.

If the element is present and does not match the type annotation then that is a
type error.

The built-in functions on `Optional` values have the following types:


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Optional/fold : ∀(a : Type) → Optional a → ∀(optional : Type) → ∀(just : a → optional) → ∀(nothing : optional) → optional


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Optional/build : ∀(a : Type) → (∀(optional : Type) → ∀(just : a → optional) → ∀(nothing : optional) → optional) → Optional a


### Records

Record types are "anonymous", meaning that they are uniquely defined by the
names and types of their fields and the order of fields does not matter:

A record can either store term-level values and functions:


    ─────────────
    Γ ⊢ {} : Type


    Γ ⊢ T :⇥ Type   Γ ⊢ { xs… } :⇥ Type
    ───────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x : T, xs… } : Type


... or store types (if it is non-empty):


    Γ ⊢ T :⇥ Kind   T ≡ Type
    ────────────────────────
    Γ ⊢ { x : T } : Kind


    Γ ⊢ T :⇥ Kind   T ≡ Type   Γ ⊢ { xs… } :⇥ Kind
    ──────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x : T, xs… } : Kind


... but not both.  If one field is a term-level value or function and another
field is a type-level value or function then that is a type error.

If the type of a field is not `Type` or `Kind` then that is a type error.

If two fields have the same name, then that is a type error.

Record values are also anonymous:


    ────────────
    Γ ⊢ {=} : {}


    Γ ⊢ t : T   Γ ⊢ T :⇥ Type   Γ ⊢ { xs… } :⇥ { ts… }
    ──────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x = t, xs… } : { x : T, ts… }


    Γ ⊢ t : T   Γ ⊢ T :⇥ Kind
    ─────────────────────────
    Γ ⊢ { x = t } : { x : T }


    Γ ⊢ t : T   Γ ⊢ T :⇥ Kind   T ≡ Type   Γ ⊢ { xs… } :⇥ { ts… }
    ─────────────────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x = t, xs… } : { x : T, ts… }


You can only select field(s) from the record if they are present:


    Γ ⊢ e :⇥ { x : T, xs… }   Γ ⊢ { x : T, xs… } :⇥ Type
    ────────────────────────────────────────────────────
    Γ ⊢ e.x : T


    Γ ⊢ e :⇥ { x : T, xs… }   Γ ⊢ { x : T, xs… } :⇥ Kind
    ────────────────────────────────────────────────────
    Γ ⊢ e.x : T


    Γ ⊢ e :⇥ { ts… }   Γ ⊢ { ts… } :⇥ Type
    ──────────────────────────────────────
    Γ ⊢ e.{} : {}


    Γ ⊢ e :⇥ { x : T, ts… }   Γ ⊢ { x : T, ts… } :⇥ Kind
    ────────────────────────────────────────────────────
    Γ ⊢ e.{ x } : { x : T }


    Γ ⊢ e :⇥ { x : T, ts₀… }   Γ ⊢ e.{ xs… } :⇥  { ts₁… }
    ─────────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ e.{ x, xs… } : { x : T, ts₁… }


If you select a field from a value that is not a record, then that is a type
error.

If the field is absent from the record then that is a type error.

Recursive record merge requires that both arguments are records:


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ r :⇥ {}
    ───────────────────
    Γ ⊢ l ∧ r : { ls… }


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ∧ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ l.a ∧ r.a : A₂
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ───────────────────────────────
    Γ ⊢ l ∧ r : { a : A₂, ts… }


If the operator arguments are not records then that is a type error.

If they share a field in common that is not a record then that is a type error.

Non-recursive right-biased merge also requires that both arguments are records:


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ r :⇥ {}
    ───────────────────
    Γ ⊢ l ⫽ r : { ls… }


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⫽ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ───────────────────────────────
    Γ ⊢ l ⫽ r : { a : A₀, ts… }


If the operator arguments are not records then that is a type error.

Recursive record type merge requires that both arguments are record type
literals.  Any conflicting fields must be safe to recursively merge:


    Γ ⊢ l :⇥ Type
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Type
    r ⇥ {}
    ────────────────
    Γ ⊢ l ⩓ r : Type


    Γ ⊢ l :⇥ Type
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Type
    r ⇥ { a : A, rs… }
    Γ ⊢ { ls… } ⩓ { rs… } : T
    ─────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⩓ r : Type


    Γ ⊢ l :⇥ Type
    l ⇥ { a : A₀, ls… }
    Γ ⊢ r :⇥ Type
    r ⇥ { a : A₁, rs… }
    Γ ⊢ l.a ⩓ r.a : T₀
    Γ ⊢ { ls… } ⩓ { rs… } : T₁
    ─────────────────────────────
    Γ ⊢ l ⩓ r : Type


    Γ ⊢ l :⇥ Kind
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Kind
    r ⇥ { a : A }
    ────────────────
    Γ ⊢ l ⩓ r : Kind


    Γ ⊢ l :⇥ Kind
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Kind
    r ⇥ { a : A, rs… }
    Γ ⊢ { ls… } ⩓ { rs… } : T
    ─────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⩓ r : Kind


    Γ ⊢ l :⇥ Kind
    l ⇥ { a : A₀, ls… }
    Γ ⊢ r :⇥ Kind
    r ⇥ { a : A₁, rs… }
    Γ ⊢ l.a ⩓ r.a : T₀
    Γ ⊢ { ls… } ⩓ { rs… } : T₁
    ─────────────────────────────
    Γ ⊢ l ⩓ r : Kind


If the operator arguments are not record types then that is a type error.

If they share a field in common that is not a record type then that is a type
error.

### Unions

Union types are "anonymous", meaning that they are uniquely defined by the names
and types of their alternatives and the order of alternatives does not matter:


    ─────────────
    Γ ⊢ <> : Type


    Γ ⊢ T :⇥ Type   Γ ⊢ < ts… > :⇥ Type
    ───────────────────────────────────  ; x ∉ < ts… >
    Γ ⊢ < x : T | ts… > : Type


    Γ ⊢ T :⇥ Kind   Γ ⊢ < ts… > :⇥ Type
    ───────────────────────────────────  ; x ∉ < ts… >
    Γ ⊢ < x : T | ts… > : Type


Note that the above rule allows storing values, types, and type-level functions
in unions.  However, if the type of the alternative is not `Type` or `Kind` then
that is a type error.

If two alternatives share the same name then that is a type error.

Union values are also anonymous:


    Γ ⊢ t : T   Γ ⊢ < t : T | ts… > :⇥ Type
    ───────────────────────────────────────
    Γ ⊢ < x = t | ts… > : < x : T | ts… >


A `merge` expression is well-typed if there is a one-to-one correspondence
between the fields of the handler record and the alternatives of the union:


    Γ ⊢ t :⇥ {}   Γ ⊢ u :⇥ <>   Γ ⊢ T :⇥ Type
    ─────────────────────────────────────────
    Γ ⊢ (merge t u : T) : T


    Γ ⊢ t :⇥ { y : ∀(x : A₀) → T₀, ts… }
    Γ ⊢ u :⇥ < y : A₁ | us… >
    Γ ⊢ (merge { ts… } < us… > : T₂) : T₃
    A₀ ≡ A₁
    ↑(-1, x, 0, T₀) = T₁
    T₁ ≡ T₂
    ────────────────────────────────────  ; `x` not free in `T₀`
    Γ ⊢ (merge t u : T₂) : T₂


    Γ ⊢ t :⇥ { y : ∀(x : A₀) → T₀, ts… }
    Γ ⊢ u :⇥ < y : A₁ | us… >
    Γ ⊢ (merge { ts… } < us… > : T₁) : T₂
    A₀ ≡ A₁
    ↑(-1, x, 0, T₀) = T₁
    ────────────────────────────────────  ; `x` not free in `T₀`
    Γ ⊢ merge t u : T₀


If the first argument of a `merge` expression is not a record then that is a
type error.

If the second argument of a `merge` expression is not a union then that is a
type error.

If you `merge` an empty union without a type annotation then that is a type
error.

If the `merge` expression has a type annotation that is not a `Type` then that
is a type error.

If there is a handler without a matching alternative then that is a type error.

If there is an alternative without a matching handler then that is a type error.

If a handler is not a function, then that is a type error.

If the handler's input type does not match the corresponding alternative's type
then that is a type error.

If there are two handlers with different output types then that is a type error.

If a `merge` expression has a type annotation that doesn't match every handler's
output type then that is a type error.

A `constructors` expression is only well-formed if the argument normalizes to a
union type literal:


    Γ ⊢ u : c   u ⇥ <>
    ───────────────────────
    Γ ⊢ constructors u : {}


    Γ ⊢ u : c   u ⇥ < x₀ : T₀ | x₁ : T₁ | xs… >
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ constructors u : { x₀ : ∀(x₀ : T₀) → < x₀ : T₀ | x₁ : T₁ | xs… >, x₁ : ∀(x₁ : T₁) → < x₀ : T₀ | x₁ : T₁ | xs… >, … }


If the union argument is any other type of expression (including a variable)
then that is a type error.

### `Integer`

`Integer` is a type:


    ──────────────────
    Γ ⊢ Integer : Type


`Integer` literals have type `Integer`:


    ────────────────
    Γ ⊢ ±n : Integer


The built-in functions on `Integer` have the following types:


    ─────────────────────────────────
    Γ ⊢ Integer/show : Integer → Text


    ───────────────────────────────────────
    Γ ⊢ Integer/toDouble : Integer → Double


### `Double`

`Double` is a type:


    ──────────────────
    Γ ⊢ Double : Type


`Double` literals have type `Double`:


    ────────────────
    Γ ⊢ n.n : Double


The built-in `Double/show` function has the following type:


    ───────────────────────────────
    Γ ⊢ Double/show : Double → Text


### Functions

A function type is only well-typed if the input and output type are well-typed
and if the inferred input and output type are allowed by the function check:


    Γ₀ ⊢ A :⇥ i   ↑(1, x, 0, (Γ₀, x : A)) = Γ₁   Γ₁ ⊢ B :⇥ o   i ↝ o
    ────────────────────────────────────────────────────────────────
    Γ₀ ⊢ ∀(x : A) → B : o


If the input or output type is neither a `Type` nor a `Kind` then that is a type
error.

The function check disallows dependent function types but allows all other
function types.  If the function type is a dependent function type then that is
a type error.

An unquantified function type `A → B` is a short-hand for `∀(_ : A) → B`.  Note
that the `_` does *not* denote some unused type variable but rather denotes the
specific variable named `_` (which is a valid variable name and this variable
named `_` may in fact be present within `B`).  For example, this is a well-typed
judgment:

    ε ⊢ Type → ∀(x : _) → _ : Type

... because it is equivalent to:

    ε ⊢ ∀(_ : Type) → ∀(x : _) → _ : Type

The type of a λ-expression is a function type whose input type (`A`) is the same
as the type of the bound variable and whose output type (`B`) is the same as the
inferred type of the body of the λ-expression (`b`).


    ↑(1, x, 0, (Γ₀, x : A)) = Γ₁   Γ₁ ⊢ b : B   Γ₀ ⊢ ∀(x : A) → B : c
    ─────────────────────────────────────────────────────────────────
    Γ₀ ⊢ λ(x : A) → b : ∀(x : A) → B


Note that the above rule requires that the inferred function type must be
well-typed.  The type-checking step for the function type triggers a function
check which disallows dependent function types.

The type system ensures that function application is well-typed, meaning that
the input type that a function expects matches the inferred type of the
function's argument:


    Γ ⊢ f :⇥ ∀(x : A₀) → B₀
    Γ ⊢ a₀ : A₁
    A₀ ≡ A₁
    ↑(1, x, 0, a₀) = a₁
    B₀[x ≔ a₁] = B₁
    ↑(-1, x, 0, B₁) = B₂
    ───────────────────────
    Γ ⊢ f a₀ : B₂


If the function does not have a function type, then that is a type error.

If the inferred input type of the function does not match the inferred type of
the function argument then that is a type error.

### `let` expressions

For the purposes of type-checking, an expression of the form:

    let x : A = a₀ in b₀

... is **not** semantically identical to:

    (λ(x : A) → b₀) a₀

`let` differs in behavior in order to support "type synonyms", such as:

    let t : Type = Integer in 1 : t

If you were to desugar that to:

    (λ(t : Type) → 1 : t) Integer

... then that would not be a well-typed expression, even though the `let`
expression would be well-typed.


    Γ ⊢ a₀ : A₁
    Γ ⊢ A₀ : i
    A₀ ≡ A₁
    a₀ ⇥ a₁
    ↑(1, x, 0, a₁) = a₂
    b₀[x ≔ a₂] = b₁
    ↑(-1, x, 0, b₁) = b₂
    Γ ⊢ b₂ : B
    ─────────────────────────────
    Γ ⊢ let x : A₀ = a₀ in b₀ : B


    Γ ⊢ a₀ : A
    a₀ ⇥ a₁
    ↑(1, x, 0, a₁) = a₂
    b₀[x ≔ a₂] = b₁
    ↑(-1, x, 0, b₁) = b₂
    Γ ⊢ b₂ : B
    ────────────────────────
    Γ ⊢ let x = a₀ in b₀ : B


If the `let` expression has a type annotation that doesn't match the type of
the right-hand side of the assignment then that is a type error.

### Type annotations

The inferred type of a type annotation is the annotation.  Type-checking also
verifies that the annotation matches the inferred type of the annotated
expression:


    Γ ⊢ T₀ : i   Γ ⊢ t : T₁   T₀ ≡ T₁
    ─────────────────────────────────
    Γ ⊢ (t : T₀) : T₀


Note that the above rule permits kind annotations, such as `List : Type → Type`.

If the inferred type of the annotated expression does not match the type
annotation then that is a type error.

### Imports

An expression with unresolved imports cannot be type-checked

## Import resolution

You can embed external Dhall expressions using an inline URL, file, or
environment variable.  For example:

```
    -- Expression imported from a URL
    let concatSep = http://prelude.dhall-lang.org/Prelude/Text/concatSep sha256:fa909c0b2fd4f9edb46df7ff72ae105ad0bd0ae00baa7fe53b0e43863f9bd34a

in  { name = env:USER as Text  -- Expression imported from the environment
    , age  = 23
    , hobbies = concatSep ", " [ "piano", "reading", "skiing" ]
    } : ./schema.dhall  -- Expression imported from a file
```

You can protect imports with integrity checks if you append SHA-256 hash (such
as the `concatSep` import above) and you can also import a value as raw `Text`
by appending `as Text` (such as the `env:USER` import above).

Imported expressions can transitively import other expressions.  For example,
the `./schema.dhall` file imported above might also import other files:

```
-- ./schema.dhall

{ name : ./schema/name.dhall
, age  : ./schema/age.dhall
, hobbies : ./schema/hobbies.dhall
}
```

... and if `./schema/hobbies.dhall` contained a relative import such as:

```
-- ./schema/hobbies.dhall

List ./hobby.dhall
```

... then the relative import of `./hobby.dhall` would actually refer to
`./schema/hobby.dhall`.  This is known as "import chaining": resolving imports
relative to the current expression's location.

### Directories and files

A directory is a list of path components, represented using the following
inductive notation:

```
directory =  ε                    ; 0 path components
          /  directory/component  ; At least 1 path component
```

In a complete file path, the directory comprises all but the last path component
and the last path component is the file.

Here are some examples of imports highlighting which part is the directory and
which part is the file:

```
directory
↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
/ipfs/QmdtKd5Q7tebdo6rXfZed4kN6DXmErRQHJ4PsNCtca9GbB/Prelude/List/replicate
                                                                 ↑↑↑↑↑↑↑↑↑↑
                                                                 file


  directory
  ↓↓↓
../../Monoid  ; The first ".." does not belong to the path, but the second ".."
     ↑↑↑↑↑↑↑  ; does belong to the path
     file


directory = ε

./map  ; The directory is empty and does not include the leading "."
 ↑↑↑↑
 file


 directory
 ↓↓↓↓↓↓↓↓
~/.config/development.dhall
         ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
         file


                   directory
                   ↓↓↓↓↓↓↓↓↓↓↓
https://example.com/share/user/biography.dhall  ; URLs have directories, too
                              ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
                              file
```

See the grammar for more details about where the directory and file begin and
end.

### Canonicalization of directories

Canonicalization of directories is a function of the following form:

    canonicalize(directory₀) = directory₁

... where:

* `directory₀` (the input) is the directory to canonicalize
* `directory₁` (the output) is the canonicalized directory

Canonicalization has no effect on the empty directory:


    ───────────────────
    canonicalize(ε) = ε


Canonicalization removes path components named ".":


    canonicalize(directory₀) = directory₁
    ───────────────────────────────────────
    canonicalize(directory₀/.) = directory₁


Canonicalizing a path component named ".." removes that path component along
with the parent path component:


    canonicalize(directory₀) = directory₁/..
    ──────────────────────────────────────────────
    canonicalize(directory₀/..) = directory₁/../..


    canonicalize(directory₀) = directory₁/component
    ───────────────────────────────────────────────  ; If "component" is not
    canonicalize(directory₀/..) = directory₁         ; ".."


... unless there is no path component left to remove:


    canonicalize(directory₀) = ε
    ────────────────────────────
    canonicalize(directory₀/..) = /..


Canonicalization ignores directory components other than "." and "..":


    canonicalize(directory₀) = directory₁
    ─────────────────────────────────────────────────────────  ; If no other
    canonicalize(directory₀/component) = directory₁/component  ; rule matches


### Canonicalization of imports

You can also canonicalize imports by canonicalizing their directories.

Canonicalization of imports is a function of the following form:

    canonicalize(import₀) = import₁

... where:

* `import₀` (the input) is the import to canonicalize
* `import₁` (the output) is the canonicalized import

To canonicalize an import, canonicalize any path components belonging to the
directory:


    canonicalize(path₀) = path₁
    ─────────────────────────────────────
    canonicalize(path₀ file) = path₁ file


    canonicalize(path₀) = path₁
    ─────────────────────────────────────────
    canonicalize(. path₀ file) = . path₁ file


    canonicalize(path₀) = path₁
    ───────────────────────────────────────────
    canonicalize(.. path₀ file) = .. path₁ file


    canonicalize(path₀) = path₁
    ─────────────────────────────────────────
    canonicalize(~ path₀ file) = ~ path₁ file


    canonicalize(path₀) = path₁
    ─────────────────────────────────────────────────────────────────────────
    canonicalize(https://authority path₀ file) = https://authority path₁ file


    ───────────────────────────
    canonicalize(env:x) = env:x


Note that environment variables have no path components and are therefore
already in canonical form.

### Chaining directories

Chaining imports requires the ability to chain directories, using a function of
the following form:

    directory₀ </> directory₁ = directory₂

... where:

* `directory₀` (an input) is a directory
* `directory₁` (an input) is a directory
* `directory₂` (the output) is the concatenation of the two input directories

Chaining directories is the same as list concatenation of each input directory's
path components:


    ─────────────────
    path </> ε = path


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────
    path₀ </> path₁/component = path₂/component


### Chaining imports

The Dhall language supports absolute imports, such as URLs or absolute paths to
files.  Chaining imports that mix absolute and relative imports involves
computing the longest path relative to the last absolute import using a function
of the following form:

    import₀ </> import₁ = import₂

... where:

* `import₀` (an input) is the path of the parent relative to the current
  location
* `import₁` (an input) is the path of the child relative to the parent
* `import₂` (the output) is the path of the child relative to the current
  location

If a parent file path imports a child via a relative file path then you chain
their directories but prefer the file name of the child:


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────
    path₀ file₀ </> . path₁ file₁ = path₂ file₁


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────────
    . path₀ file₀ </> . path₁ file₁ = . path₂ file₁


    path₀ </> path₁ = path₂
    ─────────────────────────────────────────────────
    .. path₀ file₀ </> . path₁ file₁ = .. path₂ file₁


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────────
    ~ path₀ file₀ </> . path₁ file₁ = ~ path₂ file₁


    path₀ </> path₁ = path₂
    ───────────────────────────────────────────────────────────────────────────────
    https://authority path₀ file₀ </> . path₁ file₁ = https://authority path₂ file₁


If the child import begins with a "..", add that as a path component in between
the parent and child directories:


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ──────────────────────────────────────────────
    path₀ file₀ </> .. path₂ file₁ = path₃ file₁


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ────────────────────────────────────────────────
    . path₀ file₀ </> .. path₂ file₁ = . path₃ file₁


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ──────────────────────────────────────────────────
    .. path₀ file₀ </> .. path₂ file₁ = .. path₃ file₁


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ────────────────────────────────────────────────
    ~ path₀ file₀ </> .. path₂ file₁ = ~ path₃ file₁


    path₀ </> /.. = path₁   path₁ </> path₂ = path₃
    ────────────────────────────────────────────────────────────────────────────────
    https://authority path₀ file₀ </> .. path₂ file₁ = https://authority path₃ file₁


If the child is an absolute import, then the path to the parent import does not
matter and you import the child directly:


    ─────────────────────────────  ; If no other rule matches
    import₀ </> import₁ = import₁


### Duplicate imports

Importing the same canonical path twice must always return the same result
within a single run of the import resolution phase.  That means that import
resolution for the following program:

```
[ ./integer.dhall, ./integer.dhall ]
```

... must replace both occurrences of `./integer.dhall` with the same
expression if import resolution succeeds.

A conforming impleentation can satisfy this obligation by caching imports, using
the canonical path as the lookup key.  Then for a duplicate import the
implementation can either:

* reuse the cached import to avoid having to retrieve the path again, or:
* retrieve the path again and fail import resolution if the result changed
  since the last retrieval

The following semantics model this by treating the context of importable
expressions as a pure, lazy, and unordered map from canonical paths to
expressions stored at those paths:

### Import resolution judgment

The import resolution phase replaces all imports with the expression located
at that import, transitively resolving imports within the imported expression if
necessary.

Import resolution is a function of the following form:

    Γ ⊢ e₀ @ here ⇒ e₁

... where

* `Γ` (an input) is an unordered map from imports to expressions
* `e₀` (an input) is the expression to resolve
* `here` (an input) is the current import, used to resolve relative imports
* `e₁` (the output) is the import-free resolved expression

If an expression is an import (i.e. a URL, file path, or environment variable),
then you retrieve the expression from the canonicalized path and transitively
resolve imports within the retrieved expression:

    here </> import₀ = import₁
    canonicalize(import₁) = import₂
    Γ(import₂) = e₀                  ; Retrieve the expression
    Γ ⊢ e₀ @ import₂ ⇒ e₁
    ε ⊢ e₁ : T
    ───────────────────────────────  ; `import₀` is a file, URL or environment
    Γ ⊢ import₀ @ here ⇒ e₁          ; import and `import₀` is not `missing`


By using the `?` operator, expressions are alternatively resolved, in left-to-right order.
Pure expressions are always resolved, `missing` never resolves, and imports
might not resolve in cases like:
- an environment variable is not defined
- file doesn't exist
- URL is not reachable


    Γ ⊢ e₀ @ here ⇒ e₂
    ─────────────────────────
    Γ ⊢ (e₀ ? e₁) @ here ⇒ e₂


    Γ ⊢ e₁ @ here ⇒ e₂
    ─────────────────────────  ; if `e₀` fails to resolve
    Γ ⊢ (e₀ ? e₁) @ here ⇒ e₂


Carefully note that the fully resolved import must successfully type-check with
an empty context.  Imported expressions may not contain any free variables.

For all other cases, recursively descend into sub-expressions:


    ────────────────────
    Γ ⊢ x@n @ here ⇒ x@n


    Γ ⊢ A₀ @ here ⇒ A₁   Γ ⊢ b₀ @ here ⇒ b₁
    ──────────────────────────────────────────
    Γ ⊢ λ(x : A₀) → b₀ @ here ⇒ λ(x : A₁) → b₁


    …


    ──────────────────────
    Γ ⊢ Kind @ here ⇒ Kind
