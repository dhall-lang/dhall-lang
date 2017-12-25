# Type-checking and normalization

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
m, n = +0 / +1 + n     ; Natural numbers

d = n / -n             ; Integers

"…" = "" / "." ++ "…"  ; Text ("." is any character and "…" is any string)

x, y                   ; Variables

; Mnemonics:
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
;     L  = type of the input term "l"
;     R  = type of the output term "r"
;     E  = type of the term "e"
;     T  = type of the term "t"
;     U  = type of the term "u"
;
; Kinds are lowercase:
;
;     k = "k"ind
;     i = kind of a function's "i"nput type
;     o = kind of a function's "o"utput type
;
; Note that these are only informal mnemonics.  Dhall is a pure type system,
; which means that many places in the syntax permit types, terms, and kinds.
; The typing judgments are the authoritative rules for what expressions are
; permitted and forbidden.
a, b, f, l, r, e, t, u, A, B, L, R, E, T, U, k, i, o
  = x@n                      ; Identifier
                             ; (`x` is short-hand for `x@0`)
  / λ(x : A) → b             ; Anonymous function
  / ∀(x : A) → B             ; Function type
                             ; (`A → B` is short-hand for `∀(_ : A) → B`)
  / let x : A = a in b       ; Let expression with type annotation
  / let x     = a in b       ; Let expression without type annotation
  / if t then l else r       ; If-Then-Else expression
  / merge t u : T            ; Union elimination with type annotation
  / merge t u                ; Union elimination
  / [] : List T              ; Empty list literals with type annotation
  / [ t, ts… ]               ; Non-empty list literals
  / [   ] : Optional T       ; Empty optional literal
  / [ t ] : Optional T       ; Non-empty optional literal
  / t : T                    ; Type annotation
  / l || r                   ; Boolean or
  / l + r                    ; Natural addition
  / l ++ r                   ; Text append
  / l # r                    ; List append
  / l && r                   ; Boolean and
  / l ∧ r                    ; Recursive record merge
  / l ⫽ r                    ; Non-recursive right-biased record merge
  / l * r                    ; Natural multiplication
  / l == r                   ; Boolean equality
  / l != r                   ; Boolean inequality
  / f a                      ; Function application
  / e.x                      ; Field selection
  / n.n                      ; Double-precision floating point literal
  / +n                       ; Natural number literal
  / n                        ; Integer literal
  / "…"                      ; Text literal
  / { x : T, … }             ; Record type
  / { x = t, … }             ; Non-empty record literal
  / < x : T | xs… >          ; Union type
  / < x = t | y : T | ys… >  ; Union literal
  / Natural/build            ; Natural introduction
  / Natural/fold             ; Natural elimination
  / Natural/isZero           ; Test if zero
  / Natural/even             ; Test if even
  / Natural/odd              ; Test if odd
  / Natural/toInteger        ; Convert Natural to Integer
  / Natural/show             ; Convert Natural to Text
  / Integer/show             ; Convert Integer to Text
  / Double/show              ; Convert Double to Text
  / List/build               ; List introduction
  / List/fold                ; List elimination
  / List/length              ; Length of list
  / List/head                ; First element of list
  / List/last                ; Last element of list
  / List/indexed             ; Tag elements with index
  / List/reverse             ; Reverse list
  / Optional/fold            ; Optional introduction
  / Optional/build           ; Optional elimination
  / Bool                     ; Bool type
  / Optional                 ; Optional type
  / Natural                  ; Natural type
  / Integer                  ; Integer type
  / Double                   ; Double type
  / Text                     ; Text type
  / List                     ; List type
  / True                     ; True term
  / False                    ; False term
  / Type                     ; Type of terms
  / Kind                     ; Type of types
```

Carefully note that the syntax does not include imports because you cannot infer
the type of a Dhall expression that has unresolved imports.  In other words,
import resolution is a distinct phase that must precede type checking and type
inference.

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


To be precise, `x@n` refers to the "nth" bound variable named `x` counting
outwards from where the variable is referenced.

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

* `d` (an input) is the amount to add to the variable indices
    * `d` is always `-1` or `1`
* `x` (an input) is is the name of the free variable(s) to shift
    * variables with a different name are unaffected by the shift function
* `m` (an input) is the minimum index to shift
    * `m` always starts out at `0`
    * `m` increases by one when descending past a bound variable named `x`
    * variables with an index lower than `m` are unaffected by the shift
      function
* `e₀` (an input) is the expression to shift
* `e₁` (the output) is the shifted expression

For example:

    ↑(1, x, 0, x) = x@1

    ↑(1, x, 1, x) = x

    ↑(1, x, 0, y) = y

    ↑(-1, x, 0, x@1) = x

    ↑(1, x, 0, List x) = List x@1

### Variables

The first rule is that the shift function will increase the index of any
variable if the variable name matches and the variable's index is greater than
or equal to the minimum index to shift:


    ───────────────────────────  ; m <= n
    ↑(d, x, m, x@n) = x@(n + d)


We don't shift the index if the index falls short of the lower bound or if the
variable name does not match:


    ─────────────────────  ; m > n
    ↑(d, x, m, x@n) = x@n


    ─────────────────────  ; x ≠ y
    ↑(d, x, m, x@n) = y@n


### Bound variables

The shift function is designed to shift only free variables.  For example, the
shift function should have no effect on a closed expression:

    ↑(1, x, 0, λ(x : Type) → x) = λ(x : Type) → x

This implies that we must increment the minimum bound when we descend into a
λ-expression that binds a variable of the same name in order to avoid shifting
the bound variable:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m + 1, b₀) = b₁
    ─────────────────────────────────────────────
    ↑(d, x, m, λ(x : A₀) → b₀) = λ(x : A₁) → b₁


... otherwise we descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, λ(y : A₀) → b₀) = λ(y : A₁) → b₁


Similarly, we increase the minimum bound when we descend into a function type
that binds a variable of the same name:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m + 1, B₀) = B₁
    ─────────────────────────────────────────────
    ↑(d, x, m, ∀(x : A₀) → B₀) = ∀(x : A₁) → B₁


... otherwise we descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, B₀) = B₁
    ───────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, ∀(y : A₀) → B₀) = ∀(y : A₁) → B₁


Similarly, we increase the minimum bound when descending into a `let` expression
that binds a variable of the same name:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, a₀) = a₁   ↑(d, x, m + 1, b₀) = b₁
    ───────────────────────────────────────────────────────────────────
    ↑(d, x, m, let x : A₀ = a₀ in b₀) = let x : A₁ = a₁ in b₁


    ↑(d, x, m, a₀) = a₁   ↑(d, x, m + 1, b₀) = b₁
    ───────────────────────────────────────────────
    ↑(d, x, m, let x = a₀ in b₀) = let x = a₁ in b₁


... otherwise we descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, a₀) = a₁   ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, let y : A₀ = a₀ in b₀) = let y : A₁ = a₁ in b₁


    ↑(d, x, m, a₀) = a₁   ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, let y = a₀ in b₀) = let y = a₁ in b₁


Note that Dhall's `let` expressions do not allow recursive definitions so the
bound variable is not in scope for the right-hand side of the assignment (i.e.
`a₀` in the above rules).  Therefore we do not need to shift the minimum index
when descending into the right-hand side of the `let` expression.

### Induction

No other Dhall expressions bind variables, so the remaining rules just descend
as normal:


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


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, ts₀…) = ts₁…
    ─────────────────────────────────────────────
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
    ↑(d, x, m, l₀ * r₀) = l₁ * r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ == r₀) = l₁ == r₁


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ != r₀) = l₁ != r₁


    ↑(d, x, m, f₀) = f₁   ↑(d, x, m, a₀) = a₁
    ─────────────────────────────────────────
    ↑(d, x, m, f₀ a₀) = f₁ a₁


    ↑(d, x, m, e₀) = e₁
    ──────────────────────
    ↑(d, x, m, e.x) = e₁.x


    ─────────────────────
    ↑(d, x, m, n.n) = n.n


    ───────────────────
    ↑(d, x, m, +n) = +n


    ─────────────────
    ↑(d, x, m, n) = n


    ─────────────────────
    ↑(d, x, m, "…") = "…"


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, xs₀…) = xs₁…
    ───────────────────────────────────────────────
    ↑(d, x, m, { x : T₀, xs₀… }) = { x : T₁, xs₁… }


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, xs₀…) = xs₁…
    ───────────────────────────────────────────────
    ↑(d, x, m, { x = t₀, xs₀… }) = { x = t₁, xs₁… }


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, xs₀…) = xs₁…
    ─────────────────────────────────────────────────
    ↑(d, x, m, < x : T₀ | xs₀… >) = < x : T₁ | xs₁… >


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, T₀) = T₁   ↑(d, x, m, ys₀…) = ys₁…
    ───────────────────────────────────────────────────────────────────
    ↑(d, x, m, < x = t₀ | y : T₀ | ys₀… >) = < x = t₁ | y : T₁ | ys₁… >


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
      / Γ, x : E  ; A context extended with a type annotation for a variable

Contexts are ordered and there can be multiple type annotations in the context
for the same variable.

## Shift context

You can also shift a context by shifting each expression in that context:

    ─────────────────
    ↑(d, x, m, ε) = ε


    ↑(d, x, m, Γ₀) = Γ₁   ↑(d, x, m, E₀) = E₁
    ─────────────────────────────────────────
    ↑(d, x, m, (Γ₀, x : E₀)) = Γ₁, x : E₁


## Substitution

β-reduction requires support for substitution, which has the following form:

    e₀[x@n ≔ a] = e₁

... where:

* `e₀` (an input) is the expression that you want to transform
* `x@n` (an input) is the variable that you want to substitute with another
  expression
* `a` (an input) is the expression that you want to substitute `x@n` with
* `e₁` (the output) is transformed expression where all occurrences of `x@n`
  have been replaced with `a`

`e[x ≔ a]` is short-hand for `e[x@0 ≔ a]`

For example:

    x[x ≔ Bool] = Bool

    y[x ≔ Bool] = y

    (List x)[x ≔ Bool] = List Bool

### Variables

Like shifting, we only need to pay special attention to the cases where we
bind variables or reference variables.

The first two rules govern when we can substitute a variable with the specified
expression:


    ────────────────
    x@n[x@n ≔ e] = e


    ──────────────────  ; x@n ≠ y@m
    y@m[x@n ≔ e] = y@m


In other words, we substitute the expression if the variable name and index
exactly match, but otherwise we do not substitute and leave the variable as-is.

### Bound variables

The substitution function is designed to only substitute free variables and
ignore bound variables.  The following few examples can help build an intuition
for how substitution uses the numeric index of the variable that we are
substituting for:

    (λ(x : Text) → x)[x ≔ True] = λ(x : Text) → x

    (λ(y : Text) → x)[x ≔ True] = λ(x : Text) → True

    (λ(x : Text) → x@1)[x ≔ True] = λ(x : Text) → True

    (λ(x : Text) → x@2)[x@1 ≔ True] = λ(x : Text) → True

    (λ(x : Text) → λ(x : Text) → x@3)[x@1 ≔ True] = λ(x : Text) → λ(x : Text) → True

We ensure that we only substitute free variables by increasing the index when a
new bound variable of the same name is in scope:


    …   b₀[x@(+1 + n) ≔ e₁] = b₁   …
    ────────────────────────────────
    …


We also take care to avoid variable capture by shifting variables within the
expression that we substitute, like this:

    (λ(x : Type) → y)[y ≔ x] = λ(x : Type) → x@1

We avoid variable capture by shifting the expression to substitute when any new
bound variable is in scope, like this:


    …   ↑(1, x, 0, e₀) = e₁   …
    ───────────────────────────
    …


All of the following rules cover expressions that can bind variables:


    A₀[x@n ≔ e₀] = A₁   ↑(1, x, 0, e₀) = e₁   b₀[x@(+1 + n) ≔ e₁] = b₁
    ──────────────────────────────────────────────────────────────────
    (λ(x : A₀) → b₀)[x@n ≔ e₀] = λ(x : A₁) → b₁


    A₀[x@n ≔ e₀] = A₁   ↑(1, y, 0, e₀) = e₁   b₀[x@n ≔ e₁] = b₁
    ───────────────────────────────────────────────────────────  ; x ≠ y
    (λ(y : A₀) → b₀)[x@n ≔ e₀] = λ(y : A₁) → b₁


    A₀[x@n ≔ e₀] = A₁   ↑(1, x, 0, e₀) = e₁   B₀[x@(+1 + n) ≔ e₁] = B₁
    ──────────────────────────────────────────────────────────────────
    (∀(x : A₀) → B₀)[x@n ≔ e₀] = ∀(x : A₁) → B₁


    A₀[x@n ≔ e₀] = A₁   ↑(1, y, 0, e₀) = e₁   B₀[x@n ≔ e₁] = B₁
    ───────────────────────────────────────────────────────────  ; x ≠ y
    (∀(y : A₀) → B₀)[x@n ≔ e₀] = ∀(y : A₁) → B₁


    A₀[x@n ≔ e₀] = A₁
    a₀[x@n ≔ e₀] = a₁
    ↑(1, x, 0, e₀) = e₁
    b₀[x@(+1 + n) ≔ e₁] = b₁
    ─────────────────────────────────────────────────────────
    (let x : A₀ = a₀ in b₀)[x@n ≔ e₀] = let x : A₁ = a₁ in b₁


    A₀[x@n ≔ e₀] = A₁
    a₀[x@n ≔ e₀] = a₁
    ↑(1, y, 0, e₀) = e₁
    b₀[x@n ≔ e₁] = b₁
    ─────────────────────────────────────────────────────────  ; x ≠ y
    (let y : A₀ = a₀ in b₀)[x@n ≔ e₀] = let y : A₁ = a₁ in b₁


    a₀[x@n ≔ e₀] = a₁   ↑(1, x, 0, e₀) = e₁   b₀[x@(+1 + n) ≔ e₁] = b₁
    ──────────────────────────────────────────────────────────────────
    (let x = a₀ in b₀)[x@n ≔ e₀] = let x = a₁ in b₁


    a₀[x@n ≔ e₀] = a₁   ↑(1, y, 0, e₀) = e₁   b₀[x@n ≔ e₁] = b₁
    ───────────────────────────────────────────────────────────  ; x ≠ y
    (let y = a₀ in b₀)[x@n ≔ e₀] = let y = a₁ in b₁


### Induction

No other Dhall expressions bind variables, so the remaining rules just descend
as normal:


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


    t₀[x@n ≔ e] = t₁   ts₀…[x@n ≔ e] = ts₁…
    ───────────────────────────────────────
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
    (l₀ * r₀)[x@n ≔ e] = l₁ * r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ == r₀)[x@n ≔ e] = l₁ == r₁


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ != r₀)[x@n ≔ e] = l₁ != r₁


    f₀[x@n ≔ e] = f₁   a₀[x@n ≔ e] = a₁
    ───────────────────────────────────
    (f₀ a₀)[x@n ≔ e] = f₁ a₁


    e₀[x@n ≔ e] = e₁
    ────────────────────────
    (e₀.x₀)[x@n ≔ e] = e₁.x₀


    ──────────────────
    n.n[x@n ≔ e] = n.n


    ────────────────
    +n[x@n ≔ e] = +n


    ──────────────
    n[x@n ≔ e] = n


    ──────────────────
    "…"[x@n ≔ e] = "…"


    T₀[x@n ≔ e] = T₁   xs₀…[x@n ≔ e] = xs₁…
    ──────────────────────────────────────────────
    { x₀ : T₀, xs₀… }[x@n ≔ e] = { x₀ : T₁, xs₁… }


    t₀[x@n ≔ e] = t₁   xs₀…[x@n ≔ e] = xs₁…
    ──────────────────────────────────────────────
    { x₀ = t₀, xs₀… }[x@n ≔ e] = { x₀ = t₁, xs₁… }


    t₀[x@n ≔ e] = t₁   T₀[x@n ≔ e] = T₁   ys₀…[x@n ≔ e] = ys₁…
    ──────────────────────────────────────────────────────────
    < x₀ = t₀ | y : T₀ | ys₀… >[x@n ≔ e] = < x₀ = t₁ | y : T₁ | ys₁… >


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


    ─────────────────────────
    Intger[x@n ≔ e] = Integer


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


## Normalization

Normalization is a function of the following form:

    t₀ ⇥ t₁

... where:

* `t₀` (the input) is the expression to normalize
* `t₁` (the output) is the normalized expression

Normalization evaluates all β-reducible expressions:

    (λ(x : Bool) → x != False) True ⇥ False

Normalization also evaluates all built-in functions if they are fully saturated:

    List/length Integer [1, 2, 3] ⇥ +3

Normalization works under λ, meaning that the body of an unapplied λ-expression
can be normalized:

    λ(x : Bool) → List/length Integer [1, 2, 3] ⇥ λ(x : Bool) → +3

Dhall is a total language that is strongly normalizing, so evaluation order has
no effect on the language semantics and a conforming implementation can select
any evaluation strategy.

Also, note that the semantics specifies several built-in types, functions and
operators that conforming implementations must support.  Implementations are
encouraged to implement the following functions and operators in more efficient
ways than the following reduction rules so long as the result of normalization
is the same.

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


Otherwise, you normalize the predicate and both branches of the `if` expression:


    t₀ ⇥ t₁   l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────────────────────────────────  ; If no other rule matches
    if t₀ then l₀ else r₀ ⇥ if t₁ then l₁ else r₁


Even though `True`, `False`, and `if` expression suffice for all `Bool` logic,
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


Otherwise, you normalize each argument:


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


Otherwise, you normalize each argument:


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


... or if both arguments normalize to a `False` literal:


    l ⇥ False   r ⇥ False
    ─────────────────────
    l == r ⇥ True


Otherwise, you normalize each argument:


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


... or if both arguments normalize to a `True` literal:


    l ⇥ True   r ⇥ True
    ───────────────────
    l != r ⇥ False


Otherwise, you normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ != r₀ ⇥ l₁ != r₁


### `Natural`

The `Natural` number type is in normal form:


    ─────────────────
    Natural ⇥ Natural


`Natural` number literals are in normal form:


    ───────
    +n ⇥ +n


The `Natural/fold` function is the canonical elimination function for natural
numbers:


    f ⇥ Natural/fold +0 B g   b ⇥ t₁
    ────────────────────────────────
    f b ⇥ t₁


    f ⇥ Natural/fold (+1 + n) B g   g (Natural/fold n B g b) ⇥ t₁
    ─────────────────────────────────────────────────────────────
    f b ⇥ t₁


`Natural/build` is the canonical introduction function for `Natural` numbers and
is the inverse of `Natural/fold`, which leads to the following rules for
`build/fold` fusion:


    f ⇥ Natural/build   a ⇥ Natural/fold b
    ──────────────────────────────────────
    f a ⇥ b


    f ⇥ Natural/fold   a ⇥ Natural/build b
    ──────────────────────────────────────
    f a ⇥ b


In the absence of fusion, `Natural/build` falls back on the following rule:


    f ⇥ Natural/build   g Natural (λ(x : Natural) → x + +1) +0 ⇥ b
    ──────────────────────────────────────────────────────────────
    f g ⇥ b


Even though `Natural/fold` and `Natural/build` suffice for all `Natural` number
programming, Dhall also supports `Natural` number literals and built-in
functions and operators on `Natural` numbers, both for convenience and
efficiency.

Use machine addition to simplify the "plus" operator if both arguments normalize
to `Natural` literals:


    l ⇥ +m   r ⇥ +n
    ───────────────  ; "+m + +n" means "use machine addition"
    l + r ⇥ +m + +n


Also, simplify the "plus" operator if either argument normalizes to a `+0`
literal:


    l ⇥ +0   r₀ ⇥ r₁
    ────────────────
    l + r₀ ⇥ r₁


    r ⇥ +0   l₀ ⇥ l₁
    ────────────────
    l₀ + r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ + r₀ ⇥ l₁ + r₁


Use machine multiplication to simplify the "times" operator if both arguments
normalize to a `Natural` literal:


    l ⇥ +m   r ⇥ +n
    ───────────────  ; "+m * +n" means "use machine addition"
    l + r ⇥ +m * +n


    l ⇥ +0
    ──────────
    l * r ⇥ +0


Also, simplify the "plus" operator if either argument normalizes to either a
`+0` literal:

    r ⇥ +0
    ──────────
    l * r ⇥ +0


    l ⇥ +1   r₀ ⇥ r₁
    ────────────────
    l * r₀ ⇥ r₁


... or a `+1` literal:


    r ⇥ +1   l₀ ⇥ l₁
    ────────────────
    l₀ * r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ * r₀ ⇥ l₁ * r₁


`Natural/isZero` detects whether or not a `Natural` number is `+0`:


    f ⇥ Natural/isZero   a ⇥ +0
    ───────────────────────────
    f a ⇥ True


    f ⇥ Natural/isZero   a ⇥ +1 + n
    ───────────────────────────────  ; "+1 + n" means "a `Natural` literal
    f a ⇥ False                      ; greater than `+0`"


`Natural/even` detects whether or not a `Natural` number is even.  You should
implement this using a more efficient machine operation even though this
function is defined using induction:


    f ⇥ Natural/even   a ⇥ +0
    ─────────────────────────
    f a ⇥ True


    f ⇥ Natural/even   a ⇥ +1
    ─────────────────────────
    f a ⇥ False


    f ⇥ Natural/even
    a ⇥ +1 + n
    Natural/odd n ⇥ b
    ─────────────────  ; "+1 + n" means "a `Natural` literal greater than `+0`"
    f a ⇥ b


`Natural/odd` detects whether or not a `Natural` number is odd.  You should
implement this using a more efficient machine operation even though this is
function is defined using induction:


    f ⇥ Natural/odd   a ⇥ +0
    ────────────────────────
    f a ⇥ False


    f ⇥ Natural/odd   a ⇥ +1
    ────────────────────────
    f a ⇥ True


    f ⇥ Natural/odd
    a ⇥ +1 + n
    Natural/even n ⇥ b
    ──────────────────  ; "+1 + n" means "a `Natural` literal greater than `+0`"
    f a ⇥ b


`Natural/toInteger` transforms a `Natural` number into the corresponding
`Integer`:


    f ⇥ Natural/toInteger   a ⇥ +n
    ──────────────────────────────
    f a ⇥ n


`Natural/show` transforms a `Natural` number into a `Text` literal representing
valid Dhall code for representing that `Natural` number:


    f ⇥ Natural/show   a ⇥ +n
    ─────────────────────────
    f a ⇥ "+n"


Carefully note that the rendered `Natural` number should include a leading
`+` sign.

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


`Text` literals are in normal form:


    ─────────
    "…" ⇥ "…"


Use machine concatenation to simplify the "text concatenation" operator if both
arguments normalize to `Text` literals:


    l ⇥ "l…"   r ⇥ "r…"
    ─────────────────────  ; "l…" ++ "r…" means "use machine concatenation"
    l ++ r ⇥ "l…" ++ "r…"


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

The `List` type is in normal form:


    ───────────
    List ⇥ List


Normalizing a `List` normalizes each field and the type annotation:


    T₀ ⇥ T₁
    ──────────────────────────
    [] : List T₀ ⇥ [] : List T₁


    x₀ ⇥ x₁   xs₀… ⇥ xs₁…
    ───────────────────────────
    [ x₀, xs₀… ] ⇥ [ x₁, xs₁… ]


Lists are defined here via induction as if they were linked lists, but a real
implementation might represent them using another data structure under the hood.
Dhall does not impose time complexity requirements on list operations.

`List/fold` is the canonical elimination function for `List`s:


    f ⇥ List/fold A₀ ([] : List A₁) B g   b₀ ⇥ b₁
    ─────────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ List/fold A₀ [ a, as… ] B g   g a (List/fold A₀ as… B g b₀) ⇥ b₁
    ────────────────────────────────────────────────────────────────────
    f b₀ ⇥ b₁


`List/build` is the canonical introduction function for `List`s and is the
inverse of `List/fold`, which leads to the following rules for `build/fold`
fusion:


    f ⇥ List/fold A₀   a ⇥ List/build A₁ b
    ──────────────────────────────────────
    f a ⇥ b


    f ⇥ List/build A₀   a ⇥ List/fold A₁ b
    ──────────────────────────────────────
    f a ⇥ b


In the absence of fusion, `List/build` falls back on the following rule:


    f ⇥ List/build A
    g (List A) (λ(a : A) → λ(as : List A) → [ a ] # as) ([] : List A) ⇥ b
    ───────────────────────────────────────────────────────────────────────
    f g ⇥ b


Even though `List/build` and `List/fold` suffice for all `List` operations,
Dhall also supports built-in functions and operators on `List`s, both for
convenience and efficiency.

Use machine concatenation to simplify the "list concatenation" operator if both
arguments normalize to `List` literals:


    ls₀ ⇥ [ l₁, ls₁… ]
    rs₀ ⇥ [ r₁, rs₁… ]
    [ l₁, ls₁… ] # [ r₁, rs₁… ] ⇥ t
    ──────────────────────────────   ;  "[ l₁, ls₁… ] # [ r₁, rs₁… ]" means
    ls₀ # rs₀ ⇥ t                    ;  "use machine concatenation"


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
    f a ⇥ +0


    f ⇥ List/length A₀   as₀ ⇥ [ a, as₁… ]   +1 + List/length A₀ as₁… ⇥ n
    ─────────────────────────────────────────────────────────────────────
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
    ────────────────────────────────────────────────────────────────────
    f as ⇥ [ { index = +0, value = a₀ }, { index = +1, value = a₁ }, … ]


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

The `Optional` type is in normal form:


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


`Optional/fold` is the canonical elimination function for `Optional` values:


    f ⇥ Optional/fold A₀ ([ a ] : Optional A₁) B₀ g   g a ⇥ b₁
    ──────────────────────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ Optional/fold A₀ ([] : Optional A₁) B₀ g   b₀ ⇥ b₁
    ─────────────────────────────────────────────────────
    f b₀ ⇥ b₁


`Optional/build` is the canonical introduction function for `Optional` values
and is the inverse of `Optional/fold`, which leads to the following rules for
`build/fold` fusion:


    f ⇥ Optional/fold A₀   a ⇥ Optional/build A₁ b
    ──────────────────────────────────────────────
    f a ⇥ b


    f ⇥ Optional/build A₀   a ⇥ Optional/fold A₁ b
    ──────────────────────────────────────────────
    f a ⇥ b


In the absence of fusion, `Optional/build` falls back on the following rule:


    f ⇥ Optional/build A
    g (Optional A) (λ(a : A) → [ a ] : Optional A) ([] : Optional A) ⇥ b
    ────────────────────────────────────────────────────────────────────
    f g ⇥ b


All of the built-in functions on `Optional` values are in normal form:


    ─────────────────────────────
    Optional/fold ⇥ Optional/fold


    ───────────────────────────────
    Optional/build ⇥ Optional/build


### Record types

Normalizing a record type normalizes the type of each field:


    T₀ ⇥ T₁   xs₀… ⇥ xs₁…
    ───────────────────────────────────
    { x : T₀, xs₀… } ⇥ { x : T₁, xs₁… }


### Record values

Normalizing a record normalizes each field:


    ───────
    {} ⇥ {}


    t₀ ⇥ t₁   xs₀… ⇥ xs₁…
    ───────────────────────────────────
    { x = t₀, xs₀… } ⇥ { x = t₁, xs₁… }


Simplify a record selection if the argument is a record literal:


    e ⇥ { x = t, … }
    ────────────────
    e.x ⇥  t


The type system ensures that the selected field must be present.

Otherwise, normalize the argument:

    e₀ ⇥ e₁
    ───────────  ; If no other rule matches
    e₀.x ⇥ e₁.x


Recursive record merge combines two records, recursively merging any fields that
collide.  The type system ensures that colliding fields must be records:


    l ⇥ {}   r₀ ⇥ r₁
    ────────────────
    l ∧ r₀ ⇥ r₁


    r ⇥ {}   l₀ ⇥ l₁
    ────────────────
    l₀ ∧ r ⇥ l₁


    ls₀ ⇥ { x = l₁, ls₁… }
    rs₀ ⇥ { x = r₁, rs₁… }
    l₁ ∧ r₁ ⇥ t
    ls₁… ∧ rs₁… ⇥ ts…
    ──────────────────────────
    ls₀ ∧ rs₀ ⇥ { x = t, ts… }


    ls₀ ⇥ { x = l₁, ls₁… }   ls₁… ∧ rs ⇥ ls₂…
    ─────────────────────────────────────────  ; x ∉ rs
    ls₀ ∧ rs ⇥ { x = l₁, ls₂… }


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ∧ r₀ ⇥ l₁ ∧ r₁


Right-biased record merge is non-recursive.  Field collisions are resolved by
preferring the field from the right record and discarding the colliding field
from the left record:


    r ⇥ e
    ──────────
    {} ⫽ r ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }   rs₀ ⇥ { x = r₁, rs₁… }   ls₁… ⫽ rs₁… ⇥ ts…
    ───────────────────────────────────────────────────────────────────
    ls₀ ⫽ rs₀ ⇥ { x = r₁, ts… }


    ls₀ ⇥ { x = l₁, ls₁… }   ls₁… ⫽ rs ⇥ ls₂…
    ─────────────────────────────────────────  ; x ∉ rs
    ls₀ ⫽ rs ⇥ { x = l₁, ls₂… }


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ⫽ r₀ ⇥ l₁ ⫽ r₁


### Union types

Normalizing a union type normalizes the type of each alternative:


    T₀ ⇥ T₁   xs₀… ⇥ xs₁…
    ─────────────────────────────────────
    < x : T₀ | xs₀… > ⇥ < x : T₁ | xs₁… >


### Union values

Normalizing a union literal is the same as normalizing the specified value and
the type of each alternative:


    t₀ ⇥ t₁   T₀ ⇥ T₁   ys₀… ⇥ ys₁…
    ───────────────────────────────────────────────────────
    < x = t₀ | y : T₀ | ys₀… > ⇥ < x = t₁ | y : T₁ | ys₁… >


`merge` expressions are the canonical way to eliminate a union literal.  The
first argument to `merge` is a record of handlers and the second argument is a
union value.  You apply the handler of the same label to the selected value of
the union literal:


    t ⇥ { x = f, … }   u ⇥ < x = a | y : T | … >   f a ⇥ e
    ──────────────────────────────────────────────────────
    merge t u : T ⇥ e


    t₀ ⇥ t₁   u₀ ⇥ u₁   T₀ ⇥ T₁
    ───────────────────────────────────  ; If no other rule matches
    merge t₀ u₀ : T₀ ⇥ merge t₁ u₁ : T₁


    t ⇥ { x = f, … }   u ⇥ < x = a | y : T | … >   f a ⇥ e
    ──────────────────────────────────────────────────────
    merge t u ⇥ e


    t₀ ⇥ t₁   u₀ ⇥ u₁
    ─────────────────────────  ; If no other rule matches
    merge t₀ u₀ ⇥ merge t₁ u₁


### `Integer`

The `Integer` type is in normal form:


    ─────────────────
    Integer ⇥ Integer


An `Integer` literal is in normal form:


    ─────
    n ⇥ n


`Integer/show` transforms an `Integer` into a `Text` literal representing valid
Dhall code for representing that `Integer` number:


    f ⇥ Integer/show   a ⇥ n
    ────────────────────────
    f a ⇥ "n"


The `Integer/show` function is in normal form:


    ───────────────────────────
    Integer/show ⇥ Integer/show


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

Normalizing an anonymous function normalizes the type of the bound variable and
the body of the function:


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


### Function types

Normalizing a function type normalizes the types of the input and output:


    A₀ → A₁   B₀ → B₁
    ───────────────────────────────
    ∀(x : A₀) → B₀ ⇥ ∀(x : A₁) → B₁


### `let` expressions

An expression of the form:

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

A type annotation of the form:

    t : T

... is semantically identical to:

    (λ(x : T) → x) t

... and the normalization rule for type annotations reflects that semantic
equivalence:


    t₀ ⇥ t₁
    ───────────
    t₀ : T ⇥ t₁


### Constants

Type-checking constants are in normal form:


    ───────────
    Type ⇥ Type


    ───────────
    Kind ⇥ Kind


## Equivalence

Equivalence is a relationship between two expression of the form:

    e₀ ≡ e₁

Two expressions are equivalent if they are α-equivalent when normalized.

Note that this notion of equivalence does not include η-equivalence, since
normalization does not η-expand or η-reduce expressions.

## Kind check

The kind check governs which types of functions that our pure type system
permits.

Dhall forbids dependent function types, but permits all other function
types.

The following rule enables support for functions from terms to terms (i.e.
"term-level" functions):


    ─────────────
    ⊢ Type ↝ Type


For example, these are term-level functions:

    Natural/even

    λ(x : Bool) → x != False

The following rule enables support for functions from types to terms (i.e.
"polymorphic" functions):


    ─────────────
    ⊢ Kind ↝ Type


For example, these are polymorphic functions:

    List/head

    λ(a : Type) → λ(x : a) → x

The following rule enables support for functions from types to types (i.e.
"type-level" functions):


    ─────────────
    ⊢ Kind ↝ Kind


For example, these are type-level functions:

    List

    λ(m : Type) → [ m ] → m

However, Dhall does not support dependently-typed functions, so there is no rule
for `⊢ Type ↝ Kind`.  Dhall omits support for dependent function types because
that would entail robustly detecting non-trivial type-level equivalences.

## Type inference

Type inference is a judgment of the form:

    Γ ⊢ t : T

... where:

* `Γ` (an input) is the type inference context which relates identifiers to
  their types
* `t` (an input) is the term to infer the type of
* `T` (the output) is the inferred type

To infer the type of a closed expression, supply an empty context:

    ε ⊢ t : T

### Reduction

Additionally, there is a separate helper judgment for inferring a type reduced
to normal form:


    Γ ⊢ a : A   A ⇥ B
    ─────────────────
    Γ ⊢ a :⇥ B


This judgment is essentially identical to the judgment for type inference except
that this judgment guarantees that the inferred type is in normal form.

### Types and Kinds

The first rule is that the inferred type of `Type` is `Kind`, no matter the
context:


    ───────────────
    ε ⊢ Type : Kind


In other words, `Kind` is the "type of types" and `Kind` serves as the
foundation of the type system.  Note that you cannot infer the type of `Kind`
as there is nothing above `Kind` in the type system's hierarchy.  Inferring the
type of `Kind` is a type error.

### Variables

You can infer the type of a variable by looking up the variable's type in the
context:


    Γ ⊢ T : k
    ──────────────────
    Γ, x : T ⊢ x@0 : T


Since `x` is a synonym for `x@0`, you can shorten this rule to:


    Γ ⊢ T : k
    ──────────────────
    Γ, x : T ⊢ x : T


The order of types in the context matters because there can be multiple type
annotations in the context for the same variable.  The natural number associated
with each variable disambiguates which type annotation in the context to use:


    Γ ⊢ A : k   Γ ⊢ x@n : T
    ─────────────────────────  ; 0 < n
    Γ, x : A ⊢ x@(+1 + n) : T


    Γ ⊢ A : k   Γ ⊢ x@n : T
    ───────────────────────  ; x ≠ y
    Γ, y : A ⊢ x@n : T


If the natural number associated with the variable is greater than or equal to
the number of type annotations in the context matching the variable then that is
a type error

Carefully note that the above two rules imply that each type stored in the
context must be well-typed.  This restriction ensures that we can safely
normalize any type retrieved from the context since well-typed terms will not
infinitely loop if normalized.

### Lambdas

You can create new (anonymous) functions using a λ:


    ↑(1, x, 0, (Γ₀, x : A)) = Γ₁   Γ₁ ⊢ b : B   Γ₀ ⊢ ∀(x : A) → B : k
    ─────────────────────────────────────────────────────────────────
    Γ₀ ⊢ λ(x : A) → b : ∀(x : A) → B


The type of a λ-expression is a function type whose input type (`A`) is the same
as the type of the bound variable and whose output type (`B`) is the same as the
inferred type of the body of the λ-expression (`b`).

Carefully note that the above rule requires that the inferred function type must
be well-typed.  The type-checking step for the function type disallows certain
types of functions.

### Function types

A function type is only well-typed if the input and output type are well-typed:


    Γ₀ ⊢ A :⇥ i   ↑(1, x, 0, (Γ₀, x : A)) = Γ₁   Γ₁ ⊢ B :⇥ o   ⊢ i ↝ o
    ──────────────────────────────────────────────────────────────────
    Γ₀ ⊢ ∀(x : A) → B : o


... and if the inferred kinds of the input and output type are allowed.  This
kind check disallows dependent function types.

The unquantified function type `A → B` is a short-hand for `∀(_ : A) → B`.  Note
that the `_` does **NOT** denote some unused type variable but rather denotes
the specific variable named `_` (which is a valid variable name and this
variable named `_` may in fact be present within `B`).  For example, this is a
well-typed judgment:

    ε ⊢ Type → _ : Kind

... because it is equivalent to:

    ε ⊢ ∀(_ : Type) → _ : Kind

### Function application

The type system ensures that function application is well-typed, meaning that
the input type that a function expects matches the inferred type of the
function's argument:


    Γ ⊢ f :⇥ ∀(x : A₀) → B₀
    Γ ⊢ a : A₁
    A₀ ≡ A₁
    ↑(1, x, 0, a₀) = a₁
    B₀[x ≔ a₁] = B₁
    ↑(-1, x, 0, B₁) = B₂
    ────────────────────────────────────────────
    Γ ⊢ f a₀ : B₂


If the inferred input type of the function does not match the inferred type of
the function argument then that is a type error

Carefully note that you should not check `A₀ ≡ A₁` until you have first checked
the two other preconditions.  This is because a β-equivalence check might not
terminate if one of `A₀` or `A₁` is not well-typed

### `let` expressions


    Γ₀ ⊢ a₀ : A₁
    Γ₀ ⊢ A₀ :⇥ i
    A₀ ≡ A₁
    ↑(1, x, 0, (Γ₀, x : A₀)) = Γ₁
    Γ₁ ⊢ b : B₀
    Γ₁ ⊢ B₀ :⇥ o
    ⊢ i ↝ o
    ↑(1, x, 0, a₀) = a₁
    B₀[x ≔ a₁]) = B₁
    ↑(-1, x, 0, B₁) = B₂
    ──────────────────────────────
    Γ₀ ⊢ let x : A₀ = a₀ in b : B₂


    Γ₀ ⊢ a₀ : A
    Γ₀ ⊢ A :⇥ i
    ↑(1, x, 0, (Γ₀, x : A)) = Γ₁
    Γ₁ ⊢ b : B₀
    Γ₁ ⊢ B ₀:⇥ o
    ⊢ i ↝ o
    ↑(1, x, 0, a₀) = a₁
    B₀[x ≔ a₁] = B₁
    ↑(-1, x, 0, B₁) = B₂
    ────────────────────────────
    Γ₀ ⊢ let x = a₀ in b : B₂


### `if`/`then`/`else`


    Γ ⊢ t :⇥ Bool
    Γ ⊢ l : L
    Γ ⊢ r : R
    Γ ⊢ L :⇥ Type
    Γ ⊢ R :⇥ Type
    L ≡ R
    ──────────────────────────
    Γ ⊢ if t then l else r : L


### `merge`




# TODO

* Consistently use + prefix for natural numbers
* Use … notation more precisely
* Does the valid context optimization from the Henk paper work?  If so, simplify
  typing judgments
* Reference Henk paper
