# Shift

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

## Table of contents

* [Variables](#variables)
* [Bound variables](#bound-variables)
* [Imports](#imports)
* [Other](#other)

## Variables

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


## Bound variables

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


    ↑(d, x, m, A₀) = A₁
    ↑(d, x, m, a₀) = a₁
    ↑(d, x, m + 1, b₀) = b₁
    ─────────────────────────────────────────────────────────
    ↑(d, x, m, let x : A₀ = a₀ in b₀) = let x : A₁ = a₁ in b₁


    ↑(d, x, m, a₀) = a₁
    ↑(d, x, m + 1, b₀) = b₁
    ───────────────────────────────────────────────
    ↑(d, x, m, let x = a₀ in b₀) = let x = a₁ in b₁


Again, the bound variable, `x`, is not in scope for its own type, `A₀`, so do
not increase the lower bound, `m`, when shifting the bound variable's type.

Note that Dhall's `let` expressions do not allow recursive definitions so the
bound variable, `x`, is not in scope for the right-hand side of the assignment,
`a₀`, so do not increase the lower bound, `m`, when shifting the right-hand side
of the assignment.

Descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁
    ↑(d, x, m, a₀) = a₁
    ↑(d, x, m, b₀) = b₁
    ─────────────────────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, let y : A₀ = a₀ in b₀) = let y : A₁ = a₁ in b₁


    ↑(d, x, m, a₀) = a₁
    ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, let y = a₀ in b₀) = let y = a₁ in b₁


## Imports

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


## Other

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
    ↑(d, x, m, "s") = "s"


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, "ss₀…") = "ss₁…"
    ─────────────────────────────────────────────────
    ↑(d, x, m, "s₀${t₀}ss₀…") = "s₀${t₁}ss₁…"


    ───────────────────
    ↑(d, x, m, {}) = {}


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, { xs₀… }) = { xs₁… }
    ─────────────────────────────────────────────────────
    ↑(d, x, m, { x₀ : T₀, xs₀… }) = { x₀ : T₁, xs₁… }


    ─────────────────────
    ↑(d, x, m, {=}) = {=}


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, { xs₀… }) = { xs₁… }
    ─────────────────────────────────────────────────────
    ↑(d, x, m, { x₀ = t₀, xs₀… }) = { x₀ = t₁, xs₁… }


    ───────────────────
    ↑(d, x, m, <>) = <>


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, < xs₀… >) = < xs₁… >
    ─────────────────────────────────────────────────────
    ↑(d, x, m, < x₀ : T₀ | xs₀… >) = < x₀ : T₁ | xs₁… >


    ↑(d, x, m, < xs₀… >) = < xs₁… >
    ─────────────────────────────────────────
    ↑(d, x, m, < x₀ | xs₀… >) = < x₀ | xs₁… >


    ↑(d, x, m, t₀) = t₁
    ─────────────────────────────────────
    ↑(d, x, m, < x₀ = t₀ >) = < x₀ = t₁ >


    ↑(d, x, m, T₁₀) = T₁₁
    ↑(d, x, m, < x₀ = t₀₀ | xs₀… >) = < x₀ = t₀₁ | xs₁… >
    ───────────────────────────────────────────────────────────────────────────
    ↑(d, x, m, < x₀ = t₀₀ | x₁ : T₁₀ | xs₀… >) = < x₀ = t₀₁ | x₁ : T₁₁ | xs₁… >


    ↑(d, x, m, a₀) = a₁
    ─────────────────────────────
    ↑(d, x, m, Some a₀) = Some a₁


    ───────────────────────
    ↑(d, x, m, None) = None


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


    ─────────────────────────────────
    ↑(d, x, m, Text/show) = Text/show


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


    ───────────────────────
    ↑(d, x, m, Sort) = Sort

