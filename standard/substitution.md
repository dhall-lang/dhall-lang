# Substitution

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

## Table of contents

* [Variables](#variables)
* [Bound variables](#bound-variables)
* [Imports](#imports)
* [Other](#other)

## Variables

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

## Bound variables

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


    a₀[x@n ≔ e₀] = a₁
    ↑(1, x, 0, e₀) = e₁
    b₀[x@(1 + n) ≔ e₁] = b₁
    ───────────────────────────────────────────────
    (let x = a₀ in b₀)[x@n ≔ e₀] = let x = a₁ in b₁


    a₀[x@n ≔ e₀] = a₁
    ↑(1, y, 0, e₀) = e₁
    b₀[x@n ≔ e₁] = b₁
    ──────────────────────────────────────────────  ; x ≠ y
    (let y = a₀ in b₀)[x@n ≔ e₀] = let y = a₁ in b₁


## Imports

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


## Other

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
    "s"[x@n ≔ e] = "s"


    t₀[x@n ≔ e] = t₁   "ss₀…"[x@n ≔ e] = "ss₁…"
    ───────────────────────────────────────────
    "s₀${t₀}ss₀…"[x@n ≔ e] = "s₀${t₁}ss₁…"


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


    < xs₀… >[x@n ≔ e] = < xs₁… >
    ──────────────────────────────────────
    < x₀ | xs₀… >[x@n ≔ e] = < x₀ | xs₁… >


    t₀[x@n ≔ e] = t₁
    ──────────────────────────────────
    < x₀ = t₀ >[x@n ≔ e] = < x₀ = t₁ >


    T₁₀[x@n ≔ e] = T₁₁   < x₀ = t₀₀ | xs₀… >[x@n ≔ e] = < x₀ = t₀₁ | xs₁… >
    ────────────────────────────────────────────────────────────────────────
    < x₀ = t₀₀ | x₁ : T₁₀ | xs₀… >[x@n ≔ e] = < x₀ = t₀₁ | x₁ : T₁₁ | xs₁… >


    a₀[x@n ≔ e] = a₁
    ────────────────────────────
    (Some a₀)[x@n ≔ e] = Some a₁


    ────────────────────
    None[x@n ≔ e] = None


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


    ──────────────────────────────
    Text/show[x@n ≔ e] = Text/show


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


    ────────────────────
    Sort[x@n ≔ e] = Sort

