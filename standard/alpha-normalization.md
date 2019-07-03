# α-normalization

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


## Table of contents

* [Bound variables](#bound-variables)
* [Variables](#variables)
* [Imports](#imports)
* [Other](#other)

## Bound variables

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
    ───────────────────────────────  ; x ≠ _
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
    ───────────────────────────────  ; x ≠ _
    ∀(x : A₀) → B₀ ↦ ∀(_ : A₁) → B₄


    a₀ ↦ a₁
    A₀ ↦ A₁
    b₀ ↦ b₁
    ─────────────────────────────────────────────
    let _ : A₀ = a₀ in b₀ ↦ let _ : A₁ = a₁ in b₁


    a₀ ↦ a₁
    A₀ ↦ A₁
    ↑(1, _, 0, b₀) = b₁
    b₁[x ≔ _] = b₂
    ↑(-1, x, 0, b₂) = b₃
    b₃ ↦ b₄
    ─────────────────────────────────────────────  ; x ≠ _
    let x : A₀ = a₀ in b₀ ↦ let _ : A₁ = a₁ in b₄


    a₀ ↦ a₁
    b₀ ↦ b₁
    ───────────────────────────────────
    let _ = a₀ in b₀ ↦ let _ = a₁ in b₁


    a₀ ↦ a₁
    ↑(1, _, 0, b₀) = b₁
    b₁[x ≔ _] = b₂
    ↑(-1, x, 0, b₂) = b₃
    b₃ ↦ b₄
    ───────────────────────────────────  ; x ≠ _
    let x = a₀ in b₀ ↦ let _ = a₁ in b₄


## Variables

Variables are already in α-normal form:


    ─────────
    x@n ↦ x@n


If they are free variables then there is nothing to do because α-normalization
does not affect free variables.  If they were originally bound variables there
is still nothing to do because would have been renamed to `_` along the way by
one of the preceding rules.

## Imports

An expression with unresolved imports cannot be α-normalized.

## Other

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


    t₀ ↦ t₁   T₀ ↦ T₁
    ─────────────────
    t₀ : T₀ ↦ t₁ : T₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ || r₀ ↦ l₁ || r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ + r₀ ↦ l₁ + r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ ++ r₀ ↦ l₁ ++ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ # r₀ ↦ l₁ # r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ && r₀ ↦ l₁ && r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ ∧ r₀ ↦ l₁ ∧ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ ⫽ r₀ ↦ l₁ ⫽ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ ⩓ r₀ ↦ l₁ ⩓ r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────
    l₀ * r₀ ↦ l₁ * r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ == r₀ ↦ l₁ == r₁


    l₀ ↦ l₁   r₀ ↦ r₁
    ───────────────────
    l₀ != r₀ ↦ l₁ != r₁


    f₀ ↦ f₁   a₀ ↦ a₁
    ─────────────────
    f₀ a₀ ↦ f₁ a₁


    t₀ ↦ t₁
    ───────────
    t₀.x ↦ t₁.x


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
    "s" ↦ "s"


    "ss₀…" ↦ "ss₁…"   t₀ ↦ t₁
    ─────────────────────────────
    "s₀${t₀}ss₀…" ↦ "s₀${t₁}ss₁…"


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


    s₀ ↦ s₁   t₀ ↦ t₁
    ────────────────
    s₀.(t₀) ↦ s₁.(t₁)


    ───────
    <> ↦ <>


    T₀ ↦ T₁   < xs₀… > ↦ < xs₁… >
    ─────────────────────────────────────
    < x : T₀ | xs₀… > ↦ < x : T₁ | xs₁… >


    < xs₀… > ↦ < xs₁… >
    ───────────────────────────
    < x | xs₀… > ↦ < x | xs₁… >


    t₀ ↦ t₁
    ───────────────────────
    < x = t₀ > ↦ < x = t₁ >


    t₀ ↦ t₁   < xs₀… > ↦ < xs₁… >
    ─────────────────────────────────────
    < x = t₀ | xs₀… > ↦ < x = t₁ | xs₁… >


    a₀ ↦ a₁
    ─────────────────
    Some a₀ ↦ Some a₁


    ───────────
    None ↦ None


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


    ─────────────────────
    Text/show ↦ Text/show


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


    ───────────
    Sort ↦ Sort


