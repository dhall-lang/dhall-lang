# α-normalization

```haskell
module AlphaNormalization where

import Shift (shift)
import Substitution (substitute)
import Syntax (Expression(..), TextLiteral(..))
```

α-normalization is a function of the following form:

    t₀ ↦ t₁

... where:

* `t₀` (the input) is the expression to α-normalize
* `t₁` (the output) is the α-normalized expression

```haskell
alphaNormalize
    :: Expression  -- ^ @t₀@, the expression to α-normalize
    -> Expression  -- ^ @t₁@, the α-normalized expression
```

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


```haskell
alphaNormalize (Lambda "_" _A₀ b₀) = Lambda "_" _A₁ b₁
  where
    _A₁ = alphaNormalize _A₀

    b₁ = alphaNormalize b₀

alphaNormalize (Lambda x _A₀ b₀) = Lambda "_" _A₁ b₄
  where
    _A₁ = alphaNormalize _A₀

    b₁ = shift 1 "_" 0 b₀

    b₂ = substitute b₁ x 0 (Variable "_" 0)

    b₃ = shift (-1) x 0 b₂

    b₄ = alphaNormalize b₃
```


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


```haskell
alphaNormalize (Forall "_" _A₀ _B₀) = Forall "_" _A₁ _B₁
  where
    _A₁ = alphaNormalize _A₀

    _B₁ = alphaNormalize _B₀

alphaNormalize (Forall x _A₀ _B₀) = Forall "_" _A₁ _B₄
  where
    _A₁ = alphaNormalize _A₀

    _B₁ = shift 1 "_" 0 _B₀

    _B₂ = substitute _B₁ x 0 (Variable "_" 0)

    _B₃ = shift (-1) x 0 _B₂

    _B₄ = alphaNormalize _B₂
```


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


```haskell
alphaNormalize (Let "_" (Just _A₀) a₀ b₀) = Let "_" (Just _A₁) a₁ b₁
  where
    a₁ = alphaNormalize a₀

    _A₁ = alphaNormalize _A₀

    b₁ = alphaNormalize b₀

alphaNormalize (Let x (Just _A₀) a₀ b₀) = Let "_" (Just _A₁) a₁ b₄
  where
    a₁ = alphaNormalize a₀

    _A₁ = alphaNormalize _A₀

    b₁ = shift 1 "_" 0 b₀

    b₂ = substitute b₁ x 0 (Variable "_" 0)

    b₃ = shift (-1) x 0 b₂

    b₄ = alphaNormalize b₃

alphaNormalize (Let "_" Nothing a₀ b₀) = Let "_"  Nothing a₁ b₁
  where
    a₁ = alphaNormalize a₀

    b₁ = alphaNormalize b₀

alphaNormalize (Let x Nothing a₀ b₀) = Let "_" Nothing a₁ b₄
  where
    a₁ = alphaNormalize a₀

    b₁ = shift 1 "_" 0 b₀

    b₂ = substitute b₁ x 0 (Variable "_" 0)

    b₃ = shift (-1) x 0 b₂

    b₄ = alphaNormalize b₃
```

## Variables

Variables are already in α-normal form:


    ─────────
    x@n ↦ x@n


```haskell
alphaNormalize (Variable x n) = Variable x n
```

If they are free variables then there is nothing to do because α-normalization
does not affect free variables.  If they were originally bound variables there
is still nothing to do because would have been renamed to `_` along the way by
one of the preceding rules.

## Imports

An expression with unresolved imports cannot be α-normalized.

```haskell
alphaNormalize Import{} = error "Imports cannot be α-normalized"
```

## Other

No other Dhall expressions bind variables, so α-normalization just descends into
sub-expressions for the remaining rules:


    t₀ ↦ t₁   l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────────────────────────────────
    if t₀ then l₀ else r₀ ↦ if t₁ then l₁ else r₁



```haskell
alphaNormalize (If t₀ l₀ r₀) = If t₁ l₁ r₁
  where
    t₁ = alphaNormalize t₀

    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
```


    t₀ ↦ t₁   u₀ ↦ u₁   T₀ ↦ T₁
    ───────────────────────────────────
    merge t₀ u₀ : T₀ ↦ merge t₁ u₁ : T₁


    t₀ ↦ t₁   u₀ ↦ u₁
    ─────────────────────────
    merge t₀ u₀ ↦ merge t₁ u₁


```haskell
alphaNormalize (Merge t₀ u₀ (Just _T₀)) = Merge t₁ u₁ (Just _T₁)
  where
    t₁ = alphaNormalize t₀

    u₁ = alphaNormalize u₀

    _T₁ = alphaNormalize _T₀

alphaNormalize (Merge t₀ u₀ Nothing) = Merge t₁ u₁ Nothing
  where
    t₁ = alphaNormalize t₀

    u₁ = alphaNormalize u₀
```


    t₀ ↦ t₁   T₀ ↦ T₁
    ─────────────────────────────
    toMap t₀ : T₀ ↦ toMap t₁ : T₁


    t₀ ↦ t₁
    ───────────────────
    toMap t₀ ↦ toMap t₁


```haskell
alphaNormalize (ToMap t₀ (Just _T₀)) = ToMap t₁ (Just _T₁)
  where
    t₁ = alphaNormalize t₀

    _T₁ = alphaNormalize _T₀

alphaNormalize (ToMap t₀ Nothing) = ToMap t₁ Nothing
  where
    t₁ = alphaNormalize t₀
```


    T₀ ↦ T₁
    ─────────────────
    [] : T₀ ↦ [] : T₁


    t₀ ↦ t₁   [ ts₀… ] ↦ [ ts₁… ]
    ─────────────────────────────
    [ t₀, ts₀… ] ↦ [ t₁, ts₁… ]


```haskell
alphaNormalize (EmptyList _T₀) = EmptyList _T₁
  where
    _T₁ = alphaNormalize _T₀

alphaNormalize (NonEmptyList ts₀) = NonEmptyList ts₁
  where
    ts₁ = fmap adapt ts₀

    adapt t₀ = t₁
      where
        t₁ = alphaNormalize t₀
```

    t₀ ↦ t₁   T₀ ↦ T₁
    ─────────────────
    t₀ : T₀ ↦ t₁ : T₁


```haskell
alphaNormalize (Annotation t₀ _T₀) = Annotation t₁ _T₁
  where
    t₁ = alphaNormalize t₀

    _T₁ = alphaNormalize _T₀
```


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


    l₀ ↦ l₁   r₀ ↦ r₁
    ─────────────────────
    l₀ === r₀ ↦ l₁ === r₁


```haskell
alphaNormalize (Operator l₀ op r₀) = Operator l₁ op r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
```


    f₀ ↦ f₁   a₀ ↦ a₁
    ─────────────────
    f₀ a₀ ↦ f₁ a₁


```haskell
alphaNormalize (Application f₀ a₀) = Application f₁ a₁
  where
    f₁ = alphaNormalize f₀

    a₁ = alphaNormalize a₀
```


    t₀ ↦ t₁
    ───────────
    t₀.x ↦ t₁.x


```haskell
alphaNormalize (Field t₀ x) = Field t₁ x
  where
    t₁ = alphaNormalize t₀
```


    t₀ ↦ t₁
    ───────────────────────
    t₀.{ xs… } ↦ t₁.{ xs… }


```haskell
alphaNormalize (ProjectByLabels t₀ xs) = ProjectByLabels t₁ xs
  where
    t₁ = alphaNormalize t₀
```


    t₀ ↦ t₁   T₀ ↦ T₁
    ─────────────────
    t₀.(T₀) ↦ t₁.(T₁)


```haskell
alphaNormalize (ProjectByType t₀ _T₀) = ProjectByType t₁ _T₁
  where
    t₁ = alphaNormalize t₀

    _T₁ = alphaNormalize _T₀
```


    T₀ ↦ T₁   r₀ ↦ r₁
    ─────────────────
    T₀::r₀ ↦ T₁::r₁


```haskell
alphaNormalize (Completion _T₀ r₀) = Completion _T₁ r₁
  where
    _T₁ = alphaNormalize _T₀

    r₁ = alphaNormalize r₀
```


    T₀ ↦ T₁
    ─────────────────────────
    assert : T₀ ↦ assert : T₁


```haskell
alphaNormalize (Assert _T₀) = Assert _T₁
  where
    _T₁ = alphaNormalize _T₀
```


    e₀ ↦ e₁   v₀ ↦ v₁
    ───────────────────────────────────
    e₀ with ks… = v₀ ↦ e₁ with ks… = v₁


```haskell
alphaNormalize (With e₀ ks v₀) = With e₁ ks v₁
  where
    e₁ = alphaNormalize e₀

    v₁ = alphaNormalize v₀
```


    ─────────
    n.n ↦ n.n


```haskell
alphaNormalize (DoubleLiteral n) = DoubleLiteral n
```


    ─────
    n ↦ n


```haskell
alphaNormalize (NaturalLiteral n) = NaturalLiteral n
```


    ───────
    ±n ↦ ±n


```haskell
alphaNormalize (IntegerLiteral n) = IntegerLiteral n
```


    ───────────────────────
    YYYY-MM-DD ↦ YYYY-MM-DD


```haskell
alphaNormalize (DateLiteral n) = DateLiteral n
```


    ───────────────────
    hh:mm:ss ↦ hh:mm:ss


```haskell
alphaNormalize (TimeLiteral n p) = TimeLiteral n p
```


    ───────────────────
    ±HH:MM ↦ ±HH:MM


```haskell
alphaNormalize (TimeZoneLiteral n) = TimeZoneLiteral n
```


    ─────────
    "s" ↦ "s"


    "ss₀…" ↦ "ss₁…"   t₀ ↦ t₁
    ─────────────────────────────
    "s₀${t₀}ss₀…" ↦ "s₀${t₁}ss₁…"


```haskell
alphaNormalize (TextLiteral (Chunks xys₀ z)) = TextLiteral (Chunks xys₁ z)
  where
    xys₁ = fmap adapt xys₀

    adapt (s, t₀) = (s, t₁)
      where
        t₁ = alphaNormalize t₀
```


    ───────
    {} ↦ {}


    T₀ ↦ T₁   { xs₀… } ↦ { xs₁… }
    ───────────────────────────────────
    { x : T₀, xs₀… } ↦ { x : T₁, xs₁… }


```haskell
alphaNormalize (RecordType ks₀) = RecordType ks₁
  where
    ks₁ = fmap adapt ks₀

    adapt (k, _T₀) = (k, _T₁)
      where
        _T₁ = alphaNormalize _T₀
```


    ─────────
    {=} ↦ {=}


    t₀ ↦ t₁   { xs₀… } ↦ { xs₁… }
    ───────────────────────────────────
    { x = t₀, xs₀… } ↦ { x = t₁, xs₁… }


```haskell
alphaNormalize (RecordLiteral ks₀) = RecordLiteral ks₁
  where
    ks₁ = fmap adapt ks₀

    adapt (k, t₀) = (k, t₁)
      where
        t₁ = alphaNormalize t₀
```


    ───────
    <> ↦ <>


    T₀ ↦ T₁   < xs₀… > ↦ < xs₁… >
    ─────────────────────────────────────
    < x : T₀ | xs₀… > ↦ < x : T₁ | xs₁… >


    < xs₀… > ↦ < xs₁… >
    ───────────────────────────
    < x | xs₀… > ↦ < x | xs₁… >


```haskell
alphaNormalize (UnionType ks₀) = UnionType ks₁
  where
    ks₁ = fmap adapt ks₀

    adapt (k, Just _T₀) = (k, Just _T₁)
      where
        _T₁ = alphaNormalize _T₀
    adapt (k, Nothing) = (k, Nothing)
```


    a₀ ↦ a₁
    ─────────────────
    Some a₀ ↦ Some a₁


```haskell
alphaNormalize (Some a₀) = Some a₁
  where
    a₁ = alphaNormalize a₀
```


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
    Natural/subtract ↦ Natural/subtract


    ───────────────────────────────────
    Integer/toDouble ↦ Integer/toDouble


    ───────────────────────────
    Integer/show ↦ Integer/show


    ───────────────────────────────
    Integer/negate ↦ Integer/negate


    ─────────────────────────────
    Integer/clamp ↦ Integer/clamp


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


    ─────────────────────
    Text/show ↦ Text/show


    ───────────────────────────
    Text/replace ↦ Text/replace
 
 
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
    Date ↦ Date


    ───────────
    Time ↦ Time


    ───────────────────
    TimeZOne ↦ TimeZone


    ───────────
    True ↦ True


    ─────────────
    False ↦ False


```haskell
alphaNormalize (Builtin b) = Builtin b
```


    ───────────
    Type ↦ Type


    ───────────
    Kind ↦ Kind


    ───────────
    Sort ↦ Sort


```haskell
alphaNormalize (Constant c) = Constant c
```
