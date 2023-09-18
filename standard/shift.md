# Shift

```haskell
-- | Haskell implementation of shifting
module Shift
    ( -- * Shift
      shift
    ) where

import Syntax (Expression(..), Natural, Text, TextLiteral(..))
```

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

```haskell
-- | Haskell implementation of the shift function
shift
    :: Integer    -- ^ @d@, the amount to add to the variable indices
    -> Text       -- ^ @x@, the name of the free variable to shift
    -> Natural    -- ^ @m@, the minimum index to shift
    -> Expression -- ^ @e₀@, the expression to shift
    -> Expression -- ^ @e₁@, the shifted expression
```

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


```haskell
shift d x m (Variable x' n) | x == x' && m <= n =
    Variable x' (n + fromInteger d)
```

Don't shift the index if the index falls short of the lower bound:


    ─────────────────────  ; m > n
    ↑(d, x, m, x@n) = x@n


```haskell
shift _d x _m (Variable x' n) | x == x' = Variable x' n
```

Also, don't shift the index if the variable name does not match:


    ─────────────────────  ; x ≠ y
    ↑(d, x, m, y@n) = y@n


```haskell
shift _d _x _m (Variable y n) = Variable y n
```

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


```haskell
shift d x m (Lambda x' _A₀ b₀) | x == x' = Lambda x' _A₁ b₁
  where
    _A₁ = shift d x m _A₀

    b₁ = shift d x (m + 1) b₀
```

Note that the bound variable, `x`, is not in scope for its own type, `A₀`, so
do not increase the lower bound, `m`, when shifting the bound variable's type.

Descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, b₀) = b₁
    ───────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, λ(y : A₀) → b₀) = λ(y : A₁) → b₁


```haskell
shift d x m (Lambda y _A₀ b₀) = Lambda y _A₁ b₁
  where
    _A₁ = shift d x m _A₀

    b₁ = shift d x m b₀
```

Function types also introduce bound variables, so increase the minimum bound
when descending past such a bound variable:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m + 1, B₀) = B₁
    ─────────────────────────────────────────────
    ↑(d, x, m, ∀(x : A₀) → B₀) = ∀(x : A₁) → B₁


```haskell
shift d x m (Forall x' _A₀ _B₀) | x == x' = Forall x' _A₁ _B₁
  where
    _A₁ = shift d x m _A₀

    _B₁ = shift d x (m + 1) _B₀
```

Again, the bound variable, `x`, is not in scope for its own type, `A₀`, so do
not increase the lower bound, `m`, when shifting the bound variable's type.

Descend as normal if the bound variable name does not match:


    ↑(d, x, m, A₀) = A₁   ↑(d, x, m, B₀) = B₁
    ───────────────────────────────────────────  ; x ≠ y
    ↑(d, x, m, ∀(y : A₀) → B₀) = ∀(y : A₁) → B₁


```haskell
shift d x m (Forall y _A₀ _B₀) = Forall y _A₁ _B₁
  where
    _A₁ = shift d x m _A₀

    _B₁ = shift d x m _B₀
```

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


```haskell
shift d x m (Let x' (Just _A₀) a₀ b₀) | x == x' = Let x' (Just _A₁) a₁ b₁
  where
    _A₁ = shift d x m _A₀

    a₁ = shift d x m a₀

    b₁ = shift d x (m + 1) b₀

shift d x m (Let x' Nothing a₀ b₀) | x == x' = Let x' Nothing a₁ b₁
  where
    a₁ = shift d x m a₀

    b₁ = shift d x (m + 1) b₀
```

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


```haskell
shift d x m (Let y (Just _A₀) a₀ b₀) = Let y (Just _A₁) a₁ b₁
  where
    _A₁ = shift d x m _A₀

    a₁ = shift d x m a₀

    b₁ = shift d x m b₀

shift d x m (Let y Nothing a₀ b₀) = Let y Nothing a₁ b₁
  where
    a₁ = shift d x m a₀

    b₁ = shift d x m b₀
```

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


```haskell
shift _d _x _m (Import importType importMode maybeDigest) =
    Import importType importMode maybeDigest
```

## Other

No other Dhall expressions bind variables, so the shift function descends into
sub-expressions in those cases, like this:

    ↑(1, x, 0, List x) = List x@1

The remaining rules are:


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ───────────────────────────────────────────────────────────────
    ↑(d, x, m, if t₀ then l₀ else r₀) = if t₁ then l₁ else r₁


```haskell
shift d x m (If t₀ l₀ r₀) = If t₁ l₁ r₁
  where
    t₁ = shift d x m t₀

    l₁ = shift d x m l₀

    r₁ = shift d x m r₀
```


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, u₀) = u₁   ↑(d, x, m, T₀) = T₁
    ───────────────────────────────────────────────────────────────
    ↑(d, x, m, merge t₀ u₀ : T₀) = merge t₁ u₁ : T₁


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, u₀) = u₁
    ─────────────────────────────────────────
    ↑(d, x, m, merge t₀ u₀) = merge t₁ u₁


```haskell
shift d x m (Merge t₀ u₀ (Just _T₀)) = Merge t₁ u₁ (Just _T₁)
  where
    t₁ = shift d x m t₀

    u₁ = shift d x m u₀

    _T₁ = shift d x m _T₀

shift d x m (Merge t₀ u₀ Nothing) = Merge t₁ u₁ Nothing
  where
    t₁ = shift d x m t₀

    u₁ = shift d x m u₀
```


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, T₀) = T₁
    ─────────────────────────────────────────
    ↑(d, x, m, toMap t₀ : T₀) = toMap t₁ : T₁


    ↑(d, x, m, t₀) = t₁
    ───────────────────────────────
    ↑(d, x, m, toMap t₀) = toMap t₁


```haskell
shift d x m (ToMap t₀ (Just _T₀)) = ToMap t₁ (Just _T₁)
  where
    t₁ = shift d x m t₀

    _T₁ = shift d x m _T₀

shift d x m (ToMap t₀ Nothing) = ToMap t₁ Nothing
  where
    t₁ = shift d x m t₀
```


    ↑(d, x, m, t₀) = t₁
    ───────────────────────────────
    ↑(d, x, m, showConstructor t₀) = showConstructor t₁


```haskell
shift d x m (ShowConstructor t₀) = ShowConstructor t₁
  where
    t₁ = shift d x m t₀
```

    ↑(d, x, m, T₀) = T₁
    ─────────────────────────────
    ↑(d, x, m, [] : T₀) = [] : T₁


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, [ ts₀… ]) = [ ts₁… ]
    ─────────────────────────────────────────────────────
    ↑(d, x, m, [ t₀, ts₀… ]) = [ t₁, ts₁… ]


```haskell
shift d x m (EmptyList _T₀) = EmptyList _T₁
  where
    _T₁ = shift d x m _T₀

shift d x m (NonEmptyList ts₀) = NonEmptyList ts₁
  where
    ts₁ = fmap (shift d x m) ts₀
```


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, T₀) = T₁
    ─────────────────────────────────────────
    ↑(d, x, m, t₀ : T₀) = t₁ : T₁


```haskell
shift d x m (Annotation t₀ _T₀) = Annotation t₁ _T₁
  where
    t₁ = shift d x m t₀

    _T₁ = shift d x m _T₀
```


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


    ↑(d, x, m, l₀) = l₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, l₀ === r₀) = l₁ === r₁


```haskell
shift d x m (Operator l₀ op r₀) = Operator l₁ op r₁
  where
    l₁ = shift d x m l₀

    r₁ = shift d x m r₀
```


    ↑(d, x, m, f₀) = f₁   ↑(d, x, m, a₀) = a₁
    ─────────────────────────────────────────
    ↑(d, x, m, f₀ a₀) = f₁ a₁


```haskell
shift d x m (Application f₀ a₀) = Application f₁ a₁
  where
    f₁ = shift d x m f₀

    a₁ = shift d x m a₀
```


    ↑(d, x, m, t₀) = t₁
    ───────────────────────
    ↑(d, x, m, t₀.y) = t₁.y


```haskell
shift d x m (Field t₀ y) = Field t₁ y
  where
    t₁ = shift d x m t₀
```


    ↑(d, x, m, t₀) = t₁
    ───────────────────────────────────
    ↑(d, x, m, t₀.{ xs… }) = t₁.{ xs… }


```haskell
shift d x m (ProjectByLabels t₀ xs) = ProjectByLabels t₁ xs
  where
    t₁ = shift d x m t₀
```


    ↑(d, x, m, t₀) = t₁ ↑(d, x, m, T₀) = T₁
    ───────────────────────────────────────
    ↑(d, x, m, t₀.(T₀) = t₁.(T₁)


```haskell
shift d x m (ProjectByType t₀ _T₀) = ProjectByType t₁ _T₁
  where
    t₁ = shift d x m t₀

    _T₁ = shift d x m _T₀
```


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, r₀) = r₁
    ─────────────────────────────────────────
    ↑(d, x, m, T₀::r₀) = T₁::r₁


```haskell
shift d x m (Completion _T₀ r₀) = Completion _T₁ r₁
  where
    _T₁ = shift d x m _T₀

    r₁ = shift d x m r₀
```


    ↑(d, x, m, T₀) = T₁
    ─────────────────────────────────────
    ↑(d, x, m, assert : T₀) = assert : T₁


```haskell
shift d x m (Assert _T₀) = Assert _T₁
  where
    _T₁ = shift d x m _T₀
```

    ↑(d, x, m, e₀) = e₁   ↑(d, x, m, v₀) = v₁
    ───────────────────────────────────────────────────
    ↑(d, x, m, e₀ with k.ks… = v₀) = e₁ with k.ks… = v₁


```haskell
shift d x m (With e₀ ks v₀) = With e₁ ks v₁
  where
    e₁ = shift d x m e₀

    v₁ = shift d x m v₀
```


    ─────────────────────
    ↑(d, x, m, n.n) = n.n


```haskell
shift _d _x _m (DoubleLiteral n) = DoubleLiteral n
```


    ─────────────────
    ↑(d, x, m, n) = n


```haskell
shift _d _x _m (NaturalLiteral n) = NaturalLiteral n
```


    ───────────────────
    ↑(d, x, m, ±n) = ±n


```haskell
shift _d _x _m (IntegerLiteral n) = IntegerLiteral n
```


    ───────────────────────────────────
    ↑(d, x, m, YYYY-MM-DD) = YYYY-MM-DD


```haskell
shift _d _x _m (DateLiteral d) = DateLiteral d
```


    ───────────────────────────────
    ↑(d, x, m, hh:mm:ss) = hh:mm:ss


```haskell
shift _d _x _m (TimeLiteral t p) = TimeLiteral t p
```


    ───────────────────────────
    ↑(d, x, m, ±HH:MM) = ±HH:MM


```haskell
shift _d _x _m (TimeZoneLiteral z) = TimeZoneLiteral z
```


    ─────────────────────
    ↑(d, x, m, "s") = "s"


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, "ss₀…") = "ss₁…"
    ─────────────────────────────────────────────────
    ↑(d, x, m, "s₀${t₀}ss₀…") = "s₀${t₁}ss₁…"


```haskell
shift d x m (TextLiteral (Chunks xys₀ z)) = TextLiteral (Chunks xys₁ z)
  where
    xys₁ = fmap adapt xys₀

    adapt (s, t₀) = (s, t₁)
      where
        t₁ = shift d x m t₀
```


    ───────────────────────────────────────────────────────
    ↑(d, x, m, 0x"0123456789abcdef") = 0x"0123456789abcdef"


```haskell
shift _d _x _m (BytesLiteral xs) = BytesLiteral xs
```

    ───────────────────
    ↑(d, x, m, {}) = {}


    ↑(d, x, m, T₀) = T₁   ↑(d, k, m, { ks₀… }) = { ks₁… }
    ─────────────────────────────────────────────────────
    ↑(d, x, m, { k : T₀, xs₀… }) = { k : T₁, ks₁… }


```haskell
shift d x m (RecordType ks₀) = RecordType ks₁
  where
    ks₁ = map adapt ks₀

    adapt (k, _T₀) = (k, _T₁)
      where
        _T₁ = shift d x m _T₀
```


    ─────────────────────
    ↑(d, x, m, {=}) = {=}


    ↑(d, x, m, t₀) = t₁   ↑(d, x, m, { ks₀… }) = { ks₁… }
    ─────────────────────────────────────────────────────
    ↑(d, x, m, { k = t₀, ks₀… }) = { k = t₁, ks₁… }


```haskell
shift d x m (RecordLiteral ks₀) = RecordLiteral ks₁
  where
    ks₁ = map adapt ks₀

    adapt (k, t₀) = (k, t₁)
      where
        t₁ = shift d x m t₀
```


    ───────────────────
    ↑(d, x, m, <>) = <>


    ↑(d, x, m, T₀) = T₁   ↑(d, x, m, < ks₀… >) = < ks₁… >
    ─────────────────────────────────────────────────────
    ↑(d, x, m, < k : T₀ | ks₀… >) = < k : T₁ | ks₁… >


    ↑(d, x, m, < ks₀… >) = < ks₁… >
    ─────────────────────────────────────────
    ↑(d, x, m, < k | ks₀… >) = < k | ks₁… >


```haskell
shift d x m (UnionType ks₀) = UnionType ks₁
  where
    ks₁ = map adapt ks₀

    adapt (k, Just _T₀) = (k, Just _T₁)
      where
        _T₁ = shift d x m _T₀
    adapt (k, Nothing) = (k, Nothing)
```


    ↑(d, x, m, a₀) = a₁
    ─────────────────────────────
    ↑(d, x, m, Some a₀) = Some a₁


```haskell
shift d x m (Some a₀) = Some a₁
  where
    a₁ = shift d x m a₀
```


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


    ───────────────────────────────────────────────
    ↑(d, x, m, Natural/subtract) = Natural/subtract


    ───────────────────────────────────────
    ↑(d, x, m, Natural/show) = Natural/show


    ─────────────────────────────────────────────
    ↑(d, x, m, Natural/showHex) = Natural/showHex


    ───────────────────────────────────────────────
    ↑(d, x, m, Integer/toDouble) = Integer/toDouble


    ───────────────────────────────────────
    ↑(d, x, m, Integer/show) = Integer/show


    ───────────────────────────────────────────
    ↑(d, x, m, Integer/negate) = Integer/negate


    ─────────────────────────────────────────
    ↑(d, x, m, Integer/clamp) = Integer/clamp


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


    ─────────────────────────────────
    ↑(d, x, m, Text/show) = Text/show


    ───────────────────────────────────────
    ↑(d, x, m, Text/replace) = Text/replace


    ─────────────────────────────────
    ↑(d, x, m, Date/show) = Date/show


    ─────────────────────────────────
    ↑(d, x, m, Time/show) = Time/show


    ─────────────────────────────────────────
    ↑(d, x, m, TimeZone/show) = TimeZone/show


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
    ↑(d, x, m, Date) = Date


    ───────────────────────
    ↑(d, x, m, Time) = Date


    ───────────────────────────────
    ↑(d, x, m, TimeZone) = TimeZone


    ───────────────────────
    ↑(d, x, m, True) = True


    ─────────────────────────
    ↑(d, x, m, False) = False


```haskell
shift _d _x _m (Builtin b) = Builtin b
```


    ───────────────────────
    ↑(d, x, m, Type) = Type


    ───────────────────────
    ↑(d, x, m, Kind) = Kind


    ───────────────────────
    ↑(d, x, m, Sort) = Sort


```haskell
shift _d _x _m (Constant c) = Constant c
```
