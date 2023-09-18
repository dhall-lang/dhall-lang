# Substitution

```haskell
-- | Haskell implementation of substitution
module Substitution
    ( -- * Substitute
      substitute
    ) where

import Shift (shift)
import Syntax (Expression(..), Natural, Text, TextLiteral(..))
```

β-reduction requires support for substitution, which has the following form:

    e₀[x@n ≔ a] = e₁

... where:

* `e₀` (an input expression) is the expression to transform
* `x@n` (an input variable) is the variable to substitute with another
  expression
* `a` (an input expression) is the expression to substitute `x@n` with
* `e₁` (the output expression) is transformed expression where all occurrences
  of `x@n` have been replaced with `a`

```haskell
-- | Haskell implementation of the substitute function
substitute
    :: Expression  -- ^ @e₀@, the expression to transform
    -> Text        -- ^ @x@, the variable name to substitute
    -> Natural     -- ^ @n@, the index of the variable to substitute
    -> Expression  -- ^ @a@, the expression to substitute @x\@n@ with
    -> Expression  -- ^ @e₁@
```

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


```haskell
substitute (Variable x' n') x n e | x == x' && n == n' = e

substitute (Variable y m) _x _n _e = Variable y m
```

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


```haskell
substitute (Lambda x _A₀ b₀) x' n e₀ | x == x' = Lambda x _A₁ b₁
  where
    _A₁ = substitute _A₀ x n e₀

    e₁ = shift 1 x 0 e₀

    b₁ = substitute b₀ x (1 + n) e₁

substitute (Lambda y _A₀ b₀) x n e₀ = Lambda y _A₁ b₁
  where
    _A₁ = substitute _A₀ x n e₀

    e₁ = shift 1 y 0 e₀

    b₁ = substitute b₀ x n e₁
```


    A₀[x@n ≔ e₀] = A₁   ↑(1, x, 0, e₀) = e₁   B₀[x@(1 + n) ≔ e₁] = B₁
    ─────────────────────────────────────────────────────────────────
    (∀(x : A₀) → B₀)[x@n ≔ e₀] = ∀(x : A₁) → B₁


    A₀[x@n ≔ e₀] = A₁   ↑(1, y, 0, e₀) = e₁   B₀[x@n ≔ e₁] = B₁
    ───────────────────────────────────────────────────────────  ; x ≠ y
    (∀(y : A₀) → B₀)[x@n ≔ e₀] = ∀(y : A₁) → B₁


```haskell
substitute (Forall x _A₀ _B₀) x' n e₀ | x == x' = Forall x _A₁ _B₁
  where
    _A₁ = substitute _A₀ x n e₀

    e₁ = shift 1 x 0 e₀

    _B₁ = substitute _B₀ x (1 + n) e₁

substitute (Forall y _A₀ _B₀) x n e₀ = Forall y _A₁ _B₁
  where
    _A₁ = substitute _A₀ x n e₀

    e₁ = shift 1 y 0 e₀

    _B₁ = substitute _B₀ x n e₁
```


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


```haskell
substitute (Let x (Just _A₀) a₀ b₀) x' n e₀ | x == x' = Let x (Just _A₁) a₁ b₁
  where
    _A₁ = substitute _A₀ x n e₀

    a₁ = substitute a₀ x n e₀

    e₁ = shift 1 x 0 e₀

    b₁ = substitute b₀ x (1 + n) e₁

substitute (Let y (Just _A₀) a₀ b₀) x n e₀ = Let y (Just _A₁) a₁ b₁
  where
    _A₁ = substitute _A₀ x n e₀

    a₁ = substitute a₀ x n e₀

    e₁ = shift 1 y 0 e₀

    b₁ = substitute b₀ x n e₁

substitute (Let x Nothing a₀ b₀) x' n e₀ | x == x' = Let x Nothing a₁ b₁
  where
    a₁ = substitute a₀ x n e₀

    e₁ = shift 1 x 0 e₀

    b₁ = substitute b₀ x (1 + n) e₁

substitute (Let y Nothing a₀ b₀) x n e₀ = Let y Nothing a₁ b₁
  where
    a₁ = substitute a₀ x n e₀

    e₁ = shift 1 y 0 e₀

    b₁ = substitute b₀ x n e₁
```

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


```haskell
substitute (Import importType importMode maybeDigest) _x _n _e =
    Import importType importMode maybeDigest
```

## Other

No other Dhall expressions bind variables, so the substitution function descends
into sub-expressions in those cases, like this:

    (List x)[x ≔ Bool] = List Bool

The remaining rules are:


    t₀[x@n ≔ e] = t₁   l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ────────────────────────────────────────────────────────
    (if t₀ then l₀ else r₀)[x@n ≔ e] = if t₁ then l₁ else r₁


```haskell
substitute (If t₀ l₀ r₀) x n e = If t₁ l₁ r₁
  where
    t₁ = substitute t₀ x n e

    l₁ = substitute l₀ x n e

    r₁ = substitute r₀ x n e
```


    t₀[x@n ≔ e] = t₁   u₀[x@n ≔ e] = u₁   T₀[x@n ≔ e] = T₁
    ──────────────────────────────────────────────────────
    (merge t₀ u₀ : T₀)[x@n ≔ e] = merge t₁ u₁ : T₁


    t₀[x@n ≔ e] = t₁   u₀[x@n ≔ e] = u₁
    ────────────────────────────────────
    (merge t₀ u₀)[x@n ≔ e] = merge t₁ u₁


```haskell
substitute (Merge t₀ u₀ (Just _T₀)) x n e = Merge t₁ u₁ (Just _T₁)
  where
    t₁ = substitute t₀ x n e

    u₁ = substitute u₀ x n e

    _T₁ = substitute _T₀ x n e

substitute (Merge t₀ u₀ Nothing) x n e = Merge t₁ u₁ Nothing
  where
    t₁ = substitute t₀ x n e

    u₁ = substitute u₀ x n e
```


    t₀[x@n ≔ e] = t₁   T₀[x@n ≔ e] = T₁
    ────────────────────────────────────────
    (toMap t₀ : T₀)[x@n ≔ e] = toMap t₁ : T₁


    t₀[x@n ≔ e] = t₁
    ──────────────────────────────
    (toMap t₀)[x@n ≔ e] = toMap t₁


```haskell
substitute (ToMap t₀ (Just _T₀)) x n e = ToMap t₁ (Just _T₁)
  where
    t₁ = substitute t₀ x n e

    _T₁ = substitute _T₀ x n e

substitute (ToMap t₀ Nothing) x n e = ToMap t₁ Nothing
  where
    t₁ = substitute t₀ x n e
```


    t₀[x@n ≔ e] = t₁
    ──────────────────────────────
    (showConstructor t₀)[x@n ≔ e] = showConstructor t₁


```haskell
substitute (ShowConstructor t₀) x n e = ShowConstructor t₁
  where
    t₁ = substitute t₀ x n e
```

    T₀[x@n ≔ e] = T₁
    ────────────────────────────
    ([] : T₀)[x@n ≔ e] = [] : T₁


    t₀[x@n ≔ e] = t₁   [ ts₀… ][x@n ≔ e] = [ ts₁… ]
    ───────────────────────────────────────────────
    ([ t₀, ts₀… ])[x@n ≔ e] = [ t₁, ts₁… ]


```haskell
substitute (EmptyList _T₀) x n e = EmptyList _T₁
  where
    _T₁ = substitute _T₀ x n e

substitute (NonEmptyList ts₀) x n e = NonEmptyList ts₁
  where
    ts₁ = fmap adapt ts₀

    adapt t₀ = t₁
      where
        t₁ = substitute t₀ x n e
```


    t₀[x@n ≔ e] = t₁   T₀[x@n ≔ e] = T₁
    ───────────────────────────────────
    (t₀ : T₀)[x@n ≔ e] = t₁ : T₁


```haskell
substitute (Annotation t₀ _T₀)  x n e = Annotation t₁ _T₁
  where
    t₁ = substitute t₀ x n e

    _T₁ = substitute _T₀ x n e
```


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


    l₀[x@n ≔ e] = l₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (l₀ === r₀)[x@n ≔ e] = l₁ === r₁


```haskell
substitute (Operator l₀ op r₀) x n e = Operator l₁ op r₁
  where
    l₁ = substitute l₀ x n e

    r₁ = substitute r₀ x n e
```


    f₀[x@n ≔ e] = f₁   a₀[x@n ≔ e] = a₁
    ───────────────────────────────────
    (f₀ a₀)[x@n ≔ e] = f₁ a₁


```haskell
substitute (Application f₀ a₀) x n e = Application f₁ a₁
  where
    f₁ = substitute f₀ x n e

    a₁ = substitute a₀ x n e
```


    t₀[x@n ≔ e] = t₁
    ────────────────────────
    (t₀.y)[x@n ≔ e] = t₁.y


```haskell
substitute (Field t₀ y) x n e = Field t₁ y
  where
    t₁ = substitute t₀ x n e
```


    t₀[x@n ≔ e] = t₁
    ──────────────────────────────────
    (t₀.{ xs… })[x@n ≔ e] = t₁.{ xs… }


```haskell
substitute (ProjectByLabels t₀ xs) x n e = ProjectByLabels t₁ xs
  where
    t₁ = substitute t₀ x n e
```


    t₀[x@n ≔ e] = t₁   T₀[x@n ≔ e] = T₁
    ───────────────────────────────────
    (t₀.(T₀))[x@n ≔ e] = t₁.(T₁)


```haskell
substitute (ProjectByType t₀ _T₀) x n e = ProjectByType t₁ _T₁
  where
    t₁ = substitute t₀ x n e

    _T₁ = substitute _T₀ x n e
```


    T₀[x@n ≔ e] = T₁   r₀[x@n ≔ e] = r₁
    ───────────────────────────────────
    (T₀::r₀)[x@n ≔ e] = T₁::r₁


```haskell
substitute (Completion _T₀ r₀) x n e = Completion _T₁ r₁
  where
    _T₁ = substitute _T₀ x n e

    r₁ = substitute r₀ x n e
```


    T₀[x@n ≔ e] = T₁
    ────────────────────────────────────
    (assert : T₀)[x@n ≔ e] = assert : T₁


```haskell
substitute (Assert _T₀) x n e = Assert _T₁
  where
    _T₁ = substitute _T₀ x n e
```


    e₀[x@n ≔ e] = e₁   v₀[x@n ≔ e] = v₁
    ──────────────────────────────────────────────────
    (e₀ with k.ks… = v₀)[x@n ≔ e] = e₁ with k.ks… = v₁


```haskell
substitute (With e₀ ks v₀) x n e = With e₁ ks v₁
  where
    e₁ = substitute e₀ x n e

    v₁ = substitute v₀ x n e
```


    ──────────────────
    n.n[x@n ≔ e] = n.n


```haskell
substitute (DoubleLiteral n) _x _n _e = DoubleLiteral n
```


    ──────────────
    n[x@n ≔ e] = n


```haskell
substitute (NaturalLiteral n) _x _n _e = NaturalLiteral n
```


    ────────────────
    ±n[x@n ≔ e] = ±n


```haskell
substitute (IntegerLiteral n) _x _n _e = IntegerLiteral n
```


    ────────────────────────────────
    YYYY-MM-DD[x@n ≔ e] = YYYY-MM-DD


```haskell
substitute (DateLiteral n) _x _n _e = DateLiteral n
```


    ────────────────────────────
    hh:mm:ss[x@n ≔ e] = hh:mm:ss


```haskell
substitute (TimeLiteral n p) _x _n _e = TimeLiteral n p
```


    ────────────────────────
    ±HH:MM[x@n ≔ e] = ±HH:MM


```haskell
substitute (TimeZoneLiteral n) _x _n _e = TimeZoneLiteral n
```


    ──────────────────
    "s"[x@n ≔ e] = "s"


    t₀[x@n ≔ e] = t₁   "ss₀…"[x@n ≔ e] = "ss₁…"
    ───────────────────────────────────────────
    "s₀${t₀}ss₀…"[x@n ≔ e] = "s₀${t₁}ss₁…"


```haskell
substitute (TextLiteral (Chunks xys₀ z)) x n e = TextLiteral (Chunks xys₁ z)
  where
    xys₁ = fmap adapt xys₀

    adapt (s, t₀) = (s, t₁)
      where
        t₁ = substitute t₀ x n e
```


    ────────────────────────────────────────────────────
    0x"0123456789abcdef"[x@n ≔ e] = 0x"0123456789abcdef"


```haskell
substitute (BytesLiteral xs) _x _n _e = BytesLiteral xs
```


    ────────────────
    {}[x@n ≔ e] = {}


    T₀[x@n ≔ e] = T₁   { ks₀… }[x@n ≔ e] = { ks₁… }
    ───────────────────────────────────────────────
    { k : T₀, ks₀… }[x@n ≔ e] = { k : T₁, ks₁… }


```haskell
substitute (RecordType ks₀) x n e = RecordType ks₁
  where
    ks₁ = fmap adapt ks₀

    adapt (k, _T₀) = (k, _T₁)
      where
        _T₁ = substitute _T₀ x n e
```


    ──────────────────
    {=}[x@n ≔ e] = {=}


    t₀[x@n ≔ e] = t₁   { ks₀… }[x@n ≔ e] = { ks₁… }
    ───────────────────────────────────────────────
    { k = t₀, ks₀… }[x@n ≔ e] = { k = t₁, ks₁… }


```haskell
substitute (RecordLiteral ks₀) x n e = RecordLiteral ks₁
  where
    ks₁ = fmap adapt ks₀

    adapt (k, t₀) = (k, t₁)
      where
        t₁ = substitute t₀ x n e
```


    ────────────────
    <>[x@n ≔ e] = <>


    T₀[x@n ≔ e] = T₁   < ks₀… >[x@n ≔ e] = < ks₁… >
    ────────────────────────────────────────────────
    < k : T₀ | ks₀… >[x@n ≔ e] = < k : T₁ | ks₁… >


    < ks₀… >[x@n ≔ e] = < ks₁… >
    ──────────────────────────────────────
    < k | ks₀… >[x@n ≔ e] = < k | ks₁… >


```haskell
substitute (UnionType ks₀) x n e = UnionType ks₁
  where
    ks₁ = fmap adapt ks₀

    adapt (k, Just _T₀) = (k, Just _T₁)
      where
        _T₁ = substitute _T₀ x n e
    adapt (k, Nothing) = (k, Nothing)
```


    a₀[x@n ≔ e] = a₁
    ────────────────────────────
    (Some a₀)[x@n ≔ e] = Some a₁


```haskell
substitute (Some a₀) x n e = Some a₁
  where
    a₁ = substitute a₀ x n e
```


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


    ──────────────────────────────────────────
    Natural/showHex[x@n ≔ e] = Natural/showHex


    ────────────────────────────────────────────
    Natural/subtract[x@n ≔ e] = Natural/subtract


    ────────────────────────────────────────────
    Integer/toDouble[x@n ≔ e] = Integer/toDouble


    ────────────────────────────────────
    Integer/show[x@n ≔ e] = Integer/show


    ────────────────────────────────────────
    Integer/negate[x@n ≔ e] = Integer/negate


    ──────────────────────────────────────
    Integer/clamp[x@n ≔ e] = Integer/clamp


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


    ──────────────────────────────
    Text/show[x@n ≔ e] = Text/show


    ────────────────────────────────────
    Text/replace[x@n ≔ e] = Text/replace


    ──────────────────────────────
    Date/show[x@n ≔ e] = Date/show


    ──────────────────────────────
    Time/show[x@n ≔ e] = Time/show


    ──────────────────────────────────────
    TimeZone/show[x@n ≔ e] = TimeZone/show


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
    Date[x@n ≔ e] = Date


    ────────────────────
    Time[x@n ≔ e] = Time


    ────────────────────────────
    TimeZone[x@n ≔ e] = TimeZone


    ────────────────────
    True[x@n ≔ e] = True


    ──────────────────────
    False[x@n ≔ e] = False


```haskell
substitute (Builtin b) _x _n _e = Builtin b
```


    ────────────────────
    Type[x@n ≔ e] = Type


    ────────────────────
    Kind[x@n ≔ e] = Kind


    ────────────────────
    Sort[x@n ≔ e] = Sort


```haskell
substitute (Constant c) _x _n _e = Constant c
```
