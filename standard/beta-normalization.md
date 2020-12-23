# β-normalization

```haskell
{-# LANGUAGE OverloadedStrings #-}

module BetaNormalization where

import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty(..))
import {-# SOURCE #-} Equivalence (equivalent)
import Prelude hiding (Bool(..))
import Shift (shift)
import Syntax

import qualified Data.Text          as Text
import qualified Data.List          as List
import qualified Data.Ord           as Ord
import qualified Data.List.NonEmpty as NonEmpty
```

β-normalization is a function of the following form:

    t₀ ⇥ t₁

... where:

* `t₀` (the input) is the expression to normalize
* `t₁` (the output) is the normalized expression

```haskell
betaNormalize
    :: Expression  -- ^ @t₀@, the expression to normalize
    -> Expression  -- ^ @t₁@, the normalized expression
```

β-normalization evaluates all β-reducible expressions:

    (λ(x : Bool) → x == False) True ⇥ False

β-normalization evaluates all built-in functions if they are fully saturated
(i.e.  no missing arguments):

    List/length Natural [1, 2, 3] ⇥ 3

β-normalization does not simplify partially applied built-in functions:

    List/length Integer ⇥ List/length Integer

β-normalization works under λ, meaning that the body of an unapplied
λ-expression can be normalized:

    λ(x : Integer) → List/length Integer [x, x, x] ⇥ λ(x : Integer) → 3

Dhall is a total language that is strongly normalizing, so evaluation order has
no effect on the language semantics and a conforming implementation can select
any evaluation strategy.

Also, note that the semantics specifies several built-in types, functions and
operators that conforming implementations must support.  Implementations are
encouraged to implement the following functions and operators in more efficient
ways than the following reduction rules so long as the result of normalization
is the same.

## Table of contents

* [Constants](#constants)
* [Variables](#variables)
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
* [Assertions](#assertions)
* [Imports](#imports)

## Constants

Type-checking constants are in normal form:


    ───────────
    Type ⇥ Type


    ───────────
    Kind ⇥ Kind


    ───────────
    Sort ⇥ Sort


```haskell
betaNormalize (Constant c) = Constant c
```

## Variables

Variables are in normal form:


    ─────────
    x@n ⇥ x@n


```haskell
betaNormalize (Variable x n) = Variable x n
```

## `Bool`


The `Bool` type is in normal form:


    ───────────
    Bool ⇥ Bool


```haskell
betaNormalize (Builtin Bool) = Builtin Bool
```

The `Bool` constructors are in normal form:


    ───────────
    True ⇥ True


    ─────────────
    False ⇥ False


```haskell
betaNormalize (Builtin True) = Builtin True

betaNormalize (Builtin False) = Builtin False
```

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


Simplify `if` expressions where both alternatives are equivalent:


    l₀ ≡ r   l₀ ⇥ l₁
    ────────────────────────
    if t then l₀ else r ⇥ l₁


Otherwise, normalize the predicate and both branches of the `if` expression:


    t₀ ⇥ t₁   l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────────────────────────────────  ; If no other rule matches
    if t₀ then l₀ else r₀ ⇥ if t₁ then l₁ else r₁


```haskell
betaNormalize (If t₀ l₀ r₀)
    | Builtin True  <- t₁ = l₁
    | Builtin False <- t₁ = r₁
    | equivalent l₀ r₀    = l₁
    | otherwise           = If t₁ l₁ r₁
  where
    t₁ = betaNormalize t₀

    l₁ = betaNormalize l₀

    r₁ = betaNormalize r₀
```

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


    l₀ ≡ r   l₀ ⇥ l₁
    ────────────────
    l₀ || r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ || r₀ ⇥ l₁ || r₁


```haskell
betaNormalize (Operator l₀ Or r₀)
    | Builtin False <- l₁ = r₁
    | Builtin False <- r₁ = l₁
    | Builtin True  <- l₁ = Builtin True
    | Builtin True  <- r₁ = Builtin True
    | equivalent l₀ r₀    = l₁
    | otherwise           = Operator l₁ Or r₁
  where
    l₁ = betaNormalize l₀
    r₁ = betaNormalize r₀
```

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


    l₀ ≡ r   l₀ ⇥ l₁
    ────────────────
    l₀ && r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ && r₀ ⇥ l₁ && r₁


```haskell
betaNormalize (Operator l₀ And r₀)
    | Builtin True  <- l₁ = r₁
    | Builtin True  <- r₁ = l₁
    | Builtin False <- l₁ = Builtin False
    | Builtin False <- r₁ = Builtin False
    | equivalent l₀ r₀    = l₁
    | otherwise           = Operator l₁ And r₁
  where
    l₁ = betaNormalize l₀
    r₁ = betaNormalize r₀
```

Simplify the logical "equal" operator if one argument normalizes to a `True`
literal:


    l ⇥ True   r₀ ⇥ r₁
    ──────────────────
    l == r₀ ⇥ r₁


    r ⇥ True   l₀ ⇥ l₁
    ──────────────────
    l₀ == r ⇥ l₁


... or if both arguments are equivalent:


    l ≡ r
    ─────────────
    l == r ⇥ True


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ == r₀ ⇥ l₁ == r₁


```haskell
betaNormalize (Operator l₀ Equal r₀)
    | Builtin True <- l₁ = r₁
    | Builtin True <- r₁ = l₁
    | equivalent l₀ r₀   = Builtin True
    | otherwise          = Operator l₁ Equal r₁
  where
    l₁ = betaNormalize l₀
    r₁ = betaNormalize r₀
```

Simplify the logical "not equal" operator if one argument normalizes to a
`False` literal:


    l ⇥ False   r₀ ⇥ r₁
    ───────────────────
    l != r₀ ⇥ r₁


    r ⇥ False   l₀ ⇥ l₁
    ───────────────────
    l₀ != r ⇥ l₁


... or if both arguments are equivalent:


    l ≡ r
    ──────────────
    l != r ⇥ False


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ != r₀ ⇥ l₁ != r₁


```haskell
betaNormalize (Operator l₀ NotEqual r₀)
    | Builtin False <- l₁ = r₁
    | Builtin False <- r₁ = l₁
    | equivalent l₀ r₀    = Builtin False
    | otherwise           = Operator l₁ NotEqual r₁
  where
    l₁ = betaNormalize l₀
    r₁ = betaNormalize r₀
```

## `Natural`

The `Natural` number type is in normal form:


    ─────────────────
    Natural ⇥ Natural


```haskell
betaNormalize (Builtin Natural) = Builtin Natural
```

`Natural` number literals are in normal form:


    ─────
    n ⇥ n


```haskell
betaNormalize (NaturalLiteral n) = NaturalLiteral n
```

`Natural/build` is the canonical introduction function for `Natural` numbers:


    f ⇥ Natural/build   g Natural (λ(x : Natural) → x + 1) 0 ⇥ b
    ────────────────────────────────────────────────────────────
    f g ⇥ b


```haskell
betaNormalize (Application f g)
    | Builtin NaturalBuild <- betaNormalize f = b
  where
    b = betaNormalize
            (Application
                (Application g
                    (Lambda "x" (Builtin Natural)
                        (Operator (Variable "x" 0) Plus (NaturalLiteral 1))
                    )
                )
                (NaturalLiteral 0)
            )
```

`Natural/fold` function is the canonical elimination function for `Natural`
numbers:


    f ⇥ Natural/fold 0 B g   b ⇥ t₁
    ───────────────────────────────
    f b ⇥ t₁


    f ⇥ Natural/fold (1 + n) B g
    g (Natural/fold n B g b) ⇥ t₁
    ─────────────────────────────  ; "1 + n" means "a `Natural` literal greater
    f b ⇥ t₁                       ; than `0`"


```haskell
betaNormalize (Application f b)
    | Application
        (Application
            (Application
                (Builtin NaturalFold)
                (NaturalLiteral m)
            )
            _B
        )
        g <- betaNormalize f =

        let t₁  | m == 0 =
                    betaNormalize b
                | otherwise =
                    betaNormalize
                        (Application g
                            (Application
                                (Application
                                    (Application
                                        (Application (Builtin NaturalFold)
                                            (NaturalLiteral (m - 1))
                                        )
                                        _B
                                    )
                                    g
                                )
                                b
                            )
                        )
        in  t₁
```

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


```haskell
betaNormalize (Operator l₀ Plus r₀)
    | NaturalLiteral m <- l₁
    , NaturalLiteral n <- r₁ = NaturalLiteral (m + n)
    | NaturalLiteral 0 <- l₁ = r₁
    | NaturalLiteral 0 <- r₁ = l₁
    | otherwise              = Operator l₁ Plus r₁
  where
    l₁ = betaNormalize l₀
    r₁ = betaNormalize r₀
```

Use machine multiplication to simplify the "times" operator if both arguments
normalize to a `Natural` literal:


    l ⇥ m   r ⇥ n
    ─────────────  ; "m * n" means "use machine multiplication"
    l * r ⇥ m * n


Also, simplify the "times" operator if either argument normalizes to either a
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


```haskell
betaNormalize (Operator l₀ Times r₀)
    | NaturalLiteral m <- l₁
    , NaturalLiteral n <- r₁ = NaturalLiteral (m * n)
    | NaturalLiteral 0 <- l₁ = NaturalLiteral 0
    | NaturalLiteral 0 <- r₁ = NaturalLiteral 0
    | NaturalLiteral 1 <- l₁ = r₁
    | NaturalLiteral 1 <- r₁ = l₁
    | otherwise              = Operator l₁ Times r₁
  where
    l₁ = betaNormalize l₀
    r₁ = betaNormalize r₀
```

`Natural/isZero` detects whether or not a `Natural` number is `0`:


    f ⇥ Natural/isZero   a ⇥ 0
    ───────────────────────────
    f a ⇥ True


    f ⇥ Natural/isZero   a ⇥ 1 + n
    ──────────────────────────────  ; "1 + n" means "a `Natural` literal greater
    f a ⇥ False                     ; than `0`"


```haskell
betaNormalize (Application f a)
    | Builtin NaturalIsZero <- betaNormalize f
    , NaturalLiteral 0      <- betaNormalize a = Builtin True
    | Builtin NaturalIsZero <- betaNormalize f
    , NaturalLiteral _      <- betaNormalize a = Builtin False
```

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


```haskell
betaNormalize (Application f a)
    | Builtin NaturalEven <- betaNormalize f
    , NaturalLiteral m    <- betaNormalize a =
        Builtin (if even m then True else False)
```

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


```haskell
betaNormalize (Application f a)
    | Builtin NaturalOdd <- betaNormalize f
    , NaturalLiteral m   <- betaNormalize a =
        Builtin (if odd m then True else False)
```

`Natural/toInteger` transforms a `Natural` number into the corresponding
`Integer`:


    f ⇥ Natural/toInteger   a ⇥ n
    ─────────────────────────────
    f a ⇥ +n


```haskell
betaNormalize (Application f a)
    | Builtin NaturalToInteger <- betaNormalize f
    , NaturalLiteral n         <- betaNormalize a =
        IntegerLiteral (fromIntegral n)
```

`Natural/show` transforms a `Natural` number into a `Text` literal representing
valid Dhall code for representing that `Natural` number:


    f ⇥ Natural/show   a ⇥ n
    ────────────────────────
    f a ⇥ "n"


```haskell
betaNormalize (Application f a)
    | Builtin NaturalShow <- betaNormalize f
    , NaturalLiteral n    <- betaNormalize a =
        TextLiteral (Chunks [] (Text.pack (show n)))
```

`Natural/subtract` performs truncating subtraction, as in
[saturation arithmetic](https://en.wikipedia.org/wiki/Saturation_arithmetic):


    f ⇥ Natural/subtract a   a ⇥ m   b ⇥ n
    ──────────────────────────────────────  ; if m <= n, where "m <= n" is
    f b ⇥ n - m                             ; machine greater-than-or-equal-to
                                            ; comparison, and "n - m" is machine
                                            ; subtraction


    f ⇥ Natural/subtract a   a ⇥ m   b ⇥ n
    ──────────────────────────────────────  ; if n < m
    f b ⇥ 0


Also, simplify the `Natural/subtract` function if either argument normalizes to
a `0` literal:


    f ⇥ Natural/subtract a   a ⇥ 0   b₀ ⇥ b₁
    ────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ Natural/subtract a   b ⇥ 0
    ──────────────────────────────
    f b ⇥ 0


If the arguments are equivalent:


    f ⇥ Natural/subtract a   a ≡ b
    ──────────────────────────────
    f b ⇥ 0


Otherwise, normalize each argument:


    f ⇥ Natural/subtract a₀   a₀ ⇥ a₁   b₀ ⇥ b₁
    ────────────────────────────────────────────  ; If no other rule matches
    f b₀ ⇥ Natural/subtract a₁ b₁


```haskell
betaNormalize (Application f b)
    | Application (Builtin NaturalSubtract) a <- betaNormalize f
    , NaturalLiteral m                        <- betaNormalize a
    , NaturalLiteral n                        <- betaNormalize b
    , m <= n =
        NaturalLiteral (n - m)
    | Application (Builtin NaturalSubtract) a <- betaNormalize f
    , NaturalLiteral m                        <- betaNormalize a
    , NaturalLiteral n                        <- betaNormalize b
    , n < m =
        NaturalLiteral 0
    | Application (Builtin NaturalSubtract) a <- betaNormalize f
    , NaturalLiteral 0                        <- betaNormalize a =
        betaNormalize b
    | Application (Builtin NaturalSubtract) _a <- betaNormalize f
    , NaturalLiteral 0                         <- betaNormalize b =
        NaturalLiteral 0
    | Application (Builtin NaturalSubtract) a <- betaNormalize f
    , equivalent a b =
        NaturalLiteral 0
    | Application (Builtin NaturalSubtract) a <- betaNormalize f =

        let a₁ = betaNormalize a
            b₁ = betaNormalize b

        in  Application (Application (Builtin NaturalSubtract) a₁) b₁
```

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


    ───────────────────────────────────
    Natural/subtract ⇥ Natural/subtract


```haskell
betaNormalize (Builtin NaturalBuild    ) = Builtin NaturalBuild
betaNormalize (Builtin NaturalFold     ) = Builtin NaturalFold
betaNormalize (Builtin NaturalIsZero   ) = Builtin NaturalIsZero
betaNormalize (Builtin NaturalEven     ) = Builtin NaturalEven
betaNormalize (Builtin NaturalOdd      ) = Builtin NaturalOdd
betaNormalize (Builtin NaturalToInteger) = Builtin NaturalToInteger
betaNormalize (Builtin NaturalShow     ) = Builtin NaturalShow
betaNormalize (Builtin NaturalSubtract ) = Builtin NaturalSubtract
```

## `Text`

The `Text` type is in normal form:


    ───────────
    Text ⇥ Text


```haskell
betaNormalize (Builtin Text) = Builtin Text
```

A `Text` literal with no interpolations is already in normal form:


    ─────────
    "s" ⇥ "s"


As a special case, if a `Text` literal has no content except for
interpolations, and all interpolations except one normalize to the
empty string, simplify the `Text` literal:


    t₀ ⇥ t₁   "ss₀…" ⇥ ""
    ─────────────────────
    "${t₀}ss₀…" ⇥ t₁


    t₀ ⇥ ""   "ss₀…" ⇥ t₁
    ─────────────────────
    "${t₀}ss₀…" ⇥ t₁


Otherwise, normalizing `Text` literals normalizes each interpolated
expression and inlines any interpolated expression that normalize to
`Text` literals:


    t₀ ⇥ "s₁"   "ss₀…" ⇥ "ss₂…"
    ───────────────────────────
    "s₀${t₀}ss₀…" ⇥ "s₀s₁ss₂…"


    t₀ ⇥ "s₁${t₁}ss₁…"   "ss₀…" ⇥ "ss₂…"
    ────────────────────────────────────
    "s₀${t₀}ss₀…" ⇥ "s₀s₁${t₁}ss₁…ss₂…"


    t₀ ⇥ t₁   "ss₀…" ⇥ "ss₁…"
    ─────────────────────────────  ; If no other rule matches
    "s₀${t₀}ss₀…" ⇥ "s₀${t₁}ss₁…"


```haskell
-- See the `Semigroup` instance for `Chunks` in the `Syntax.lhs` module, which
-- provides the `(<>)` operator that does the heavy lifting in this function.
betaNormalize (TextLiteral chunks₀) =
    case loop chunks₀ of
        Chunks [("", t)] "" -> t
        chunks₁             -> TextLiteral chunks₁
  where
    loop :: TextLiteral -> TextLiteral
    loop (Chunks ((s₀, t₀) : ss₀) z₀)
        | TextLiteral chunks <- t₁ =
            Chunks [] s₀ <> chunks <> loop (Chunks ss₀ z₀)
        | otherwise =
            Chunks [(s₀, t₁)] "" <> loop (Chunks ss₀ z₀)
      where
        t₁ = betaNormalize t₀
    loop chunks =
        chunks
```

The "text concatenation" operator is interpreted as two interpolations together:


    "${l}${r}" ⇥ s₀
    ───────────────
    l ++ r ⇥ s₀


```haskell
betaNormalize (Operator l TextAppend r) = s₀
  where
    s₀ = betaNormalize (TextLiteral (Chunks [("", l), ("", r)] ""))
```

The `Text/show` function replaces an uninterpolated `Text` literal
with another `Text` literal representing valid Dhall source code for
the original literal.  In particular, this function both quotes and
escapes the original literal so that if you were to render the escaped
`Text` value the result would appear to be the original `Text` input
to the function:

    -- Rendering the right-hand side gives the original argument: "abc\ndef"
    Text/show "abc\ndef" = "\"abc\\ndef\""

Escaping is done in such a way that the rendered result is not only valid
Dhall source code but also valid JSON when the `Text` does not contain any
Unicode code points above `\uFFFF`.  This comes in handy if you want to use
Dhall to generate Dhall code or to generate JSON.

The body of the `Text` literal escapes the following characters according to
these rules:

* `"`  → `\"`
* `$`  → `\u0024`
* `\b` → `\\b`
* `\f` → `\\f`
* `\n` → `\\n`
* `\r` → `\\r`
* `\t` → `\\t`
* `\`  → `\\`

Carefully note that `$` is not escaped as `\$` since that is not a valid JSON
escape sequence.

`Text/show` also escapes any non-printable characters as their Unicode escape
sequences:

* `\u0000`-`\u001F` → `\\u0000`-`\\u001F`

... since that is the only valid way to represent them within the Dhall grammar.

Or in other words:


    f ⇥ Text/show   a ⇥ "…\n…\$…\\…\"…\u0000…"
    ─────────────────────────────────────────── ; "…\n…\$…\\…\"…\u0000…" contains no interpolations
    f a ⇥ "\"…\\n…\\u0024…\\\\…\\\"…\\u0000…\""


```haskell
betaNormalize (Application f a)
    | Builtin TextShow           <- betaNormalize f
    , TextLiteral (Chunks [] s₀) <- betaNormalize a =

        let s₁ =
                ( Text.replace "\"" "\\\""
                . Text.replace "$"  "\\u0024"
                . Text.replace "\b" "\\b"
                . Text.replace "\f" "\\f"
                . Text.replace "\n" "\\n"
                . Text.replace "\r" "\\r"
                . Text.replace "\t" "\\t"
                . Text.replace "\\" "\\\\"
                ) s₀

        in  TextLiteral (Chunks [] s₁)
```

`Text/replace` modifies a substring of a given `Text` literal. It takes 3
arguments, the `Text` literal substring to match (the _needle_), the `Text`
value replacement (the _replacement_), and the `Text` literal in which to
replace all matches (the _haystack_). The _needle_ and the _haystack_ must
have reached normal form before any further beta normalization can be applied.
In the case that the substring to replace is empty (`""`), then no replacement
is performed:


    f ⇥ Text/replace "" replacement   a₀ ⇥ a₁  ; No replacement performed if
    ─────────────────────────────────────────  ; the search string is empty,
    f a₀ ⇥ a₁                                  ; to avoid an infinite loop.


    f ⇥ Text/replace "NEEDLE" replacement  ; If the "NEEDLE" is not found within
    a ⇥ "other"                            ; the string, then perform no
    ─────────────────────────────────────  ; replacement.
    f a ⇥ "other"


    f ⇥ Text/replace "NEEDLE" replacement  ; If the "NEEDLE" is found within
    a ⇥ "beforeNEEDLEafter"                ; the string, then replace.
    "before${replacement}" ++ Text/replace "NEEDLE" replacement "after" → e
    ───────────────────────────────────────────────────────────────────────
    f a ⇥ e


```haskell
betaNormalize (Application f a₀)
    | Application
        (Application (Builtin TextReplace) (TextLiteral (Chunks [] "")))
        _replacement <- betaNormalize f =

        let a₁ = betaNormalize a₀

        in  a₁
    | Application
        (Application
            (Builtin TextReplace)
            (TextLiteral (Chunks [] needle))
        )
        replacement <- betaNormalize f
    , TextLiteral (Chunks [] haystack) <- betaNormalize a₀ =

        let loop [ ] = Chunks [] ""
            loop [s] = Chunks [] s
            loop (s : ss) = Chunks ((s, replacement) : xys) z
              where
                Chunks xys z = loop ss

        in  TextLiteral (loop (Text.splitOn needle haystack))
```

All of the built-in functions on `Text` are in normal form:


    ─────────────────────
    Text/show ⇥ Text/show

    ───────────────────────────
    Text/replace ⇥ Text/replace


```haskell
betaNormalize (Builtin TextShow   ) = Builtin TextShow
betaNormalize (Builtin TextReplace) = Builtin TextReplace
```

## `List`

The `List` type-level function is in normal form:


    ───────────
    List ⇥ List


```haskell
betaNormalize (Builtin List) = Builtin List
```

Normalizing a `List` normalizes each field and the type annotation:


    T₀ ⇥ T₁
    ─────────────────
    [] : T₀ ⇥ [] : T₁


    t₀ ⇥ t₁   [ ts₀… ] ⇥ [ ts₁… ]
    ─────────────────────────────
    [ t₀, ts₀… ] ⇥ [ t₁, ts₁… ]


```haskell
betaNormalize (EmptyList _T₀) = EmptyList _T₁
  where
    _T₁ = betaNormalize _T₀
betaNormalize (NonEmptyList ts₀) = NonEmptyList ts₁
  where
    ts₁ = fmap betaNormalize ts₀
```

Lists are defined here via induction as if they were linked lists, but a real
implementation might represent them using another data structure under the hood.
Dhall does not impose time complexity requirements on list operations.

`List/build` is the canonical introduction function for `List`s:


    f ⇥ List/build A₀
    ↑(1, a, 0, A₀) = A₁
    g (List A₀) (λ(a : A₀) → λ(as : List A₁) → [ a ] # as) ([] : List A₀) ⇥ b
    ─────────────────────────────────────────────────────────────────────────
    f g ⇥ b


```haskell
betaNormalize (Application f g)
    | Application (Builtin ListBuild) _A₀ <- betaNormalize f =

        let _A₁ = shift 1 "a" 0 _A₀

            b = betaNormalize
                    (Application
                        (Application
                            (Application g (Application (Builtin List) _A₀))
                            (Lambda "a" _A₀
                                (Lambda "as" (Application (Builtin List) _A₁)
                                    (Operator
                                        (NonEmptyList (Variable "a" 0 :| []))
                                        ListAppend
                                        (Variable "as" 0)
                                    )
                                )
                            )
                        )
                        (EmptyList _A₀)
                    )

        in  b
```

`List/fold` is the canonical elimination function for `List`s:


    f ⇥ List/fold A₀ ([] : List A₁) B g   b₀ ⇥ b₁
    ─────────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ List/fold A₀ [ a, as… ] B g   g a (List/fold A₀ [ as… ] B g b₀) ⇥ b₁
    ────────────────────────────────────────────────────────────────────────
    f b₀ ⇥ b₁


```haskell
betaNormalize (Application f b₀)
    | Application
        (Application
            (Application
                (Application (Builtin ListFold) _A₀)
                (EmptyList _A₁)
            )
            _B
        )
        _g <- betaNormalize f =

        let b₁ = betaNormalize b₀

        in  b₁

betaNormalize (Application f b₀)
    | Application
        (Application
            (Application
                (Application (Builtin ListFold) _A₀)
                (NonEmptyList (a :| as))
            )
            _B
        )
        g <- betaNormalize f =

        let rest =
                case as of
                    []    -> EmptyList _A₀
                    h : t -> NonEmptyList (h :| t)

            b₁ =
                betaNormalize
                    (Application
                        (Application g a)
                        (Application
                            (Application
                                (Application
                                    (Application (Builtin ListFold) _A₀)
                                    rest
                                )
                                g
                            )
                            b₀
                        )
                    )

        in  b₁
```

Even though `List/build` and `List/fold` suffice for all `List` operations,
Dhall also supports built-in functions and operators on `List`s, both for
convenience and efficiency.

Use machine concatenation to simplify the "list concatenation" operator if both
arguments normalize to `List` literals:


    ls₀ ⇥ [ ls₁… ]
    rs₀ ⇥ [ rs₁… ]
    [ ls₁… ] # [ rs₁… ] ⇥ t
    ───────────────────────  ;  "[ ls₁… ] # [ rs₁… ]" means "use machine
    ls₀ # rs₀ ⇥ t            ;  concatenation"


Also, simplify the "list concatenation" operator if either argument normalizes
to an empty `List`:


    ls ⇥ [] : T   rs₀ ⇥ rs₁
    ───────────────────────
    ls # rs₀ ⇥ rs₁


    rs ⇥ [] : T   ls₀ ⇥ ls₁
    ───────────────────────
    ls₀ # rs ⇥ ls₁


Otherwise, normalize each argument:


    ls₀ ⇥ ls₁   rs₀ ⇥ rs₁
    ─────────────────────  ; If no other rule matches
    ls₀ # rs₀ ⇥ ls₁ # rs₁


```haskell
betaNormalize (Operator ls₀ ListAppend rs₀)
    | EmptyList _T   <- ls₁ = rs₁
    | EmptyList _T   <- rs₁ = ls₁
    | NonEmptyList l <- ls₁
    , NonEmptyList r <- rs₁ = NonEmptyList (l <> r)
    | otherwise             = Operator ls₁ ListAppend rs₁
  where
    ls₁ = betaNormalize ls₀
    rs₁ = betaNormalize rs₀
```

`List/length` returns the length of a list:


    f ⇥ List/length A₀   a ⇥ [] : A₁
    ────────────────────────────────
    f a ⇥ 0


    f ⇥ List/length A₀   as₀ ⇥ [ a, as₁… ]   1 + List/length A₀ [ as₁… ] ⇥ n
    ────────────────────────────────────────────────────────────────────────
    f as₀ ⇥ n


```haskell
betaNormalize (Application f a)
    | Application (Builtin ListLength) _A₀ <- betaNormalize f
    , EmptyList _A₁                        <- betaNormalize a =
        NaturalLiteral 0
    | Application (Builtin ListLength) _A₀ <- betaNormalize f
    , NonEmptyList as₀                     <- betaNormalize a =
        NaturalLiteral (fromIntegral (length as₀))
```

`List/head` returns the first element of a list:


    f ⇥ List/head A₀   as ⇥ [] : A₁
    ───────────────────────────────
    f as ⇥ None A₀


    f ⇥ List/head A₀   as ⇥ [ a, … ]
    ────────────────────────────────
    f as ⇥ Some a


```haskell
betaNormalize (Application f as)
    | Application (Builtin ListHead) _A₀ <- betaNormalize f
    , EmptyList _A₁                      <- betaNormalize as =
        Application (Builtin None) _A₀
    | Application (Builtin ListHead) _A₀ <- betaNormalize f
    , NonEmptyList (a :| _)              <- betaNormalize as =
        Some a
```

`List/last` returns the last element of a list:


    f ⇥ List/last A₀   as ⇥ [] : A₁
    ───────────────────────────────
    f as ⇥ None A₀


    f ⇥ List/last A₀   as ⇥ [ …, a ]
    ────────────────────────────────
    f as ⇥ Some a


```haskell
betaNormalize (Application f as)
    | Application (Builtin ListLast) _A₀ <- betaNormalize f
    , EmptyList _A₁                      <- betaNormalize as =
        Application (Builtin None) _A₀
    | Application (Builtin ListLast) _A₀ <- betaNormalize f
    , NonEmptyList as₁                   <- betaNormalize as =
        Some (NonEmpty.last as₁)
```

`List/indexed` tags each element of the list with the element's index:


    f ⇥ List/indexed A₀   as ⇥ [] : A₁
    ───────────────────────────────────────────────
    f as ⇥ [] : List { index : Natural, value : A₀ }


    f ⇥ List/indexed A₀   as ⇥ [ a₀, a₁, …, ]
    ──────────────────────────────────────────────────────────────────
    f as ⇥ [ { index = 0, value = a₀ }, { index = 1, value = a₁ }, … ]


```haskell
betaNormalize (Application f as)
    | Application (Builtin ListIndexed) _A₀ <- betaNormalize f
    , EmptyList _A₁                         <- betaNormalize as =
        EmptyList
            (Application
                (Builtin List)
                (RecordType [("index", Builtin Natural), ("value", _A₀)])
            )
    | Application (Builtin ListIndexed) _A₀ <- betaNormalize f
    , NonEmptyList as₁                      <- betaNormalize as =
        let combine index value =
                RecordLiteral
                    [("index", NaturalLiteral index), ("value", value)]

        in  NonEmptyList (NonEmpty.zipWith combine (0 :| [1..]) as₁)
```

`List/reverse` reverses the elements of the list:


    f ⇥ List/reverse A₀   as ⇥ [] : A₁
    ──────────────────────────────────
    f as ⇥ [] : A₁


    f ⇥ List/reverse A₀   as ⇥ [ a₀, a₁, … ]
    ────────────────────────────────────────
    f as ⇥ [ …, a₁, a₀ ]


```haskell
betaNormalize (Application f as)
    | Application (Builtin ListReverse) _A₀ <- betaNormalize f
    , EmptyList _A₁                         <- betaNormalize as =
        EmptyList _A₁
    | Application (Builtin ListReverse) _A₀ <- betaNormalize f
    , NonEmptyList as₁                      <- betaNormalize as =
        NonEmptyList (NonEmpty.reverse as₁)
```

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


```haskell
betaNormalize (Builtin ListBuild  ) = Builtin ListBuild
betaNormalize (Builtin ListFold   ) = Builtin ListFold
betaNormalize (Builtin ListLength ) = Builtin ListLength
betaNormalize (Builtin ListHead   ) = Builtin ListHead
betaNormalize (Builtin ListLast   ) = Builtin ListLast
betaNormalize (Builtin ListIndexed) = Builtin ListIndexed
betaNormalize (Builtin ListReverse) = Builtin ListReverse
```

## `Optional`

The `Optional` and `None` functions are in normal form:


    ───────────────────
    Optional ⇥ Optional


    ───────────
    None ⇥ None


```haskell
betaNormalize (Builtin Optional) = Builtin Optional
betaNormalize (Builtin None    ) = Builtin None
```

Normalize a `Some` expression by normalizing its argument:


    t₀ ⇥ t₁
    ─────────────────
    Some t₀ ⇥ Some t₁


```haskell
betaNormalize (Some t₀) = Some t₁
  where
    t₁ = betaNormalize t₀
```

## Records

Normalizing a record type sorts the fields and normalizes the type of each
field:


    ───────
    {} ⇥ {}


    T₀ ⇥ T₁   { xs₀… } ⇥ { xs₁… }
    ───────────────────────────────────
    { x : T₀, xs₀… } ⇥ { x : T₁, xs₁… }


```haskell
betaNormalize (RecordType xTs₀) = RecordType xTs₁
  where
    xTs₁ = List.sortBy (Ord.comparing fst) (map adapt xTs₀)

    adapt (x, _T₀) = (x, _T₁)
      where
        _T₁ = betaNormalize _T₀
```

Normalizing a record value sorts the fields and normalizes each field:


    ─────────
    {=} ⇥ {=}


    t₀ ⇥ t₁   { xs₀… } ⇥ { xs₁… }
    ───────────────────────────────────
    { x = t₀, xs₀… } ⇥ { x = t₁, xs₁… }


```haskell
betaNormalize (RecordLiteral xts₀) = RecordLiteral xts₁
  where
    xts₁ = List.sortBy (Ord.comparing fst) (map adapt xts₀)

    adapt (x, t₀) = (x, t₁)
      where
        t₁ = betaNormalize t₀
```

Simplify a record selection if the argument is a record literal:


    t ⇥ { x = v, … }
    ──────────────────
    t.x ⇥ v


```haskell
betaNormalize (Field t x)
    | RecordLiteral xvs <- betaNormalize t
    , Just v            <- lookup x xvs =
        v
```

If the argument is a record projection, select from the contained record.


    t₀ ⇥ t₁.{ xs… }   t₁.x ⇥ v
    ──────────────────────────
    t₀.x ⇥ v


```haskell
betaNormalize (Field t₀ x)
    | ProjectByLabels t₁ _xs <- betaNormalize t₀
    , v                      <- betaNormalize (Field t₁ x) =
        v
```

If the argument is a right-biased record merge and one of the operands is a
record literal, we can simplify further:


    t₀ ⇥ { x = v, … } ⫽ t₁
    ─────────────────────────
    t₀.x ⇥ ({ x = v } ⫽ t₁).x


    t₀ ⇥ { xs… } ⫽ t₁   t₁.x ⇥ v
    ────────────────────────────  ; x ∉ xs
    t₀.x ⇥ v


    t₀ ⇥ t₁ ⫽ { x = v, … }
    ──────────────────────
    t₀.x ⇥ v


    t₀ ⇥ t₁ ⫽ { xs… }   t₁.x ⇥ v
    ────────────────────────────  ; x ∉ xs
    t₀.x ⇥ v


```haskell
betaNormalize (Field t₀ x)
    | Operator (RecordLiteral xvs) Prefer t₁ <- betaNormalize t₀
    , Just v                                 <- lookup x xvs =
        Field (Operator (RecordLiteral [(x, v)]) Prefer t₁) x

    | Operator (RecordLiteral xvs) Prefer t₁ <- betaNormalize t₀
    , Nothing                                <- lookup x xvs =

        let v = betaNormalize (Field t₁ x)

        in  v

    | Operator _t₁ Prefer (RecordLiteral xvs) <- betaNormalize t₀
    , Just v                                  <- lookup x xvs =
        v

    | Operator t₁ Prefer (RecordLiteral xvs) <- betaNormalize t₀
    , Nothing                                <- lookup x xvs =

        let v = betaNormalize (Field t₁ x)

        in  v
```

If the argument is a recursive record merge and one of the operands is a record
literal, we can simplify it similarly:


    t₀ ⇥ { x = v, … } ∧ t₁
    ─────────────────────────
    t₀.x ⇥ ({ x = v } ∧ t₁).x


    t₀ ⇥ { xs… } ∧ t₁   t₁.x ⇥ v
    ────────────────────────────  ; x ∉ xs
    t₀.x ⇥ v


    t₀ ⇥ t₁ ∧ { x = v, … }
    ─────────────────────────
    t₀.x ⇥ (t₁ ∧ { x = v }).x


    t₀ ⇥ t₁ ∧ { xs… }   t₁.x ⇥ v
    ────────────────────────────  ; x ∉ xs
    t₀.x ⇥ v


```haskell
betaNormalize (Field t₀ x)
    | Operator (RecordLiteral xvs) CombineRecordTerms t₁ <- betaNormalize t₀
    , Just v                                             <- lookup x xvs =
        Operator (RecordLiteral [(x, v)]) CombineRecordTerms t₁

    | Operator (RecordLiteral xvs) CombineRecordTerms t₁ <- betaNormalize t₀
    , Nothing                                            <- lookup x xvs =

        let v = betaNormalize (Field t₁ x)

        in  v
    | Operator t₁ CombineRecordTerms (RecordLiteral xvs) <- betaNormalize t₀
    , Just v                                             <- lookup x xvs =
        Operator t₁ CombineRecordTerms (RecordLiteral [(x, v)])
    | Operator t₁ CombineRecordTerms (RecordLiteral xvs) <- betaNormalize t₀
    , Nothing                                            <- lookup x xvs =

        let v = betaNormalize (Field t₁ x)

        in  v
```

Otherwise, normalize the argument:


    t₀ ⇥ t₁
    ───────────  ; If no other rule matches
    t₀.x ⇥ t₁.x


```haskell
betaNormalize (Field t₀ x) = Field t₁ x
  where
    t₁ = betaNormalize t₀
```

You can also project out more than one field into a new record:


    ──────────
    t.{} ⇥ {=}


Simplify a record projection if the argument is a record literal:


    t ⇥ { x = v, ts… }   { ts… }.{ xs… } ⇥ { ys… }
    ──────────────────────────────────────────────
    t.{ x, xs… } ⇥ { x = v, ys… }


If the argument is itself a projection, skip the inner projection:


    t₀ ⇥ t₁.{ ys… }   t₁.{ xs… } ⇥ t₂
    ─────────────────────────────────
    t₀.{ xs… } ⇥ t₂


If the argument is a right-biased record merge and its right operand reveals
the fields it contains, simplify:


    t₀ ⇥ l ⫽ { rs… }
    keys(rs…) = ks…
    l.{ xs… \ ks… } ⫽ { rs… }.{ xs… ∩ ks… } ⇥ t₁
    ────────────────────────────────────────────  ; "\" means set difference, "∩" means set intersection
    t₀.{ xs… } ⇥ t₁


Otherwise, normalize the argument and sort the fields:


    t₀ ⇥ t₁   sort(xs₀…) = xs₁…
    ───────────────────────────
    t₀.{ xs₀… } ⇥ t₁.{ xs₁… }


```haskell
betaNormalize (ProjectByLabels _ []) = RecordLiteral []
betaNormalize (ProjectByLabels t₀ xs₀)
    | RecordLiteral xvs <- betaNormalize t₀ =

        let predicate (x, _v) = x `elem` xs₀

        in  RecordLiteral (filter predicate xvs)

    | ProjectByLabels t₁ _ys  <- betaNormalize t₀ =

        let t₂ = betaNormalize (ProjectByLabels t₁ xs₀)

        in  t₂

    | Operator l Prefer (RecordLiteral rs) <- betaNormalize t₀ =
        let ks = map fst rs

            predicate x = x `elem` ks

            t₁ =
                Operator
                    (ProjectByLabels l (xs₀ \\ ks))
                    Prefer
                    (ProjectByLabels (RecordLiteral rs) (filter predicate xs₀))

        in  t₁
    | otherwise =
        let t₁ = betaNormalize t₀

            xs₁ = List.sort xs₀

        in  ProjectByLabels t₁ xs₁
```

You can also project by type:


    s ⇥ { ss… }
    keys(s) = s₁
    t.{s₁} ⇥ ts₁
    ────────────
    t.(s) ⇥ ts₁


```haskell
betaNormalize (ProjectByType t s)
    | RecordType ss <- betaNormalize s =
        let s₁ = map fst ss

            ts₁ = betaNormalize (ProjectByLabels t s₁)

        in  ts₁
```

The type system ensures that the selected field(s) must be present.  The type
system also ensures that in the expression `t.(s)`, `s` will normalize to a
record type, so the last rule above will always match.

Recursive record merge combines two records, recursively merging any fields that
collide.  The type system ensures that colliding fields must be records:


    ls ⇥ {=}   rs₀ ⇥ rs₁
    ────────────────────
    ls ∧ rs₀ ⇥ rs₁


    rs ⇥ {=}   ls₀ ⇥ ls₁
    ────────────────────
    ls₀ ∧ rs ⇥ ls₁


    ls₀ ⇥ { x = l₁, ls₁… }
    rs₀ ⇥ { x = r₁, rs₁… }
    l₁ ∧ r₁ ⇥ t
    { ls₁… } ∧ { rs₁… } ⇥ { ts… }
    { x = t, ts… } ⇥ e             ; To ensure the fields are sorted
    ─────────────────────────────
    ls₀ ∧ rs₀ ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }
    { ls₁… } ∧ rs ⇥ { ls₂… }
    { x = l₁, ls₂… } ⇥ e      ; To ensure the fields are sorted
    ────────────────────────  ; x ∉ rs
    ls₀ ∧ rs ⇥ e


    ls₀ ⇥ ls₁   rs₀ ⇥ rs₁
    ─────────────────────  ; If no other rule matches
    ls₀ ∧ rs₀ ⇥ ls₁ ∧ rs₁


Right-biased record merge is non-recursive.  Field collisions are resolved by
preferring the field from the right record and discarding the colliding field
from the left record:


    l ⇥ e   r ⇥ {=}
    ───────────────
    l ⫽ r ⇥ e


    l ⇥ {=}   r ⇥ e
    ───────────────
    l ⫽ r ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }
    rs₀ ⇥ { x = r₁, rs₁… }
    { ls₁… } ⫽ { rs₁… } ⇥ { ts… }
    { x = r₁, ts… } ⇥ e            ; To ensure the fields are sorted
    ─────────────────────────────
    ls₀ ⫽ rs₀ ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }
    { ls₁… } ⫽ rs ⇥ { ls₂… }
    { x = l₁, ls₂… } ⇥ e      ;  To ensure the fields are sorted
    ────────────────────────  ;  x ∉ rs
    ls₀ ⫽ rs ⇥ e


    l₀ ≡ r   l₀ ⇥ l₁
    ────────────────
    l₀ ⫽ r ⇥ l₁


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ ⫽ r₀ ⇥ l₁ ⫽ r₁


A record update using the `with` keyword replaces the given (possibly-nested) field:


    e₀ ⇥ { k = e₁, es… }
    v₀ ⇥ v₁
    ────────────────────────────────
    e₀ with k = v₀ ⇥ { k = v₁, es… }


    e₀ ⇥ { es… }
    v₀ ⇥ v₁
    ────────────────────────────────  ; k ∉ es
    e₀ with k = v₀ ⇥ { k = v₁, es… }


    e₀ ⇥ { k₀ = e₁, es… }
    e₁ with k₁.ks… = v ⇥ e₂
    ───────────────────────────────────────
    e₀ with k₀.k₁.k… = v ⇥ { k₀ = e₂, es… }


    e₀ ⇥ { es… }
    {=} with k₁.ks… = v ⇥ e₁
    ───────────────────────────────────────  ; k₀ ∉ es
    e₀ with k₀.k₁.k… = v ⇥ { k₀ = e₁, es… }


    e₀ ⇥ e₁   v₀ ⇥ v₁
    ───────────────────────────────────  ; If no other rule matches
    e₀ with ks… = v₀ ⇥ e₁ with ks… = v₁


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
    { x : t, ts… } ⇥ e             ; To ensure the fields are sorted
    ─────────────────────────────
    ls₀ ⩓ rs₀ ⇥ e


    ls₀ ⇥ { x : l₁, ls₁… }
    { ls₁… } ⩓ rs ⇥ { ls₂… }
    { x : l₁, ls₂… } ⇥ e      ; To ensure the fields are sorted
    ────────────────────────  ; x ∉ rs
    ls₀ ⩓ rs ⇥ e


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ ⩓ r₀ ⇥ l₁ ⩓ r₁


A record whose fields all have the same type (*i.e.*, a *homogeneous* record) can be converted to a list where each list
item represents a field. The value "x" below represents the text value of the field name `x`.


    t ⇥ { x = v, ts… }   toMap { ts } ⇥ m
    ──────────────────────────────────────────────
    toMap t ⇥ [ {mapKey = "x", mapValue = v} ] # m


The `toMap` application can be annotated with a type, and it must be if the record is empty.


    t ⇥ { x = v, ts… }   toMap { ts } ⇥ m
    ───────────────────────────────────────────────────
    toMap t : T₀ ⇥ [ {mapKey = "x", mapValue = v} ] # m


    t ⇥ {=}   T₀ ⇥ T₁
    ──────────────────────
    toMap t : T₀ ⇥ [] : T₁


If the record or the type is abstract, then normalize each subexpression:


    t₀ ⇥ t₁   T₀ ⇥ T₁
    ─────────────────────────────  ; If no other rule matches
    toMap t₀ : T₀ ⇥ toMap t₁ : T₁


    t₀ ⇥ t₁
    ───────────────────  ; If no other rule matches
    toMap t₀ ⇥ toMap t₁


`T::r` is syntactic sugar for `(T.default ⫽ r) : T.Type` so substitute
accordingly and continue to normalize:


    ((T.default ⫽ r) : T.Type) ⇥ e
    ──────────────────────────────
    T::r ⇥ e


## Unions

Normalizing a union type sorts the alternatives and normalizes the type of each
alternative:


    ───────
    <> ⇥ <>


    T₀ ⇥ T₁   < xs₀… > ⇥ < xs₁… >
    ─────────────────────────────────────
    < x : T₀ | xs₀… > ⇥ < x : T₁ | xs₁… >


    < xs₀… > ⇥ < xs₁… >
    ───────────────────────────
    < x | xs₀… > ⇥ < x | xs₁… >


Normalizing a union constructor only normalizes the union type but is otherwise
inert.  The expression does not reduce further until supplied to a `merge`.


    u ⇥ < x₀ : T₀ | xs… >
    ───────────────────────────
    u.x₀ ⇥ < x₀ : T₀ | xs… >.x₀


    u ⇥ < x₀ | xs… >
    ──────────────────────
    u.x₀ ⇥ < x₀ | xs… >.x₀


`merge` expressions are the canonical way to eliminate a union value.  The
first argument to `merge` is a record of handlers and the second argument is a
union value, which can be in one of two forms:

* A union constructor for a non-empty alternative: `< x : T | … >.x v`
* A union constructor for an empty alternative: `< x | … >.x`

For union constructors specifying non-empty alternatives, apply the handler of
the same label to the wrapped value of the union constructor:


    t ⇥ { x = f, … }   u ⇥ < x : T₀ | … >.x a   f a ⇥ b
    ───────────────────────────────────────────────────
    merge t u : T ⇥ b


    t ⇥ { x = f, … }   u ⇥ < x : T | … >.x a   f a ⇥ b
    ──────────────────────────────────────────────────
    merge t u ⇥ b


For union constructors specifying empty alternatives, return the handler of the
matching label:


    t ⇥ { x = v, … }   u ⇥ < x | … >.x
    ──────────────────────────────────
    merge t u : T ⇥ v


    t ⇥ { x = v, … }   u ⇥ < x | … >.x
    ──────────────────────────────────
    merge t u ⇥ v


`Optional`s are handled as if they were union values of type
`< None | Some : A >`:


    t ⇥ { Some = f, … }   o ⇥ Some a   f a ⇥ b
    ──────────────────────────────────────────
    merge t o : T ⇥ b


    t ⇥ { Some = f, … }   o ⇥ Some a   f a ⇥ b
    ──────────────────────────────────────────
    merge t o ⇥ b


    t ⇥ { None = v, … }   o ⇥ None A
    ────────────────────────────────
    merge t o : T ⇥ v


    t ⇥ { None = v, … }   o ⇥ None A
    ────────────────────────────────
    merge t o ⇥ v


If the handler or union are abstract, then normalize each subexpression:


    t₀ ⇥ t₁   u₀ ⇥ u₁   T₀ ⇥ T₁
    ───────────────────────────────────  ; If no other rule matches
    merge t₀ u₀ : T₀ ⇥ merge t₁ u₁ : T₁


    t₀ ⇥ t₁   u₀ ⇥ u₁
    ─────────────────────────  ; If no other rule matches
    merge t₀ u₀ ⇥ merge t₁ u₁


## `Integer`

The `Integer` type is in normal form:


    ─────────────────
    Integer ⇥ Integer


An `Integer` literal is in normal form:


    ───────
    ±n ⇥ ±n


`Integer/toDouble` transforms an `Integer` into the corresponding `Double`:


    f ⇥ Integer/toDouble   a ⇥ ±n
    ─────────────────────────────
    f a ⇥ ±n.0

Note that if the magnitude of `a` is greater than 2^53, `Integer/toDouble a`
may result in loss of precision. A `Double` will be selected by rounding `a` to
the nearest `Double`. Ties go to the `Double` with an even least significant
bit. When the magnitude of `a` is greater than or equal to `c`, the magnitude
will round to `Infinity`, where `c = 2^1024 - 2^970 ≈ 1.8e308`.

`Integer/show` transforms an `Integer` into a `Text` literal representing valid
Dhall code for representing that `Integer` number:


    f ⇥ Integer/show   a ⇥ ±n
    ─────────────────────────
    f a ⇥ "±n"


Note that the `Text` representation of the rendered `Integer` should include
a leading `+` sign if the number is non-negative and a leading `-` sign if
the number is negative.


`Integer/negate` inverts the sign of an `Integer`, leaving `0` unchanged:


    f ⇥ Integer/negate   a ⇥ +0
    ────────────────────────────  ; `0` negated is still `0`.
    f a ⇥ +0


    f ⇥ Integer/negate   a ⇥ +n
    ────────────────────────────
    f a ⇥ -n


    f ⇥ Integer/negate   a ⇥ -n
    ────────────────────────────
    f a ⇥ +n


`Integer/clamp` converts an `Integer` to a `Natural` number, with negative
numbers becoming `0`:


    f ⇥ Integer/clamp   a ⇥ +n
    ───────────────────────────
    f a ⇥ n


    f ⇥ Integer/clamp   a ⇥ -n
    ───────────────────────────  ; Negative integers become `0`.
    f a ⇥ 0


All of the built-in functions on `Integer`s are in normal form:


    ───────────────────────────
    Integer/show ⇥ Integer/show


    ───────────────────────────────────
    Integer/toDouble ⇥ Integer/toDouble


    ───────────────────────────────
    Integer/negate ⇥ Integer/negate


    ─────────────────────────────
    Integer/clamp ⇥ Integer/clamp


## `Double`

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


The following 2 properties must hold for `Double/show`:

```
show (read (show (X : Double))) = show X

read (show (read (Y : Text))) = read Y
```

where `show : Double → Text` is shorthand for `Double/show` and `read : Text →
Double` is the function in the implementation of Dhall which takes a correctly
formated text representation of a `Double` as input and outputs a `Double`.


## Functions

Normalizing a function type normalizes the types of the input and output:


    A₀ ⇥ A₁   B₀ ⇥ B₁
    ───────────────────────────────
    ∀(x : A₀) → B₀ ⇥ ∀(x : A₁) → B₁


You can introduce an anonymous function using a λ:


    A₀ ⇥ A₁   b₀ ⇥ b₁
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


## `let` expressions

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


## Type annotations

Simplify a type annotation by removing the annotation:


    t₀ ⇥ t₁
    ───────────
    t₀ : T ⇥ t₁

## Assertions

Normalize an assertion by normalizing its type annotation:


    T₀ ⇥ T₁
    ─────────────────────────
    assert : T₀ ⇥ assert : T₁


Normalize an equivalence by normalizing each side of the equivalence:


    x₀ ⇥ x₁   y₀ ⇥ y₁
    ─────────────────────
    x₀ === y₀ ⇥ x₁ === y₁

## Imports

An expression with unresolved imports cannot be β-normalized.
