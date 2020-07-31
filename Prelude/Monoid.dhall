{-|
Any function `f` that is a `Monoid` must satisfy the following law:

```
t  : Type
f  : ./Monoid t
xs : List (List t)

f (./List/concat t xs) = f (./map (List t) t f xs)
```

Examples:

```
./Bool/and
    : ./Monoid Bool
./Bool/or
    : ./Monoid Bool
./Bool/even
    : ./Monoid Bool
./Bool/odd
    : ./Monoid Bool
./List/concat
    : ∀(a : Type) → ./Monoid (List a)
./List/shifted
    : ∀(a : Type) → ./Monoid (List { index : Natural, value : a })
./Natural/sum
    : ./Monoid Natural
./Natural/product
    : ./Monoid Natural
./Optional/head
    : ∀(a : Type) → ./Monoid (Optional a)
./Optional/last
    : ∀(a : Type) → ./Monoid (Optional a)
./Text/concat
    : ./Monoid Text
```
-}
let Monoid
    : ∀(m : Type) → Type
    = λ(m : Type) → List m → m

in  Monoid
