{-
Only keep elements of the list where the supplied function returns `True`

Examples:

```
./filter Natural Natural/even [ 2, 3, 5 ]
= [ 2 ]

./filter Natural Natural/odd [ 2, 3, 5 ]
= [ 3, 5 ]
```
-}
    let filter
        : ∀(a : Type) → (a → Bool) → List a → List a
        =   λ(a : Type)
          → λ(f : a → Bool)
          → λ(xs : List a)
          → List/build
            a
            (   λ(list : Type)
              → λ(cons : a → list → list)
              → List/fold
                a
                xs
                list
                (λ(x : a) → λ(xs : list) → if f x then cons x xs else xs)
            )

in  filter
