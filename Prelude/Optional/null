{-
Returns `True` if the `Optional` value is absent and `False` if present

Examples:

```
./null Natural (Some 2) = False

./null Natural (None Natural) = True
```
-}
    let null
        : ∀(a : Type) → Optional a → Bool
        =   λ(a : Type)
          → λ(xs : Optional a)
          → Optional/fold a xs Bool (λ(_ : a) → False) True

in  null
