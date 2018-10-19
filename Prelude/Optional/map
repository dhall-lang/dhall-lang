{-
Transform an `Optional` value with a function

Examples:

```
./map Natural Bool Natural/even (Some 3) = Some False

./map Natural Bool Natural/even (None Natural) = None Bool
```
-}
    let map
        : ∀(a : Type) → ∀(b : Type) → (a → b) → Optional a → Optional b
        =   λ(a : Type)
          → λ(b : Type)
          → λ(f : a → b)
          → λ(o : Optional a)
          → Optional/fold a o (Optional b) (λ(x : a) → Some (f x)) (None b)

in  map
