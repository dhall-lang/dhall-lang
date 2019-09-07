    let anonymousFunction = λ(a : Type) → List a

in    λ(HigherOrderType : (Type → Type) → Type)
    → λ(x : HigherOrderType anonymousFunction)
    → (x : HigherOrderType anonymousFunction)
