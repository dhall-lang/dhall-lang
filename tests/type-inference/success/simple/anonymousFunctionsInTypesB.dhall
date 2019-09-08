  ∀(HigherOrderType : (Type → Type) → Type)
→ ∀(x : HigherOrderType (λ(a : Type) → List a))
→ HigherOrderType (λ(a : Type) → List a)
