  λ(T : Type)
→ λ(f : ∀(list : Type) → (T → list → list) → list → list)
→ f (List T) (λ(a : T) → λ(`as` : List T) → [ a ] # `as`) ([] : List T)
