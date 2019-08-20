  λ(T : Type)
→ λ(f : ∀(optional : Type) → (T → optional) → optional → optional)
→ f (Optional T) (λ(a : T) → Some a) (None T)
