  ∀(a : Type)
→ (   ∀(optional : Type)
    → ∀(just : a → optional)
    → ∀(nothing : optional)
    → optional
  )
→ Optional a
