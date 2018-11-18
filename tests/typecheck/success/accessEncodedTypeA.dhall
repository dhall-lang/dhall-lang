  λ ( record
    : ∀(k : Kind) → ∀(makeRecord : ∀(x : Type) → ∀(y : Type → Type) → k) → k
    )
→ record Type (λ(x : Type) → λ(y : Type → Type) → x)
