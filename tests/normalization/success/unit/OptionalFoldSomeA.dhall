  λ(T : Type)
→ λ(x : T)
→ λ(y : Bool)
→ Optional/fold T (Some x) Bool (λ(_ : T) → False) y
