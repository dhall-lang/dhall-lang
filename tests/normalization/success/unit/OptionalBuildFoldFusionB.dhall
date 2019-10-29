  λ(T : Type)
→ λ(x : Optional T)
→ Optional/fold T x (Optional T) (λ(a : T) → Some a) (None T)
