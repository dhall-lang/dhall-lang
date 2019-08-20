  λ(T : Type)
→ λ(f : T → Bool)
→ λ(x : T)
→ merge { a = f } (< a : T >.a x) : Bool
