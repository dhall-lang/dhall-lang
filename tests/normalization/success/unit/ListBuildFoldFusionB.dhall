  λ(T : Type)
→ λ(x : List T)
→ List/fold
    T
    x
    (List T)
    (λ(a : T) → λ(`as` : List T) → [ a ] # `as`)
    ([] : List T)
