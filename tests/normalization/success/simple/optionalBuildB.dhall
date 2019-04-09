{ example0 =
    Some 1
, example1 =
    Some +1
, example2 =
    λ(id : ∀(a : Type) → a → a) → id (Optional Bool) (Some True)
, example3 =
    λ(a : Type) → λ(x : a) → Some x
}
