{ example0 =
    [ True, False ] : List Bool
, example1 =
    [ True, False ] : List Bool
, example2 =
    λ(id : ∀(a : Type) → a → a) → id (List Bool) [ True, False ]
}
