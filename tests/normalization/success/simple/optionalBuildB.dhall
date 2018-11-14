{ example0 =
    [ 1 ] : Optional Natural
, example1 =
    [ +1 ] : Optional Integer
, example2 =
    λ(id : ∀(a : Type) → a → a) → id (Optional Bool) ([ True ] : Optional Bool)
, example3 =
    λ(a : Type) → λ(x : a) → [ x ] : Optional a
}
