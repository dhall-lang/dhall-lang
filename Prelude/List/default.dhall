{-|
Unpack an `Optional` containing a `List`, defaulting to an empty list when the
`Optional` is `None`
-}
let default
    : ∀(a : Type) → Optional (List a) → List a
    = λ(a : Type) →
      λ(o : Optional (List a)) →
        merge { Some = λ(l : List a) → l, None = [] : List a } o

let example0 = assert : default Bool (None (List Bool)) ≡ ([] : List Bool)

let example1 = assert : default Bool (Some [ True ]) ≡ [ True ]

in  default
