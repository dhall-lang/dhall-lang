--| An empty list of the given type
let empty
    : ∀(a : Type) → List a
    = λ(a : Type) → [] : List a

let example0 = assert : empty Bool ≡ ([] : List Bool)

in  empty
