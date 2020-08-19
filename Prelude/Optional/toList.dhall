--| Convert an `Optional` value into the equivalent `List`
let toList
    : ∀(a : Type) → Optional a → List a
    = λ(a : Type) →
      λ(o : Optional a) →
        merge { Some = λ(x : a) → [ x ] : List a, None = [] : List a } o

let example0 = assert : toList Natural (Some 1) ≡ [ 1 ]

let example1 = assert : toList Natural (None Natural) ≡ ([] : List Natural)

in  toList
