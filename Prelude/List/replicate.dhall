--| Build a list by copying the given element the specified number of times
let replicate
    : Natural → ∀(a : Type) → a → List a
    = λ(n : Natural) →
      λ(a : Type) →
      λ(x : a) →
        List/build
          a
          ( λ(list : Type) →
            λ(cons : a → list → list) →
              Natural/fold n list (cons x)
          )

let example0 = assert : replicate 9 Natural 1 ≡ [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ]

let example1 = assert : replicate 0 Natural 1 ≡ ([] : List Natural)

in  replicate
