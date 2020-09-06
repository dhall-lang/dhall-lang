--| `build` is the inverse of `fold`
let build
    : ∀(a : Type) →
      (∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list) →
        List a
    = List/build

let example0 =
        assert
      :   build
            Text
            ( λ(list : Type) →
              λ(cons : Text → list → list) →
              λ(nil : list) →
                cons "ABC" (cons "DEF" nil)
            )
        ≡ [ "ABC", "DEF" ]

let example1 =
        assert
      :   build
            Text
            ( λ(list : Type) →
              λ(cons : Text → list → list) →
              λ(nil : list) →
                nil
            )
        ≡ ([] : List Text)

in  build
