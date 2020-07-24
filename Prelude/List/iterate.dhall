{-|
Generate a list of the specified length given a seed value and transition
function
-}
let iterate
    : Natural → ∀(a : Type) → (a → a) → a → List a
    = λ(n : Natural) →
      λ(a : Type) →
      λ(f : a → a) →
      λ(x : a) →
        List/build
          a
          ( λ(list : Type) →
            λ(cons : a → list → list) →
              List/fold
                { index : Natural, value : {} }
                ( List/indexed
                    {}
                    ( List/build
                        {}
                        ( λ(list : Type) →
                          λ(cons : {} → list → list) →
                            Natural/fold n list (cons {=})
                        )
                    )
                )
                list
                ( λ(y : { index : Natural, value : {} }) →
                    cons (Natural/fold y.index a f x)
                )
          )

let example0 =
        assert
      :   iterate 10 Natural (λ(x : Natural) → x * 2) 1
        ≡ [ 1, 2, 4, 8, 16, 32, 64, 128, 256, 512 ]

let example1 =
        assert
      : iterate 0 Natural (λ(x : Natural) → x * 2) 1 ≡ ([] : List Natural)

in  iterate
