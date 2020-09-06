{-|
Build a list by calling the supplied function on all `Natural` numbers from `0`
up to but not including the supplied `Natural` number
-}
let generate
    : Natural → ∀(a : Type) → (Natural → a) → List a
    = λ(n : Natural) →
      λ(a : Type) →
      λ(f : Natural → a) →
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
                (λ(x : { index : Natural, value : {} }) → cons (f x.index))
          )

let example0 =
      assert : generate 5 Bool Natural/even ≡ [ True, False, True, False, True ]

let example1 = assert : generate 0 Bool Natural/even ≡ ([] : List Bool)

in  generate
