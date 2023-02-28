--| Tag each element of the list with its index
let List/WithIndex = ./WithIndex.dhall

let indexed
    : ∀(a : Type) → List a → List (List/WithIndex a)
    = List/indexed

let example0 =
        assert
      :   indexed Bool [ True, False, True ]
        ≡ [ { index = 0, value = True }
          , { index = 1, value = False }
          , { index = 2, value = True }
          ]

let example1 =
      assert : indexed Bool ([] : List Bool) ≡ ([] : List (List/WithIndex Bool))

in  indexed
