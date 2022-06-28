{-|
Zip two `List` into a single `List`

The resulting `List` will have the length of the shortest of its arguments.
-}
let List/index =
        ./index.dhall
          sha256:7a8d7f1e038f601fd1ae93bcc904e91aef9ef6453e3da63d18b24321de79741e
      ? ./index.dhall

let zip
    : ∀(a : Type) → List a → ∀(b : Type) → List b → List { _1 : a, _2 : b }
    = λ(a : Type) →
      λ(xs : List a) →
      λ(b : Type) →
      λ(ys : List b) →
        let ixs = List/indexed a xs

        in  List/build
              { _1 : a, _2 : b }
              ( λ(list : Type) →
                λ(cons : { _1 : a, _2 : b } → list → list) →
                λ(nil : list) →
                  List/fold
                    { index : Natural, value : a }
                    ixs
                    list
                    ( λ(ix : { index : Natural, value : a }) →
                      λ(rest : list) →
                        merge
                          { None = rest
                          , Some =
                              λ(y : b) → cons { _1 = ix.value, _2 = y } rest
                          }
                          (List/index ix.index b ys)
                    )
                    nil
              )

let example0 =
        assert
      :   zip Text [ "ABC", "DEF", "GHI" ] Natural [ 1, 2, 3 ]
        ≡ [ { _1 = "ABC", _2 = 1 }
          , { _1 = "DEF", _2 = 2 }
          , { _1 = "GHI", _2 = 3 }
          ]

let example1 =
        assert
      :   zip Text [ "ABC" ] Bool ([] : List Bool)
        ≡ ([] : List { _1 : Text, _2 : Bool })

let example2 =
        assert
      :   zip Text [ "ABC", "DEF", "GHI" ] Natural [ 1 ]
        ≡ [ { _1 = "ABC", _2 = 1 } ]

in  zip
