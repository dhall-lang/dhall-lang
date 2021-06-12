--| Transform a `Map` by applying a function to each value
let Map =
        ./Type.dhall
          sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed
      ? ./Type.dhall

let Entry =
        ./Entry.dhall
          sha256:f334283bdd9cd88e6ea510ca914bc221fc2dab5fb424d24514b2e0df600d5346
      ? ./Entry.dhall

let List/map =
        ../List/map.dhall
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ../List/map.dhall

let map
    : ∀(k : Type) → ∀(a : Type) → ∀(b : Type) → (a → b) → Map k a → Map k b
    = λ(k : Type) →
      λ(a : Type) →
      λ(b : Type) →
      λ(f : a → b) →
      λ(m : Map k a) →
        List/map
          (Entry k a)
          (Entry k b)
          ( λ(before : Entry k a) →
              { mapKey = before.mapKey, mapValue = f before.mapValue }
          )
          m

let example0 =
        assert
      :   map
            Text
            Natural
            Bool
            Natural/even
            [ { mapKey = "A", mapValue = 2 }
            , { mapKey = "B", mapValue = 3 }
            , { mapKey = "C", mapValue = 5 }
            ]
        ≡ [ { mapKey = "A", mapValue = True }
          , { mapKey = "B", mapValue = False }
          , { mapKey = "C", mapValue = False }
          ]

let example1 =
        assert
      :   map
            Text
            Natural
            Bool
            Natural/even
            ([] : List { mapKey : Text, mapValue : Natural })
        ≡ ([] : List { mapKey : Text, mapValue : Bool })

in  map
