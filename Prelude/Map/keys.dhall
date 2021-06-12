--| Get all of the keys of a `Map` as a `List`
let Map =
        ./Type.dhall
          sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed
      ? ./Type.dhall

let Entry =
        ./Entry.dhall
          sha256:f334283bdd9cd88e6ea510ca914bc221fc2dab5fb424d24514b2e0df600d5346
      ? ./Entry.dhall

let List/map =
        ../List/map
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ../List/map

let keys
    : ∀(k : Type) → ∀(v : Type) → Map k v → List k
    = λ(k : Type) →
      λ(v : Type) →
        List/map (Entry k v) k (λ(x : Entry k v) → x.mapKey)

let example0 =
        assert
      :   keys
            Text
            Natural
            [ { mapKey = "A", mapValue = 2 }
            , { mapKey = "B", mapValue = 3 }
            , { mapKey = "C", mapValue = 5 }
            ]
        ≡ [ "A", "B", "C" ]

let example1 =
        assert
      :   keys Text Natural ([] : List { mapKey : Text, mapValue : Natural })
        ≡ ([] : List Text)

in  keys
