--| An empty `Map` of the given key and value types
let Map =
        ./Type.dhall
          sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed
      ? ./Type.dhall

let empty
    : ∀(k : Type) → ∀(v : Type) → Map k v
    = λ(k : Type) → λ(v : Type) → [] : Map k v

let example0 =
      assert : empty Text Bool ≡ ([] : List { mapKey : Text, mapValue : Bool })

in  empty
