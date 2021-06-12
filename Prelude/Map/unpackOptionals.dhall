--| Turn a `Map k (Optional v)` into a `Map k v` by dropping all
--  entries with value `None`.
let List/concatMap =
        ../List/concatMap.dhall
          sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64
      ? ../List/concatMap.dhall

let Map/Entry =
        ./Entry.dhall
          sha256:f334283bdd9cd88e6ea510ca914bc221fc2dab5fb424d24514b2e0df600d5346
      ? ./Entry.dhall

let Map/Type =
        ./Type.dhall
          sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed
      ? ./Type.dhall

let unpackOptionals
    : ∀(k : Type) → ∀(v : Type) → Map/Type k (Optional v) → Map/Type k v
    = λ(k : Type) →
      λ(v : Type) →
        List/concatMap
          (Map/Entry k (Optional v))
          (Map/Entry k v)
          ( λ(e : Map/Entry k (Optional v)) →
              merge
                { None = [] : Map/Type k v
                , Some = λ(v : v) → [ { mapKey = e.mapKey, mapValue = v } ]
                }
                e.mapValue
          )

let example0 =
        assert
      :   unpackOptionals
            Text
            Text
            (toMap { foo = Some "bar", baz = None Text })
        ≡ toMap { foo = "bar" }

in  unpackOptionals
