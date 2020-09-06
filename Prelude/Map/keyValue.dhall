{-|
Builds a key-value record such that a List of them will be converted to a
homogeneous record by dhall-to-json and dhall-to-yaml.
-}
let keyValue =
      λ(v : Type) →
      λ(key : Text) →
      λ(value : v) →
        { mapKey = key, mapValue = value }

let example0 =
      assert : keyValue Natural "foo" 2 ≡ { mapKey = "foo", mapValue = 2 }

let example1 =
      assert : keyValue Text "bar" "baz" ≡ { mapKey = "bar", mapValue = "baz" }

in  keyValue
