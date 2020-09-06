{-|
Builds a key-value record such that a `List` of them will be converted to a
homogeneous record by dhall-to-json and dhall-to-yaml.

Both key and value are fixed to `Text`.

Take a look at `./keyValue` for a polymorphic version.
-}
let keyText =
      λ(key : Text) → λ(value : Text) → { mapKey = key, mapValue = value }

let example0 =
      assert : keyText "foo" "bar" ≡ { mapKey = "foo", mapValue = "bar" }

in  keyText
