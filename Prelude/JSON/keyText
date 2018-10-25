{-
Builds a key-value record such that a List of them will be converted to a
homogeneous record by dhall-to-json and dhall-to-yaml.
Both key and value are fixed to Text.
Take a look at `Record/keyValue` for a polymorphic version.

Example:

```
./keyText "foo" "bar" = { mapKey = "foo", mapValue = "bar" }
```
-}

    let keyText =
          λ(key : Text) → λ(value : Text) → { mapKey = key, mapValue = value }

in  keyText
