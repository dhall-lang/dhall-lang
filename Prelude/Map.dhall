{- This is the canonical way to encode a dynamic list of key-value pairs.

   Tools (such as `dhall-to-json`/`dhall-to-yaml` will recognize values of this
   type and convert them to maps/dictionaries/hashes in the target language

   For example, `dhall-to-json` converts a Dhall value like this:

   ```
   [ { mapValue = "foo", mapValue = 1 }
   , { mapValue = "bar", mapValue = 2 }
   ]
   ```

   ... to a JSON value like this:

   ```
   { "foo": 1, "bar", 2 }
   ```
-}
let Map
    : Type → Type → Type
    = λ(k : Type) → λ(v : Type) → List { mapKey : k, mapValue : v }

in  Map
