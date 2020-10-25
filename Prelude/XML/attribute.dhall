--| Builds a key-value record with a Text key and value.
let attribute
    : Text → Text → { mapKey : Text, mapValue : Text }
    = λ(key : Text) → λ(value : Text) → { mapKey = key, mapValue = value }

in  attribute
