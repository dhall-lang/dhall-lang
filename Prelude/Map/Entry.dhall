--| The type of each key-value pair in a `Map`
let Entry
    : Type → Type → Type
    = λ(k : Type) → λ(v : Type) → { mapKey : k, mapValue : v }

in  Entry
