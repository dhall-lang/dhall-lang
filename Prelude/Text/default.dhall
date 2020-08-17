--| Unwrap an `Optional` `Text` value, defaulting `None` to `""`
let default
    : Optional Text → Text
    = λ(o : Optional Text) → merge { Some = λ(t : Text) → t, None = "" } o

let example0 = assert : default (Some "ABC") ≡ "ABC"

let example1 = assert : default (None Text) ≡ ""

in  default
