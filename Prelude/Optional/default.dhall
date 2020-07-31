--| Unpack an `Optional`, returning the default when it's `None`.
let default
    : ∀(a : Type) → a → Optional a → a
    = λ(a : Type) →
      λ(default : a) →
      λ(o : Optional a) →
        merge { Some = λ(x : a) → x, None = default } o

let example0 = assert : default Bool False (None Bool) ≡ False

let example1 = assert : default Bool False (Some True) ≡ True

in  default
