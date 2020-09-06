--| Transform the value in an `Optional` into `Text`, defaulting `None` to `""`
let defaultMap
    : ∀(a : Type) → (a → Text) → Optional a → Text
    = λ(a : Type) →
      λ(f : a → Text) →
      λ(o : Optional a) →
        merge { Some = f, None = "" } o

let example0 = assert : defaultMap Natural Natural/show (Some 0) ≡ "0"

let example1 = assert : defaultMap Natural Natural/show (None Natural) ≡ ""

in  defaultMap
