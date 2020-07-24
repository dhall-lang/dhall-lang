--| Transform an `Optional` value with a function
let map
    : ∀(a : Type) → ∀(b : Type) → (a → b) → Optional a → Optional b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → b) →
      λ(o : Optional a) →
        merge { Some = λ(x : a) → Some (f x), None = None b } o

let example0 = assert : map Natural Bool Natural/even (Some 3) ≡ Some False

let example1 = assert : map Natural Bool Natural/even (None Natural) ≡ None Bool

in  map
