--| `fold` is the primitive function for consuming `Optional` values
let fold
    : ∀(a : Type) →
      Optional a →
      ∀(optional : Type) →
      ∀(some : a → optional) →
      ∀(none : optional) →
        optional
    = λ(a : Type) →
      λ(o : Optional a) →
      λ(optional : Type) →
      λ(some : a → optional) →
      λ(none : optional) →
        merge { Some = some, None = none } o

let example0 = assert : fold Natural (Some 2) Text Natural/show "0" ≡ "2"

let example1 = assert : fold Natural (None Natural) Text Natural/show "0" ≡ "0"

in  fold
