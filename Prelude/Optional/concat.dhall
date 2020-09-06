--| Flatten two `Optional` layers into a single `Optional` layer
let concat
    : ∀(a : Type) → Optional (Optional a) → Optional a
    = λ(a : Type) →
      λ(x : Optional (Optional a)) →
        merge { Some = λ(y : Optional a) → y, None = None a } x

let example0 = assert : concat Natural (Some (Some 1)) ≡ Some 1

let example1 = assert : concat Natural (Some (None Natural)) ≡ None Natural

let example2 = assert : concat Natural (None (Optional Natural)) ≡ None Natural

in  concat
