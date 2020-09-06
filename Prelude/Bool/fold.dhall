--| `fold` is essentially the same as `if`/`then`/`else` except as a function
let fold
    : ∀(b : Bool) → ∀(bool : Type) → ∀(true : bool) → ∀(false : bool) → bool
    = λ(b : Bool) →
      λ(bool : Type) →
      λ(true : bool) →
      λ(false : bool) →
        if b then true else false

let example0 = assert : fold True Natural 0 1 ≡ 0

let example1 = assert : fold False Natural 0 1 ≡ 1

in  fold
