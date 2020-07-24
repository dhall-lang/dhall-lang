--| The identity function simply returns its input
let identity
    : ∀(a : Type) → ∀(x : a) → a
    = λ(a : Type) → λ(x : a) → x

let example0 = assert : identity Natural 1 ≡ 1

let example1 = assert : identity Bool ≡ (λ(x : Bool) → x)

in  identity
