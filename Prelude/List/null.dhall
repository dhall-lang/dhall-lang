--| Returns `True` if the `List` is empty and `False` otherwise
let null
    : ∀(a : Type) → List a → Bool
    = λ(a : Type) → λ(xs : List a) → Natural/isZero (List/length a xs)

let example0 = assert : null Natural [ 0, 1, 2 ] ≡ False

let example1 = assert : null Natural ([] : List Natural) ≡ True

in  null
