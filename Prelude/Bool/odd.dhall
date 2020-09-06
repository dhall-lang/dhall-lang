{-|
Returns `True` if there are an odd number of `True` elements in the list and
returns `False` otherwise.

This function is the `Monoid` for the `!=` operation.
-}
let odd
    : List Bool → Bool
    = λ(xs : List Bool) →
        List/fold Bool xs Bool (λ(x : Bool) → λ(y : Bool) → x != y) False

let example0 = assert : odd [ True, False, True ] ≡ False

let example1 = assert : odd [ True, False ] ≡ True

let example2 = assert : odd [ True ] ≡ True

let example3 = assert : odd ([] : List Bool) ≡ False

in  odd
