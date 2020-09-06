{-|
Returns `True` if there are an even number of `False` elements in the list and
returns `False` otherwise.

This function is the `Monoid` for the `==` operation.
-}
let even
    : List Bool → Bool
    = λ(xs : List Bool) →
        List/fold Bool xs Bool (λ(x : Bool) → λ(y : Bool) → x == y) True

let example0 = assert : even [ False, True, False ] ≡ True

let example1 = assert : even [ False, True ] ≡ False

let example2 = assert : even [ False ] ≡ False

let example3 = assert : even ([] : List Bool) ≡ True

in  even
