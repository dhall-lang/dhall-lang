{-|
The `or` function returns `True` if there are any `True` elements in the `List`
and returns `False` otherwise
-}
let or
    : List Bool → Bool
    = λ(xs : List Bool) →
        List/fold Bool xs Bool (λ(l : Bool) → λ(r : Bool) → l || r) False

let example0 = assert : or [ True, False, True ] ≡ True

let example1 = assert : or ([] : List Bool) ≡ False

in  or
