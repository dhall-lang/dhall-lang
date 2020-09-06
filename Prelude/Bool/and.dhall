{-|
The `and` function returns `False` if there are any `False` elements in the
`List` and returns `True` otherwise
-}
let and
    : List Bool → Bool
    = λ(xs : List Bool) →
        List/fold Bool xs Bool (λ(l : Bool) → λ(r : Bool) → l && r) True

let example0 = assert : and [ True, False, True ] ≡ False

let example1 = assert : and ([] : List Bool) ≡ True

in  and
