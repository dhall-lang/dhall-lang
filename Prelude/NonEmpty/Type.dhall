{-|
A `NonEmpty` list has at least one element and supports many of the same
operations as `List`s
-}
let NonEmpty
    : Type → Type
    = λ(a : Type) → { head : a, tail : List a }

in  NonEmpty
