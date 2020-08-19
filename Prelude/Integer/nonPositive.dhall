{-|
Returns `True` for `+0` and any negative `Integer`.

`nonPositive` is more efficient than `./lessThanEqual +0` or `./lessThan +1`.
-}
let nonPositive
    : Integer → Bool
    = λ(n : Integer) → Natural/isZero (Integer/clamp n)

let example0 = assert : nonPositive +1 ≡ False

let example1 = assert : nonPositive +0 ≡ True

let example2 = assert : nonPositive -1 ≡ True

in  nonPositive
