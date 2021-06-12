{-|
Returns `True` for `+0` and any positive `Integer`.

`nonNegative` is more efficient than `./greaterThanEqual +0` or `./greaterThan -1`.
-}
let nonPositive =
        ./nonPositive.dhall
          sha256:e00a852eed5b84ff60487097d8aadce53c9e5301f53ff4954044bd68949fac3b
      ? ./nonPositive.dhall

let nonNegative
    : Integer → Bool
    = λ(n : Integer) → nonPositive (Integer/negate n)

let example0 = assert : nonNegative +1 ≡ True

let example1 = assert : nonNegative +0 ≡ True

let example2 = assert : nonNegative -1 ≡ False

in  nonNegative
