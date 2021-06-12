{-|
Returns `True` for any `Integer` greater than `+0`.

`positive` is more efficient than `./greaterThan +0` or `./greaterThanEqual +1`.
-}
let not =
        ../Bool/not.dhall
          sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4
      ? ../Bool/not.dhall

let nonPositive =
        ./nonPositive.dhall
          sha256:e00a852eed5b84ff60487097d8aadce53c9e5301f53ff4954044bd68949fac3b
      ? ./nonPositive.dhall

let positive
    : Integer → Bool
    = λ(n : Integer) → not (nonPositive n)

let example0 = assert : positive +1 ≡ True

let example1 = assert : positive +0 ≡ False

let example2 = assert : positive -1 ≡ False

in  positive
