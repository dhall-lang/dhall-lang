--| Returns `True` if a number is odd and returns `False` otherwise
let divMod =
        missing
          sha256:5b354b1872d2cb3a4a85cc0a56508e2aec7716ae8976891d248acf4648601115
      ? ./divMod.dhall

let odd
    : Natural → Bool
    = λ(n : Natural) → Natural/isZero (Natural/subtract (divMod 2 {=} n).mod 1)

let example0 = assert : odd 3 ≡ True

let example1 = assert : odd 0 ≡ False

in  odd
