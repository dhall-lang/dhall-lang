--| Returns `True` if a number if even and returns `False` otherwise
let divMod =
        missing
          sha256:5b354b1872d2cb3a4a85cc0a56508e2aec7716ae8976891d248acf4648601115
      ? ./divMod.dhall

let even
    : Natural → Bool
    = λ(n : Natural) → Natural/isZero (divMod 2 {=} n).mod

let example0 = assert : even 3 ≡ False

let example1 = assert : even 0 ≡ True

in  even
