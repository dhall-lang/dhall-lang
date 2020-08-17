--| Returns `True` if a number is `0` and returns `False` otherwise
let isZero
    : Natural → Bool
    = Natural/isZero

let example0 = assert : isZero 2 ≡ False

let example1 = assert : isZero 0 ≡ True

in  isZero
