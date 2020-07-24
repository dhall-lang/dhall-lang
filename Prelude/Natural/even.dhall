--| Returns `True` if a number if even and returns `False` otherwise
let even
    : Natural → Bool
    = Natural/even

let example0 = assert : even 3 ≡ False

let example1 = assert : even 0 ≡ True

in  even
