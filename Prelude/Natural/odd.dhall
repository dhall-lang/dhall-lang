--| Returns `True` if a number is odd and returns `False` otherwise
let odd
    : Natural → Bool
    = Natural/odd

let example0 = assert : odd 3 ≡ True

let example1 = assert : odd 0 ≡ False

in  odd
