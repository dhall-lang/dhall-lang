--| Flip the value of a `Bool`
let not
    : Bool → Bool
    = λ(b : Bool) → b == False

let example0 = assert : not True ≡ False

let example1 = assert : not False ≡ True

in  not
