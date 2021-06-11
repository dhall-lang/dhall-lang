--| `equal` checks if two Naturals are equal.
let lessThanEqual =
        ./lessThanEqual.dhall
          sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
      ? ./lessThanEqual.dhall

let equal
    : Natural → Natural → Bool
    = λ(a : Natural) → λ(b : Natural) → lessThanEqual a b && lessThanEqual b a

let example0 = assert : equal 5 5 ≡ True

let example1 = assert : equal 5 6 ≡ False

let property0 = λ(n : Natural) → assert : equal n n ≡ True

in  equal
