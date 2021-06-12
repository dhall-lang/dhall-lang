--| `max a b` returns the larger of `a` or `b`
let lessThanEqual =
        ./lessThanEqual.dhall
          sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
      ? ./lessThanEqual.dhall

let max
    : Natural → Natural → Natural
    = λ(a : Natural) → λ(b : Natural) → if lessThanEqual a b then b else a

let example0 = assert : max 1 2 ≡ 2

let example1 = assert : max 2 1 ≡ 2

let property0 = λ(n : Natural) → assert : max n n ≡ n

let property1 = λ(n : Natural) → assert : max 0 n ≡ n

in  max
