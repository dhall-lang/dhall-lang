--| `min a b` returns the smaller of `a` or `b`
let lessThanEqual =
        ./lessThanEqual.dhall
          sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
      ? ./lessThanEqual.dhall

let min
    : Natural → Natural → Natural
    = λ(a : Natural) → λ(b : Natural) → if lessThanEqual a b then a else b

let example0 = assert : min 1 2 ≡ 1

let example1 = assert : min 2 1 ≡ 1

let property0 = λ(n : Natural) → assert : min n n ≡ n

in  min
