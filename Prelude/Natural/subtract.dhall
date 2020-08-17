--| `subtract m n` computes `n - m`, truncating to `0` if `m > n`
let subtract
    : Natural → Natural → Natural
    = Natural/subtract

let example0 = assert : subtract 1 2 ≡ 1

let example1 = assert : subtract 1 1 ≡ 0

let example2 = assert : subtract 2 1 ≡ 0

let property0 = λ(n : Natural) → assert : subtract 0 n ≡ n

let property1 = λ(n : Natural) → assert : subtract n 0 ≡ 0

let property2 = λ(n : Natural) → assert : subtract n n ≡ 0

in  subtract
