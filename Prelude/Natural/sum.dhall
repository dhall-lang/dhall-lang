--| Add all the numbers in a `List`
let sum
    : List Natural → Natural
    = λ(xs : List Natural) →
        List/fold Natural xs Natural (λ(l : Natural) → λ(r : Natural) → l + r) 0

let example = assert : sum [ 2, 3, 5 ] ≡ 10

let example = assert : sum ([] : List Natural) ≡ 0

in  sum
