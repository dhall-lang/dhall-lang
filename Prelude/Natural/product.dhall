--| Multiply all the numbers in a `List`
let product
    : List Natural → Natural
    = λ(xs : List Natural) →
        List/fold Natural xs Natural (λ(l : Natural) → λ(r : Natural) → l * r) 1

let example0 = assert : product [ 2, 3, 5 ] ≡ 30

let example1 = assert : product ([] : List Natural) ≡ 1

in  product
