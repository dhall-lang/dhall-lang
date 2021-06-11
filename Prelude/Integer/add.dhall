--| `add m n` computes `m + n`.
let Integer/subtract =
        ./subtract.dhall
          sha256:a34d36272fa8ae4f1ec8b56222fe8dc8a2ec55ec6538b840de0cbe207b006fda
      ? ./subtract.dhall

let add
    : Integer → Integer → Integer
    = λ(m : Integer) → λ(n : Integer) → Integer/subtract (Integer/negate m) n

let example0 = assert : add +3 +5 ≡ +8

let example1 = assert : add -3 -5 ≡ -8

let example2 = assert : add -4 +4 ≡ +0

let example3 = assert : add -3 +5 ≡ +2

let example4 = assert : add +3 -5 ≡ -2

let example5 = assert : add +0 -3 ≡ -3

let example6 = assert : add +0 +3 ≡ +3

in  add
