--| `equal` checks if two Integers are equal.
let equal
    : Integer → Integer → Bool
    = λ(a : Integer) →
      λ(b : Integer) →
            Natural/equal (Integer/clamp a) (Integer/clamp b)
        &&  Natural/equal
              (Integer/clamp (Integer/negate a))
              (Integer/clamp (Integer/negate b))

let example0 = assert : equal +5 +5 ≡ True

let example1 = assert : equal +5 +6 ≡ False

let example2 = assert : equal +5 -5 ≡ False

let example3 = assert : equal -5 -5 ≡ True

in  equal
