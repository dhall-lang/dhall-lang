{-|
Convert an `Integer` to an `Optional Natural`, with negative numbers becoming `None Natural`.
-}
let nonNegative =
        ./nonNegative.dhall
          sha256:b463373f070df6b1c8c7082051e0810fee38b360bab35256187c8c2b6af5c663
      ? ./nonNegative.dhall

let toNatural
    : Integer → Optional Natural
    = λ(n : Integer) →
        if nonNegative n then Some (Integer/clamp n) else None Natural

let example0 = assert : toNatural +7 ≡ Some 7

let example2 = assert : toNatural +0 ≡ Some 0

let example3 = assert : toNatural -3 ≡ None Natural

in  toNatural
