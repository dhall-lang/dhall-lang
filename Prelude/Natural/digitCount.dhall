--| `digitsInBase b x` returns the smallest natural number `k` such that `b ^ k > x`. The number `k` is `k = 1 + floor(log(b, x))` and represents the number of digits of `x` in base `b`.
let lessThanEqual =
        missing
          sha256:e3cca9f3942f81fa78a2bea23c0c24519c67cfe438116c38e797e12dcd26f6bc
      ? ./lessThanEqual.dhall

let Validate =
        missing
          sha256:ff769464d570cd70b8f461c957d2fe449827e585d5f266abf8ca0689dd4e4373
      ? ../Function/Validate.dhall

let IsAtLeast1 = Validate Natural (λ(x : Natural) → lessThanEqual 1 x)

let digitsInBase
    : ∀(base : Natural) → IsAtLeast1 base → Natural → Natural
    = λ(base : Natural) →
      λ(_ : IsAtLeast1 base) →
      λ(n : Natural) →
        let Accum = { b : Natural, digitsInBase : Natural }

        let init = { b = 1, digitsInBase = 0 }

        let update =
              λ(acc : Accum) →
                if    lessThanEqual acc.b n
                then  { b = acc.b * base, digitsInBase = acc.digitsInBase + 1 }
                else  acc

        let result
            : Accum
            = Natural/fold n Accum update init

        in  result.digitsInBase

let _ = assert : digitsInBase 10 {=} 15 ≡ 2

let _ = assert : digitsInBase 2 {=} 7 ≡ 3

let _ = assert : digitsInBase 2 {=} 8 ≡ 4

let _ = assert : digitsInBase 2 {=} 9 ≡ 4

let _ = assert : digitsInBase 3 {=} 123123123123123123123123 ≡ 49

let _ = assert : digitsInBase 10 {=} 123123123123123123123123 ≡ 24

in  digitsInBase
