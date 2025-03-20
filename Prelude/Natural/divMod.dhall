--| `divmod b x` computes x / b via integer division with modainder. The result is `{ div = ..., mod = ... }` and it is guaranteed that `x === b * div + mod` as long as `b` is nonzero.
let Natural/lessThan =
        missing
          sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c
      ? ../Natural/lessThan.dhall

let Result = { div : Natural, mod : Natural }

let Optional/default =
        missing
          sha256:5bd665b0d6605c374b3c4a7e2e2bd3b9c1e39323d41441149ed5e30d86e889ad
      ? ../Optional/default.dhall

let powersUntil
    -- `powersUntil b p q` will create a list [1, b, b^2, b^3, ..., b^k] where k is such that q * b ^ k <= p < q * b ^ (k + 1) .
    : Natural → Natural → Natural → List Natural
    = λ(b : Natural) →
      λ(p : Natural) →
      λ(q : Natural) →
        let appendNewPower =
              λ(prev : List Natural) →
                let nextPower =
                      b * Optional/default Natural 0 (List/last Natural prev)

                in  if    Natural/lessThan p (nextPower * q)
                    then  prev
                    else  prev # [ nextPower ]

        in  Natural/fold p (List Natural) appendNewPower [ 1 ]

let Natural/lessThanEqual =
        missing
          sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
      ? ../Natural/lessThanEqual.dhall

let Validate =
        missing
          sha256:ff769464d570cd70b8f461c957d2fe449827e585d5f266abf8ca0689dd4e4373
      ? ../Function/Validate.dhall

let AtLeast1 = Validate Natural (λ(n : Natural) → Natural/lessThanEqual 1 n)

let egyptianDivMod
    : Natural → Natural → Result
    =
      -- This algorithm is uniformly fast for computing a / b even for large a and/or b.
      λ(a : Natural) →
      λ(b : Natural) →
        let powers2 = powersUntil 2 a b

        let update
            : Natural → Result → Result
            = λ(power2 : Natural) →
              λ(prev : Result) →
                if    Natural/lessThan prev.mod (power2 * b)
                then  prev
                else  { div = prev.div + power2
                      , mod = Natural/subtract (power2 * b) prev.mod
                      }

        in  List/fold Natural powers2 Result update { div = 0, mod = a }

let divMod
    : Natural → Natural → Result
    = egyptianDivMod

let _ = assert : divMod 10 1 ≡ { div = 10, mod = 0 }

let _ = assert : divMod 10 10 ≡ { div = 1, mod = 0 }

let _ = assert : divMod 10 11 ≡ { div = 0, mod = 10 }

let _ = assert : divMod 10 2 ≡ { div = 5, mod = 0 }

let _ = assert : divMod 11 2 ≡ { div = 5, mod = 1 }

let _ = assert : divMod 1000000000000 121 ≡ { div = 8264462809, mod = 111 }

let _ =
      assert : divMod 1000000000000 12121212121 ≡ { div = 82, mod = 6060606078 }

let safeDivMod
    : ∀(y : Natural) → AtLeast1 y → Natural → Result
    = λ(y : Natural) → λ(_ : AtLeast1 y) → λ(x : Natural) → divMod x y

let _ = assert : safeDivMod 2 {=} 11 ≡ { div = 5, mod = 1 }

in  safeDivMod
