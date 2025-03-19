{-|
Render a `Natural` number as `Text` in hexadecimal base.
-}
let Validate =
        missing
          sha256:ff769464d570cd70b8f461c957d2fe449827e585d5f266abf8ca0689dd4e4373
      ? ../Function/Validate.dhall

let List/index =
        missing
          sha256:e657b55ecae4d899465c3032cb1a64c6aa6dc2aa3034204f3c15ce5c96c03e63
      ? ../List/index.dhall

let Optional/default =
        missing
          sha256:5bd665b0d6605c374b3c4a7e2e2bd3b9c1e39323d41441149ed5e30d86e889ad
      ? ../Optional/default

let Natural/lessThan =
        missing
          sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c
      ? ../Natural/lessThan.dhall

let Natural/lessThanEqual =
        missing
          sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
      ? ../Natural/lessThanEqual.dhall

let nonexpanding =
        missing
          sha256:442e6ba8da95ba802b995f4adb91c05bffe29b92c962f1fcd1e13f39c99c7ed3
      ? ../Function/nonexpanding.dhall

let nonexpandingPredicate =
        missing
          sha256:8c843e0f81446254797f7806a8a7764683867bce9589c4fc419e846e7c3e3ea8
      ? ../Function/nonexpandingPredicate.dhall

let Result = { div : Natural, mod : Natural }

let unsafeDivModNotForSmallY
    : Natural → Natural → Result
    =
      -- Warning: This function computes x / y. But it is unsafe (gives wrong results when y = 0)
      -- and also quite slow when y is much smaller than x.
      -- For example, `unsafeDivModNotForSmallY 1000000000000 121` takes a very long time.
      λ(x : Natural) →
      λ(y : Natural) →
        let init
            : Result
            = { div = 0, mod = x }

        let update
            : Result → Result
            =
              -- Loop invariant: x === div * y + mod
              λ(acc : Result) →
                if    Natural/lessThan acc.mod y
                then  acc
                else  { div = acc.div + 1, mod = Natural/subtract y acc.mod }

        in  Natural/fold x Result update init

let _ =
        assert
      : unsafeDivModNotForSmallY 100000000 111111 ≡ { div = 900, mod = 100 }

let _ =
        assert
      :   unsafeDivModNotForSmallY 1000000000000 12121212121
        ≡ { div = 82, mod = 6060606078 }

let indexTextStop1
    : Natural → List Text → Optional Text
    = nonexpanding
        Natural
        nonexpandingPredicate.Natural
        (List Text → Optional Text)
        (λ(_ : List Text) → None Text)
        (λ(i : Natural) → λ(digits : List Text) → List/index i Text digits)

let digits =
      [ "0"
      , "1"
      , "2"
      , "3"
      , "4"
      , "5"
      , "6"
      , "7"
      , "8"
      , "9"
      , "A"
      , "B"
      , "C"
      , "D"
      , "E"
      , "F"
      , "G"
      , "H"
      , "I"
      , "J"
      , "K"
      , "L"
      , "M"
      , "N"
      , "O"
      , "P"
      , "Q"
      , "R"
      , "S"
      , "T"
      , "U"
      , "V"
      , "W"
      , "X"
      , "Y"
      , "Z"
      ]

let lookupDigit =
    -- This will return "?" when the digit index is out of bounds. This should not happen because we validate the `base` value up front.
      λ(d : Natural) → Optional/default Text "?" (indexTextStop1 d digits)

let maxBase = List/length Text digits

let BaseWithinLimits =
      Validate
        Natural
        ( λ(n : Natural) →
            Natural/lessThanEqual 1 n && Natural/lessThanEqual n maxBase
        )

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

let digitsAsText
    : ∀(base : Natural) → Natural → Text
    = λ(base : Natural) →
      λ(n : Natural) →
        let basePowers = powersUntil base n 1

        let Accum = { digitsSoFar : Text, remainder : Natural }

        let init
            : Accum
            = { digitsSoFar = "", remainder = n }

        let update
            : Natural → Accum → Accum
            = λ(p : Natural) →
              λ(acc : Accum) →
                let divmodResult = unsafeDivModNotForSmallY acc.remainder p

                in  { digitsSoFar =
                        acc.digitsSoFar ++ lookupDigit divmodResult.div
                    , remainder = divmodResult.mod
                    }

        in  (List/fold Natural basePowers Accum update init).digitsSoFar

let showInBase
    : ∀(base : Natural) → BaseWithinLimits base → Natural → Text
    = λ(base : Natural) →
      λ(_ : BaseWithinLimits base) →
      λ(n : Natural) →
        if    Natural/isZero n
        then  "0"
        else  if Natural/lessThanEqual base 1
        then  Natural/fold n Text (λ(t : Text) → t ++ "1") ""
        else  digitsAsText base n

let _ = assert : showInBase 4 {=} 3 ≡ "3"

let _ = assert : showInBase 16 {=} 0 ≡ "0"

let _ = assert : showInBase 16 {=} 5 ≡ "5"

let _ = assert : showInBase 16 {=} 15 ≡ "F"

let _ = assert : showInBase 16 {=} 16 ≡ "10"

let _ = assert : showInBase 25 {=} 12345 ≡ "JIK"

let _ = assert : showInBase 1 {=} 5 ≡ "11111"

in  showInBase
