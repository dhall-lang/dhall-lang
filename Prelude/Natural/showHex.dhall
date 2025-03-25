{-|
Render a `Natural` number as `Text` in hexadecimal base.
-}
let digitsInBase =
        missing
          sha256:33c39ad3ae7467e9b27718e0f678723e62f4803c24fbaa552cf8a0c0481b57c7
      ? ./digitsInBase.dhall

let List/index =
        missing
          sha256:e657b55ecae4d899465c3032cb1a64c6aa6dc2aa3034204f3c15ce5c96c03e63
      ? ../List/index.dhall

let Optional/default =
        missing
          sha256:5bd665b0d6605c374b3c4a7e2e2bd3b9c1e39323d41441149ed5e30d86e889ad
      ? ../Optional/default

let nonexpanding =
        missing
          sha256:442e6ba8da95ba802b995f4adb91c05bffe29b92c962f1fcd1e13f39c99c7ed3
      ? ../Function/nonexpanding.dhall

let nonexpandingPredicate =
        missing
          sha256:8c843e0f81446254797f7806a8a7764683867bce9589c4fc419e846e7c3e3ea8
      ? ../Function/nonexpandingPredicate.dhall

let indexTextStop1
    : Natural → List Text → Optional Text
    =
      -- It is important to apply `nonexpanding` to List/index, or else the normal form of `showHex` blows up to 60+ MB.
      nonexpanding
        Natural
        nonexpandingPredicate.Natural
        (List Text → Optional Text)
        (λ(_ : List Text) → None Text)
        (λ(i : Natural) → λ(digits : List Text) → List/index i Text digits)

let Text/concatMap =
        missing
          sha256:7a0b0b99643de69d6f94ba49441cd0fa0507cbdfa8ace0295f16097af37e226f
      ? ../Text/concatMap.dhall

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
      ]

let lookupDigit
    : Natural → Text
    =
      -- This will return "?" when the digit index is out of bounds. This should not happen because the `base = 16` is hard-coded.
      λ(d : Natural) → Optional/default Text "?" (indexTextStop1 d digits)

let showHex
    : Natural → Text
    = λ(n : Natural) →
        let digits = digitsInBase 16 n

        in  "0x" ++ Text/concatMap Natural lookupDigit digits

let _ = assert : showHex 0 ≡ "0x0"

let _ = assert : showHex 1 ≡ "0x1"

let _ = assert : showHex 10 ≡ "0xA"

let _ = assert : showHex 16 ≡ "0x10"

let _ = assert : showHex 64 ≡ "0x40"

let _ = assert : showHex 1023 ≡ "0x3FF"

let _ = assert : showHex 1025 ≡ "0x401"

let _ = assert : showHex 12345 ≡ "0x3039"

in  showHex
