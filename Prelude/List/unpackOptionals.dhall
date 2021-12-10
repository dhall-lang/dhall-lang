--| Unpack Optionals in a List, omitting None items.
let List/filterMap =
        ./filterMap.dhall
          sha256:94b7ed4204d1c79aaf55527ef51024e7085b8dd2896952cffbd12d8f95e16f46
      ? ./filterMap.dhall

let unpackOptionals
    : ∀(a : Type) → ∀(l : List (Optional a)) → List a
    = λ(a : Type) → List/filterMap (Optional a) a (λ(x : Optional a) → x)

let property1 =
      λ(a : Type) → λ(x : a) → assert : unpackOptionals a [ Some x ] ≡ [ x ]

let property2 =
      λ(a : Type) → assert : unpackOptionals a [ None a ] ≡ ([] : List a)

let example0 =
        assert
      : unpackOptionals Natural [ Some 1, None Natural, Some 3 ] ≡ [ 1, 3 ]

let example1 =
        assert
      :   unpackOptionals Natural ([] : List (Optional Natural))
        ≡ ([] : List Natural)

let example2 =
        assert
      :   unpackOptionals Natural [ None Natural, None Natural ]
        ≡ ([] : List Natural)

in  unpackOptionals
