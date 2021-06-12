--| Unpack Optionals in a List, omitting None items.
let List/concatMap =
        ./concatMap.dhall
          sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64
      ? ./concatMap.dhall

let Optional/toList =
        ../Optional/toList.dhall
          sha256:d78f160c619119ef12389e48a629ce293d69f7624c8d016b7a4767ab400344c4
      ? ../Optional/toList.dhall

let unpackOptionals
    : ∀(a : Type) → ∀(l : List (Optional a)) → List a
    = λ(a : Type) → List/concatMap (Optional a) a (Optional/toList a)

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
