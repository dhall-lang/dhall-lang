{-|
`listMax` returns the largest element of a `List` or `None Natural` if the
`List` is empty
-}
let max =
        ./max.dhall
          sha256:1f3b18da330223ab039fad11693da72c7e68d516f50502c73f41a89a097b62f7
      ? ./max.dhall

let Optional/map =
        ../Optional/map.dhall
          sha256:501534192d988218d43261c299cc1d1e0b13d25df388937add784778ab0054fa
      ? ../Optional/map.dhall

let listMax
    : List Natural → Optional Natural
    = λ(xs : List Natural) →
        Optional/map
          Natural
          Natural
          (λ(n : Natural) → List/fold Natural xs Natural max n)
          (List/head Natural xs)

let example0 = assert : listMax [ 1, 2 ] ≡ Some 2

let example1 = assert : listMax ([] : List Natural) ≡ None Natural

let property0 = λ(n : Natural) → assert : listMax [ n ] ≡ Some n

in  listMax
