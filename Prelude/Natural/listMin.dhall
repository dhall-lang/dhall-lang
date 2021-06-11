{-|
`listMin` returns the smallest element of a `List` or `None Natural` if the
`List` is empty
-}
let min =
        ./min.dhall
          sha256:f25f9c462e4dbf0eb15f9ff6ac840c6e9c82255a7f4f2ab408bdab338e028710
      ? ./min.dhall

let Optional/map =
        ../Optional/map.dhall
          sha256:501534192d988218d43261c299cc1d1e0b13d25df388937add784778ab0054fa
      ? ../Optional/map.dhall

let listMin
    : List Natural → Optional Natural
    = λ(xs : List Natural) →
        Optional/map
          Natural
          Natural
          ( λ(n : Natural) →
              if Natural/isZero n then n else List/fold Natural xs Natural min n
          )
          (List/head Natural xs)

let example0 = assert : listMin [ 0, 1 ] ≡ Some 0

let example1 = assert : listMin ([] : List Natural) ≡ None Natural

let example2 = assert : listMin [ 3, 2, 1 ] ≡ Some 1

let property0 = λ(n : Natural) → assert : listMin [ n ] ≡ Some n

in  listMin
