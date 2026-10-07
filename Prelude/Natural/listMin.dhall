{-|
`listMin` returns the smallest element of a `List` or `None Natural` if the
`List` is empty
-}
let min =
        missing
          sha256:5edc58a27db590a8ef6561c3f0032f75468df637476520e4348308f4ccf7c010
      ? ./min.dhall

let Optional/map =
        missing
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
