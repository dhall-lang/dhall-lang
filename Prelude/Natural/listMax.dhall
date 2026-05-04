{-|
`listMax` returns the largest element of a `List` or `None Natural` if the
`List` is empty
-}
let max =
        missing
          sha256:6c4f967eb6202fff3ec1985f1112ae04b11ff72357f1d10a7c810d826ee1d68b
      ? ./max.dhall

let Optional/map =
        missing
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
