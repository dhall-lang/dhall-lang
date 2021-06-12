--| `sort` sorts a `List` of `Natural`s in ascending order
let greaterThanEqual =
        ./greaterThanEqual.dhall
          sha256:30ebfab0febd7aa0ccccfdf3dc36ee6d50f0117f35dd4a9b034750b7e885a1a4
      ? ./greaterThanEqual.dhall

let listMin =
        ./listMin.dhall
          sha256:ee70b0d010bbca6012162e8ae1f6e9d9bd10a152675509b0f23145b98b5d43c6
      ? ./listMin.dhall

let List/partition =
        ../List/partition.dhall
          sha256:38147ac6d750a6492736dd90cc967bf09aa405c499de943c64fab7b86ae02f03
      ? ../List/partition.dhall

let Accumulator = { sorted : List Natural, rest : List Natural }

let partitionMinima =
      λ(xs : List Natural) →
        merge
          { Some =
              λ(m : Natural) → List/partition Natural (greaterThanEqual m) xs
          , None = { true = [] : List Natural, false = [] : List Natural }
          }
          (listMin xs)

let test0 =
        assert
      : partitionMinima [ 2, 1, 1, 3 ] ≡ { true = [ 1, 1 ], false = [ 2, 3 ] }

let step =
      λ(x : Accumulator) →
        let p = partitionMinima x.rest

        in  { sorted = x.sorted # p.true, rest = p.false }

let test1 =
        assert
      :   step { sorted = [ 1, 1 ], rest = [ 2, 3 ] }
        ≡ { sorted = [ 1, 1, 2 ], rest = [ 3 ] }

let sort
    : List Natural → List Natural
    = λ(xs : List Natural) →
        let x =
              Natural/fold
                (List/length Natural xs)
                Accumulator
                step
                { sorted = [] : List Natural, rest = xs }

        in  x.sorted

let example0 = assert : sort ([] : List Natural) ≡ ([] : List Natural)

let example1 = assert : sort [ 1 ] ≡ [ 1 ]

let example2 = assert : sort [ 3, 2, 1, 3, 2, 1 ] ≡ [ 1, 1, 2, 2, 3, 3 ]

in  sort
