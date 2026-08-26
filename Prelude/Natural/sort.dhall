--| `sort` sorts a `List` of `Natural`s in ascending order
let greaterThanEqual =
        missing
          sha256:33319c5defc5b556a2e0355cdc6369b0138c5d0de787a85c98e35dc307c855e7
      ? ./greaterThanEqual.dhall

let listMin =
        missing
          sha256:de0ffc7476fbfd7084bc314b1a0d394966c2c52d669e0e06b1ce74a0631e2446
      ? ./listMin.dhall

let List/partition =
        missing
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
