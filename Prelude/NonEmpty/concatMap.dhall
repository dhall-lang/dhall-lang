{-|
Transform a `NonEmpty` list by applying a function to each element and
flattening the results
-}
let NonEmpty = ./Type.dhall

let NonEmpty/toList = ./toList.dhall

let List/concatMap = ../List/concatMap.dhall

let concatMap
    : ∀(a : Type) → ∀(b : Type) → (a → NonEmpty b) → NonEmpty a → NonEmpty b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → NonEmpty b) →
      λ(xs : NonEmpty a) →
        let ys = f xs.head

        in  { head = ys.head
            , tail =
                  ys.tail
                # List/concatMap
                    a
                    b
                    (λ(x : a) → NonEmpty/toList b (f x))
                    xs.tail
            }

let example0 =
        assert
      :   concatMap
            Natural
            Natural
            (λ(n : Natural) → { head = n, tail = [ n ] })
            { head = 2, tail = [ 3, 5 ] }
        ≡ { head = 2, tail = [ 2, 3, 3, 5, 5 ] }

let example1 =
        assert
      :   concatMap
            Natural
            Natural
            (λ(n : Natural) → { head = n, tail = [ n ] })
            { head = 2, tail = [] : List Natural }
        ≡ { head = 2, tail = [ 2 ] }

in  concatMap
