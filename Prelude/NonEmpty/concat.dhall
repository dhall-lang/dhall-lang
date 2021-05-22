{-|
Concatenate a `NonEmpty` list of `NonEmpty` lists into a single `NonEmpty`
list
-}
let NonEmpty = ./Type.dhall

let NonEmpty/toList = ./toList.dhall

let List/concatMap = ../List/concatMap.dhall

let concat
    : ∀(a : Type) → NonEmpty (NonEmpty a) → NonEmpty a
    = λ(a : Type) →
      λ(xss : NonEmpty (NonEmpty a)) →
        { head = xss.head.head
        , tail =
              xss.head.tail
            # List/concatMap (NonEmpty a) a (NonEmpty/toList a) xss.tail
        }

let example0 =
        assert
      :   concat
            Natural
            { head = { head = 0, tail = [ 1, 2 ] }
            , tail =
              [ { head = 3, tail = [ 4 ] }, { head = 5, tail = [ 6, 7, 8 ] } ]
            }
        ≡ { head = 0, tail = [ 1, 2, 3, 4, 5, 6, 7, 8 ] }

let example1 =
        assert
      :   concat
            Natural
            { head = { head = 0, tail = [] : List Natural }
            , tail =
              [ { head = 1, tail = [] : List Natural }
              , { head = 2, tail = [] : List Natural }
              ]
            }
        ≡ { head = 0, tail = [ 1, 2 ] : List Natural }

let example2 =
        assert
      :   concat
            Natural
            { head = { head = 0, tail = [] : List Natural }
            , tail = [] : List (NonEmpty Natural)
            }
        ≡ { head = 0, tail = [] : List Natural }

in  concat
