--| Tag each element of the `NonEmpty` list with its index
let NonEmpty = ./Type.dhall

let List/map = ../List/map.dhall

let indexed
    : ∀(a : Type) → NonEmpty a → NonEmpty { index : Natural, value : a }
    = λ(a : Type) →
      λ(xs : NonEmpty a) →
        { head = { index = 0, value = xs.head }
        , tail =
            List/map
              { index : Natural, value : a }
              { index : Natural, value : a }
              ( λ(ix : { index : Natural, value : a }) →
                  { index = ix.index + 1, value = ix.value }
              )
              (List/indexed a xs.tail)
        }

let example0 =
        assert
      :   indexed Bool { head = True, tail = [ False, True ] }
        ≡ { head = { index = 0, value = True }
          , tail = [ { index = 1, value = False }, { index = 2, value = True } ]
          }

let example1 =
        assert
      :   indexed Bool { head = True, tail = [] : List Bool }
        ≡ { head = { index = 0, value = True }
          , tail = [] : List { index : Natural, value : Bool }
          }

in  indexed
