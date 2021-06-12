--| Unzip a `NonEmpty` list into two separate `NonEmpty` lists
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let NonEmpty/map =
        ./map.dhall
          sha256:93d53afe874bb2eed946c21ca5ada3c9716b7d00e6d8edfaba6484cd9c5a00bd
      ? ./map.dhall

let unzip
    : ∀(a : Type) →
      ∀(b : Type) →
      NonEmpty { _1 : a, _2 : b } →
        { _1 : NonEmpty a, _2 : NonEmpty b }
    = λ(a : Type) →
      λ(b : Type) →
      λ(xs : NonEmpty { _1 : a, _2 : b }) →
        { _1 =
            NonEmpty/map
              { _1 : a, _2 : b }
              a
              (λ(x : { _1 : a, _2 : b }) → x._1)
              xs
        , _2 =
            NonEmpty/map
              { _1 : a, _2 : b }
              b
              (λ(x : { _1 : a, _2 : b }) → x._2)
              xs
        }

let example0 =
        assert
      :   unzip
            Text
            Bool
            { head = { _1 = "ABC", _2 = True }
            , tail = [ { _1 = "DEF", _2 = False }, { _1 = "GHI", _2 = True } ]
            }
        ≡ { _1 = { head = "ABC", tail = [ "DEF", "GHI" ] }
          , _2 = { head = True, tail = [ False, True ] }
          }

let example1 =
        assert
      :   unzip
            Text
            Bool
            { head = { _1 = "ABC", _2 = True }
            , tail = [] : List { _1 : Text, _2 : Bool }
            }
        ≡ { _1 = { head = "ABC", tail = [] : List Text }
          , _2 = { head = True, tail = [] : List Bool }
          }

in  unzip
