{-|
Zip two `NonEmpty` lists into a single `NonEmpty`

The resulting `NonEmpty` will have the length of the shortest of its arguments.
-}
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let List/zip =
        ../List/zip.dhall
          sha256:85ed955eabf3998767f4ad2a28e57d40cd4c68a95519d79e9b622f1d26d979da
      ? ../List/zip.dhall

let zip
    : ∀(a : Type) →
      NonEmpty a →
      ∀(b : Type) →
      NonEmpty b →
        NonEmpty { _1 : a, _2 : b }
    = λ(a : Type) →
      λ(xs : NonEmpty a) →
      λ(b : Type) →
      λ(ys : NonEmpty b) →
        { head = { _1 = xs.head, _2 = ys.head }
        , tail = List/zip a xs.tail b ys.tail
        }

let example0 =
        assert
      :   zip
            Text
            { head = "ABC", tail = [ "DEF", "GHI" ] }
            Natural
            { head = 1, tail = [ 2, 3 ] }
        ≡ { head = { _1 = "ABC", _2 = 1 }
          , tail = [ { _1 = "DEF", _2 = 2 }, { _1 = "GHI", _2 = 3 } ]
          }

let example1 =
        assert
      :   zip
            Text
            { head = "ABC", tail = [ "DEF" ] }
            Bool
            { head = True, tail = [] : List Bool }
        ≡ { head = { _1 = "ABC", _2 = True }
          , tail = [] : List { _1 : Text, _2 : Bool }
          }

let example2 =
        assert
      :   zip
            Text
            { head = "ABC", tail = [] : List Text }
            Natural
            { head = 1, tail = [ 2 ] }
        ≡ { head = { _1 = "ABC", _2 = 1 }
          , tail = [] : List { _1 : Text, _2 : Natural }
          }

in  zip
