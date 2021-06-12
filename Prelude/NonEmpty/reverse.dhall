--| Reverse a `NonEmpty` list
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let List/drop =
        ../List/drop.dhall
          sha256:af983ba3ead494dd72beed05c0f3a17c36a4244adedf7ced502c6512196ed0cf
      ? ../List/drop.dhall

let reverse
    : ∀(a : Type) → NonEmpty a → NonEmpty a
    = λ(a : Type) →
      λ(xs : NonEmpty a) →
        let ys = List/reverse a xs.tail

        in  merge
              { Some =
                  λ(y : a) → { head = y, tail = List/drop 1 a ys # [ xs.head ] }
              , None = { head = xs.head, tail = [] : List a }
              }
              (List/head a ys)

let example =
        assert
      :   reverse Natural { head = 0, tail = [ 1, 2 ] }
        ≡ { head = 2, tail = [ 1, 0 ] }

let example1 =
        assert
      :   reverse Natural { head = 0, tail = [] : List Natural }
        ≡ { head = 0, tail = [] : List Natural }

in  reverse
