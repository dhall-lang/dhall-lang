--| Reverse a `NonEmpty list
let NonEmpty = ./Type.dhall

let List/drop = ../List/drop.dhall

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
