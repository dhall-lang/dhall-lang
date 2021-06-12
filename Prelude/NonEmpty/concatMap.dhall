{-|
Transform a `NonEmpty` list by applying a function to each element and
flattening the results
-}
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let NonEmpty/toList =
        ./toList.dhall
          sha256:0977fe14b77232a4451dcf409c43df4589c4b3cdde7b613aab8df183be1b53f5
      ? ./toList.dhall

let List/concatMap =
        ../List/concatMap.dhall
          sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64
      ? ../List/concatMap.dhall

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
