--| Retrieve an element from a `List` using its 0-based index
let drop =
        missing
          sha256:f00263266739d78cc211f9dad876b4b1c6505d97320efdb16a2af429b85d60ae
      ? ./drop.dhall

let index
    : Natural → ∀(a : Type) → List a → Optional a
    = λ(n : Natural) → λ(a : Type) → λ(xs : List a) → List/head a (drop n a xs)

let property =
      λ(n : Natural) → λ(a : Type) → assert : index n a ([] : List a) ≡ None a

let example0 = assert : index 1 Natural [ 2, 3, 5 ] ≡ Some 3

let example1 = assert : index 1 Natural ([] : List Natural) ≡ None Natural

in  index
