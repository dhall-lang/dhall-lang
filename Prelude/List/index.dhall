--| Retrieve an element from a `List` using its 0-based index
let drop =
        ./drop.dhall
          sha256:af983ba3ead494dd72beed05c0f3a17c36a4244adedf7ced502c6512196ed0cf
      ? ./drop.dhall

let index
    : Natural → ∀(a : Type) → List a → Optional a
    = λ(n : Natural) → λ(a : Type) → λ(xs : List a) → List/head a (drop n a xs)

let property =
      λ(n : Natural) → λ(a : Type) → assert : index n a ([] : List a) ≡ None a

let example0 = assert : index 1 Natural [ 2, 3, 5 ] ≡ Some 3

let example1 = assert : index 1 Natural ([] : List Natural) ≡ None Natural

in  index
