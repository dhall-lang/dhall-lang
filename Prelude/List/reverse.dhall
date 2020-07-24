--| Reverse a list
let reverse
    : ∀(a : Type) → List a → List a
    = List/reverse

let example0 = assert : reverse Natural [ 0, 1, 2 ] ≡ [ 2, 1, 0 ]

let example1 =
      assert : reverse Natural ([] : List Natural) ≡ ([] : List Natural)

in  reverse
