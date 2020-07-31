--| Returns the last non-empty `Optional` value in a `List`
let last
    : ∀(a : Type) → List (Optional a) → Optional a
    = λ(a : Type) →
      λ(xs : List (Optional a)) →
        List/fold
          (Optional a)
          xs
          (Optional a)
          ( λ(l : Optional a) →
            λ(r : Optional a) →
              merge { Some = λ(x : a) → Some x, None = l } r
          )
          (None a)

let example0 = assert : last Natural [ None Natural, Some 1, Some 2 ] ≡ Some 2

let example1 =
      assert : last Natural [ None Natural, None Natural ] ≡ None Natural

let example2 =
      assert : last Natural ([] : List (Optional Natural)) ≡ None Natural

in  last
