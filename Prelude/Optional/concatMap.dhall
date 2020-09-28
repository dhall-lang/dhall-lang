--| Transform the value with a function and flatten the resulting `Optional`
let concatMap
    : ∀(a : Type) → ∀(b : Type) → (a → Optional b) → Optional a → Optional b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → Optional b) →
      λ(o : Optional a) →
        merge { Some = f, None = None b } o

let exampleFun
    : Natural → Optional Natural
    = λ(n : Natural) → if Natural/even n then Some (n + 1) else None Natural

let example0 =
      assert : concatMap Natural Natural exampleFun (Some 1) ≡ None Natural

let example1 = assert : concatMap Natural Natural exampleFun (Some 2) ≡ Some 3

let example2 =
        assert
      : concatMap Natural Natural exampleFun (None Natural) ≡ None Natural

in  concatMap
