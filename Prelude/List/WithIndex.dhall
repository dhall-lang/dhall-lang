--| Tag a type with an index, supporting indexed list operations
let List/WithIndex
    : ∀(a : Type) → Type
    = λ(a : Type) → { index : Natural, value : a }

in  List/WithIndex
