-- This tests that we prefer the type from the selector over the inferred field
-- type from the record
-- ie the result should be `{ a : Natural → Natural }`
--                     not `{ a : ∀(x : Natural) → Natural }`
let e = { a = λ(x : Natural) → x } let s = { a : Natural → Natural } in e.(s)
