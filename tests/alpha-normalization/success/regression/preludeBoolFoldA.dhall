-- For https://github.com/dhall-lang/dhall-haskell/issues/929
  λ(b : Bool)
→ λ(bool : Type)
→ λ(true : bool)
→ λ(false : bool)
→ if b then true else false
