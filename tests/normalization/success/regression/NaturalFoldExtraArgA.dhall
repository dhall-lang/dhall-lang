{- Checks that builtins don't accidentally discard further arguments than their arity -}
Natural/fold 0 (Bool -> Bool) (λ(_ : (Bool -> Bool)) → λ(_ : Bool) → True) (λ(_ : Bool) → False) True
