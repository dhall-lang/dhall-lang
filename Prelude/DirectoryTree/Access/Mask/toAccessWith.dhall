{- | @toAccessWith x m@ converts some fields of a `Mask` @m@ to an `Access`
setting: If the respective field is set in the first `Mask` @x@, then the flag
will be set to the flags value given in @m@. Otherwise, the value set in @m@ is
ignored and the value of the flag in the result is `None`.
-}
let Access = ../Type.dhall

let Mask = ./Type.dhall

let toAccessWith
    : Mask -> Mask -> Access
    = \(set : Mask) ->
      \(m : Mask) ->
        { execute = if set.execute then Some m.execute else None Bool
        , read = if set.read then Some m.read else None Bool
        , write = if set.write then Some m.write else None Bool
        }

in  toAccessWith
