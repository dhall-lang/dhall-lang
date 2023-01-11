{- | @toAccessWith x m@ converts some fields of a `Mask` @m@ to an `Access`
setting: If the respective field is set in the first `Mask` @x@, then the flag
will be set to the flags value given in @m@. Otherwise, the value set in @m@ is
ignored and the value of the flag in the result is `None`.
-}
let Access =
        ../Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../Type.dhall

let Mask =
        ./Type.dhall
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ./Type.dhall

let toAccessWith
    : Mask -> Mask -> Access
    = \(set : Mask) ->
      \(m : Mask) ->
        { execute = if set.execute then Some m.execute else None Bool
        , read = if set.read then Some m.read else None Bool
        , write = if set.write then Some m.write else None Bool
        }

in  toAccessWith
