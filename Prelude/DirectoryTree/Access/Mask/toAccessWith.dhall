{-|
@toAccessWith x m@ converts some fields of a `Mask` @m@ to an `Access` setting:
If the respective field is set in the first `Mask` @x@, then the flag will be
set to the flags value given in @m@. Otherwise, the value set in @m@ is ignored
and the value of the flag in the result is `None`.

For a motivation of this function see the documenation on `toModeWith`.
-}
let Access =
        missing
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../Type.dhall

let Mask =
        missing
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

let example0 =
      \(a : Mask) ->
        let Access/none =
                missing
                  sha256:955a2eed689139c811d4b9ef3dd8d0c484392b18c3bb8752c59fd69dbdaf4881
              ? ../none.dhall

        let none =
                missing
                  sha256:db6c3bb734bb3288441f2664379706052943eaba35c021326a600d41ca766925
              ? ./none.dhall

        in  assert : toAccessWith none a === Access/none

in  toAccessWith
