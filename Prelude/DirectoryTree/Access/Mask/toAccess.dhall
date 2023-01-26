{-|
Convert a `Mask` value to an `Access` value. For a partial conversion see
`toAccessWith`.
-}
let Access =
        ../Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../Type.dhall

let Mask =
        ./Type.dhall
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ./Type.dhall

let toAccess
    : Mask -> Access
    = \(m : Mask) ->
        { execute = Some m.execute, read = Some m.read, write = Some m.write }

in  toAccess
