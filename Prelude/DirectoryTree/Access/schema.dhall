{- | A schema for blank access rights.
-}
{ Type =
      ./Type.dhall
        sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
    ? ./Type.dhall
, default = { read = None Bool, write = None Bool, execute = None Bool }
}
