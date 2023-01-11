let Access =
        ../Access/Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../Access/Type.dhall

in  { user : Optional Access, group : Optional Access, other : Optional Access }
