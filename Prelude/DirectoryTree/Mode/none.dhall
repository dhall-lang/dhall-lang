let Access =
        ../Access/Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../Access/Type.dhall

in    { user = None Access, group = None Access, other = None Access }
    :   ./Type.dhall
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60
      ? ./Type.dhall
