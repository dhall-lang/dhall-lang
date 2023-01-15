{-| Permissions with no flags set: ---------
-}
let none =
        ../../Access/Mask/none.dhall
          sha256:db6c3bb734bb3288441f2664379706052943eaba35c021326a600d41ca766925
      ? ../../Access/Mask/none.dhall

in    { user = none, group = none, other = none }
    :   ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall
