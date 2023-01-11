{- | Inverts each access right set in a `Mask` using `Access/invert`.
-}
let Mask =
        ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall

let Access/invert =
        ../../Access/Mask/invert.dhall
          sha256:8807fb02f694fe2117f8a7f794e5afbb967ec36eac3405184bf9232c33cdd830
      ? ../../Access/Mask/invert.dhall

let invert
    : Mask -> Mask
    = \(m : Mask) ->
        { user = Access/invert m.user
        , group = Access/invert m.group
        , other = Access/invert m.other
        }

in  invert
