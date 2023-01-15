{-| Calculates the intersection of the access rights of two `Mask`s using
`Access/intersect`.
-}
let Mask =
        ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall

let Access/intersect =
        ../../Access/Mask/intersect.dhall
          sha256:7c7dab3e305f43cba556e0778b5a797cf8e9b1d1a6c6f6fe0ea311c329919663
      ? ../../Access/Mask/intersect.dhall

let intersect
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { user = Access/intersect m1.user m2.user
        , group = Access/intersect m1.group m2.group
        , other = Access/intersect m1.other m2.other
        }

in  intersect
