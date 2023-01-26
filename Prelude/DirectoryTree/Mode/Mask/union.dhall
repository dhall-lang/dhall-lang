{-|
Calculates the union of the access rights set by both `Mask`s using
`Access/union`.
-}
let Mask =
        ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall

let Access/union =
        ../../Access/Mask/union.dhall
          sha256:b40c4cbb266991e3f764af075e9db544b59c16b3d9aa680c0cf6cb7552da191f
      ? ../../Access/Mask/union.dhall

let union
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { user = Access/union m1.user m2.user
        , group = Access/union m1.group m2.group
        , other = Access/union m1.other m2.other
        }

in  union
