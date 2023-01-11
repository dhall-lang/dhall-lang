{- | Convert a `Mask` value to a `Mode` value.
For a partial conversion see `toModeWith`.
-}
let Mode =
        ../Type.dhall
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60
      ? ../Type.dhall

let Mask =
        ./Type.dhall
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall

let Access/toAccess =
        ../../Access/Mask/toAccess.dhall
          sha256:78fe016f0273b2551c8590da71bf204cc26d6879c6b84622d4d8ad5624328438
      ? ../../Access/Mask/toAccess.dhall

let toMode
    : Mask -> Mode
    = \(m : Mask) ->
        { user = Some (Access/toAccess m.user)
        , group = Some (Access/toAccess m.group)
        , other = Some (Access/toAccess m.other)
        }

in  toMode
