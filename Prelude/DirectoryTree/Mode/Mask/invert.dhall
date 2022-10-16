let Mask = ./Type.dhall

let Access/invert = ../../Access/Mask/invert.dhall

-- | Inverts each access right set in a `Mask` using `Access/invert`.
let invert
    : Mask -> Mask
    = \(m : Mask) ->
        { user = Access/invert m.user
        , group = Access/invert m.group
        , other = Access/invert m.other
        }

in  invert
