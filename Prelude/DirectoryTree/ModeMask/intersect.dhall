let Mask = ./Mask.dhall

let Access/intersect = ../AccessMask/intersect.dhall

-- | Calculates the intersection of the access rights of two `Mask`s using
-- `Access/intersect`.
let intersect
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { user = Access/intersect m1.user m2.user
        , group = Access/intersect m1.group m2.group
        , other = Access/intersect m1.other m2.other
        }

in  intersect
