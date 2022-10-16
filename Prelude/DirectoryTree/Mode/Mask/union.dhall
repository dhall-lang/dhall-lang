let Mask = ./Type.dhall

let Access/union = ../../Access/Mask/union.dhall

-- | Calculates the union of the access rights set by both `Mask`s using
-- `Access/union`.
let union
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { user = Access/union m1.user m2.user
        , group = Access/union m1.group m2.group
        , other = Access/union m1.other m2.other
        }

in  union
