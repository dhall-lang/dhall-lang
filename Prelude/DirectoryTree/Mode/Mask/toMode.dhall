let Mode = ../Type.dhall

let Mask = ./Type.dhall

let Access/toAccess = ../../Access/Mask/toAccess.dhall

let toMode
    : Mask -> Mode
    = \(m : Mask) ->
        { user = Some (Access/toAccess m.user)
        , group = Some (Access/toAccess m.group)
        , other = Some (Access/toAccess m.other)
        }

in  toMode
