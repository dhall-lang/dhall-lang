let Mode = ./../Mode.dhall

let Mask = ./Mask.dhall

let Access/toAccess = ./../AccessMask/toAccess.dhall

let toMode
    : Mask -> Mode
    = \(m : Mask) ->
        { user = Some (Access/toAccess m.user)
        , group = Some (Access/toAccess m.group)
        , other = Some (Access/toAccess m.other)
        }

in  toMode
