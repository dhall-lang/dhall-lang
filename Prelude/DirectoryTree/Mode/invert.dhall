-- | Inverts each access right set in a mode using Access/invert.

let Access = ./../Access.dhall

let Mode = ./../Mode.dhall

let Access/invert = ./../Access/invert.dhall

let Optional/map = ./../../Optional/map.dhall

let f
    : Optional Access -> Optional Access
    = Optional/map Access Access Access/invert

let invert
    : Mode -> Mode
    = \(m : Mode) -> { user = f m.user, group = f m.group, other = f m.other }

in  invert
