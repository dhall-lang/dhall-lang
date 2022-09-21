let Access = ./../Access.dhall

let Bool/not = ./../../Bool/not.dhall

let Optional/map = ./../../Optional/map.dhall

let f
    : Optional Bool -> Optional Bool
    = Optional/map Bool Bool Bool/not

let invert
    : Access -> Access
    = \(a : Access) ->
        { read = f a.read, write = f a.write, execute = f a.execute }

in  invert
