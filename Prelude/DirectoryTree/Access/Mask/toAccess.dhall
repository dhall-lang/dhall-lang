let Access = ../Type.dhall

let Mask = ./Type.dhall

let toAccess
    : Mask -> Access
    = \(m : Mask) ->
        { execute = Some m.execute, read = Some m.read, write = Some m.write }

in  toAccess
