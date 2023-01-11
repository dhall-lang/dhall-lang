{- | @toModeWith x m@ converts some fields of a `Mask` @m@ to a `Mode`: If the
respective field is set in the first `Mask` @x@, then the flag will be set to
the flags value given in @m@. Otherwise, the value set in @m@ is ignored and the
value of the flag in the result is `None`.
-}
let Access = ../../Access/Type.dhall

let Access/Mask = ../../Access/Mask/Type.dhall

let Mode = ../Type.dhall

let Mask = ./Type.dhall

let Access/equal = ../../Access/equal.dhall

let Access/toAccessWith = ../../Access/Mask/toAccessWith.dhall

let f
    : Access/Mask -> Access/Mask -> Optional Access
    = \(set : Access/Mask) ->
      \(m : Access/Mask) ->
        let x = Access/toAccessWith set m

        in  if    Access/equal
                    { execute = None Bool, read = None Bool, write = None Bool }
                    x
            then  None Access
            else  Some x

let toModeWith
    : Mask -> Mask -> Mode
    = \(set : Mask) ->
      \(m : Mask) ->
        { user = f set.user m.user
        , group = f set.group m.group
        , other = f set.other m.other
        }

in  toModeWith
