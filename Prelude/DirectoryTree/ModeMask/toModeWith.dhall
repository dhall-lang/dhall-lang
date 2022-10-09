let Access = ./../Access.dhall

let Access/Mask = ./../AccessMask/Mask.dhall

let Mode = ./../Mode.dhall

let Mask = ./Mask.dhall

let Access/equal = ./../equalAccess.dhall

let Access/toAccessWith = ./../AccessMask/toAccessWith.dhall

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

-- | @toModeWith x m@ converts some fields of a `Mask` @m@ to a `Mode`: If the
-- respective field is set in the first `Mask` @x@, then the flag will be set to
-- the flags value given in @m@. Otherwise, the value set in @m@ is ignored and
-- the value of the flag in the result is `None`.
let toModeWith
    : Mask -> Mask -> Mode
    = \(set : Mask) ->
      \(m : Mask) ->
        { user = f set.user m.user
        , group = f set.group m.group
        , other = f set.other m.other
        }

in  toModeWith
