-- | @update a b@ updates a mode @a@ with mode @b@. That means:
--  * If both @a@ and @b@ have values @a'@ and @b'@ set then the result is @Access/update a' b'@.
--  * If a value for @a@ is set but not for @b@, then the value of @a@ is the one of the result.
--  * If a value for @b@ is set but not for @a@, then the value of @b@ is the one of the result.
-- As a consequence @update a none == a@.
let Access = ./../Access.dhall

let Mode = ./../Mode.dhall

let Access/update = ./../Access/update.dhall

let f
    : Optional Access -> Optional Access -> Optional Access
    = \(o1 : Optional Access) ->
      \(o2 : Optional Access) ->
        merge
          { None = o1
          , Some =
              \(a2 : Access) ->
                merge
                  { None = o2
                  , Some = \(a1 : Access) -> Some (Access/update a1 a2)
                  }
                  o1
          }
          o2

let update
    : Mode -> Mode -> Mode
    = \(m1 : Mode) ->
      \(m2 : Mode) ->
        { user = f m1.user m2.user
        , group = f m1.group m2.group
        , other = f m1.other m2.other
        }

in  update
