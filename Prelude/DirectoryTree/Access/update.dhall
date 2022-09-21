-- | @update a b@ updates a set of access rights @a@ with another set of access
-- rights @b@. That means, if @b@ has a value set for particular flag then that
-- value is taken. If @b@ has no value set then the value of @a@ is taken.
-- As a consequence @update a none == a@.
let Access = ./../Access.dhall

let f
    : Optional Bool -> Optional Bool -> Optional Bool
    = \(o1 : Optional Bool) ->
      \(o2 : Optional Bool) ->
        merge { None = o1, Some = \(_ : Bool) -> o2 } o2

let update
    : Access -> Access -> Access
    = \(a1 : Access) ->
      \(a2 : Access) ->
        { read = f a1.read a2.read
        , write = f a1.write a2.write
        , execute = f a1.execute a2.execute
        }

in  update
