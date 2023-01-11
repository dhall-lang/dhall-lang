{- | `equal` checks if two `Mode`s are equal.
-}
let Access = ../Access/Type.dhall

let Mode = ./Type.dhall

let Access/equal = ../Access/equal.dhall

let f
    : Optional Access -> Optional Access -> Bool
    = \(o1 : Optional Access) ->
      \(o2 : Optional Access) ->
        merge
          { None = merge { None = True, Some = \(a2 : Access) -> False } o2
          , Some =
              \(a1 : Access) ->
                merge
                  { None = False, Some = \(a2 : Access) -> Access/equal a1 a2 }
                  o2
          }
          o1

let equal
    : Mode -> Mode -> Bool
    = \(m1 : Mode) ->
      \(m2 : Mode) ->
        f m1.user m2.user && f m1.group m2.group && f m1.other m2.other

in  equal
