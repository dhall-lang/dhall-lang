let Access = ./Type.dhall

let f
    : Optional Bool -> Optional Bool -> Bool
    = \(o1 : Optional Bool) ->
      \(o2 : Optional Bool) ->
        merge
          { None = merge { None = True, Some = \(b2 : Bool) -> False } o2
          , Some =
              \(b1 : Bool) ->
                merge { None = False, Some = \(b2 : Bool) -> b1 == b2 } o2
          }
          o1

--| `equal` checks if two `Access` settings are equal.
let equal
    : Access -> Access -> Bool
    = \(a1 : Access) ->
      \(a2 : Access) ->
        f a1.execute a2.execute && f a1.read a2.read && f a1.write a2.write

in  equal
