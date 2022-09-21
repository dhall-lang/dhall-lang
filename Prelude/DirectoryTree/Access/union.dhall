let Access = ./../Access.dhall

let f
    : Optional Bool -> Optional Bool -> Optional Bool
    = \(o1 : Optional Bool) ->
      \(o2 : Optional Bool) ->
        merge
          { None = None Bool
          , Some =
              \(b1 : Bool) ->
                merge
                  { None = None Bool, Some = \(b2 : Bool) -> Some (b1 || b2) }
                  o2
          }
          o1

let union
    : Access -> Access -> Access
    = \(a1 : Access) ->
      \(a2 : Access) ->
        { read = f a1.read a2.read
        , write = f a1.write a2.write
        , execute = f a1.execute a2.execute
        }

in  union
