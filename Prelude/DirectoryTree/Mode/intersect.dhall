-- | Calculates the intersection of the access rights of two modes using Access/intersect.

let Access = ./../Access.dhall

let Mode = ./../Mode.dhall

let Access/intersect = ./../Access/intersect.dhall

let f
    : Optional Access -> Optional Access -> Optional Access
    = \(o1 : Optional Access) ->
      \(o2 : Optional Access) ->
        merge
          { None = None Access
          , Some =
              \(a1 : Access) ->
                merge
                  { None = None Access
                  , Some = \(a2 : Access) -> Some (Access/intersect a1 a2)
                  }
                  o2
          }
          o1

let intersect
    : Mode -> Mode -> Mode
    = \(m1 : Mode) ->
      \(m2 : Mode) ->
        { user = f m1.user m2.user
        , group = f m1.group m2.group
        , other = f m1.other m2.other
        }

in  intersect
