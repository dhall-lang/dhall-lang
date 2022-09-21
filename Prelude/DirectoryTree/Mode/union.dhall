-- | Calculates the union of the access rights set by both modes using Access/union.

let Access = ./../Access.dhall

let Mode = ./../Mode.dhall

let Access/union = ./../Access/union.dhall

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
                  , Some = \(a2 : Access) -> Some (Access/union a1 a2)
                  }
                  o2
          }
          o1

let union
    : Mode -> Mode -> Mode
    = \(m1 : Mode) ->
      \(m2 : Mode) ->
        { user = f m1.user m2.user
        , group = f m1.group m2.group
        , other = f m1.other m2.other
        }

in  union
