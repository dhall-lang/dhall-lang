{-| `equal` checks if two `Mode`s are equal.
-}
let Access =
        ../Access/Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../Access/Type.dhall

let Mode =
        ./Type.dhall
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60
      ? ./Type.dhall

let Access/equal =
        ../Access/equal.dhall
          sha256:5fa90f55505780a7be942275d6bbb2b1f1fb7857364332ed732a0241c2165e53
      ? ../Access/equal.dhall

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
