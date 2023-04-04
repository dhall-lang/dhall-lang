{-|
`equal` checks if two `Mode`s are equal.
-}
let Access =
        missing
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ../Access/Type.dhall

let Mode =
        missing
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60
      ? ./Type.dhall

let Access/equal =
        missing
          sha256:5fa90f55505780a7be942275d6bbb2b1f1fb7857364332ed732a0241c2165e53
      ? ../Access/equal.dhall

let Optional/equal =
        missing
          sha256:5411888399fe9c6720f7f3b59caf5eff3e8e8c30402d09f34e46a4457649a35a
      ? ../../Optional/equal.dhall

let f
    : Optional Access -> Optional Access -> Bool
    = Optional/equal Access Access/equal

let equal
    : Mode -> Mode -> Bool
    = \(m1 : Mode) ->
      \(m2 : Mode) ->
        f m1.user m2.user && f m1.group m2.group && f m1.other m2.other

in  equal
