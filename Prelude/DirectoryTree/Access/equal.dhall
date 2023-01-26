{-|
`equal` checks if two `Access` settings are equal.
-}
let Access =
        ./Type.dhall
          sha256:50689ae80f8c8dcd6e7af33fbc20ea871afb92ec87104253cdbae01f838f6c38
      ? ./Type.dhall

let Bool/equal =
        ../../Bool/equal.dhall
          sha256:f0dc047ca14644c2a979bb126f2a3c6659ec770c66bd7beb70ae4a9d05815709
      ? ../../Bool/equal.dhall

let Optional/equal =
        ../../Optional/equal.dhall
          sha256:5411888399fe9c6720f7f3b59caf5eff3e8e8c30402d09f34e46a4457649a35a
      ? ../../Optional/equal.dhall

let f
    : Optional Bool -> Optional Bool -> Bool
    = Optional/equal Bool Bool/equal

let equal
    : Access -> Access -> Bool
    = \(a1 : Access) ->
      \(a2 : Access) ->
        f a1.execute a2.execute && f a1.read a2.read && f a1.write a2.write

in  equal
