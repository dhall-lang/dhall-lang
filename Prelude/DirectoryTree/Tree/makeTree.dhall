{-|
@makeTree tree make parents x@ places the tree @x@ below the directory @parent@.
@parent@ is given as a list of 'Metadata' values where each of those will be
used to create the respective level of the directory tree.
-}
let Make =
        ./Make.dhall
          sha256:235d511ed943dded33b46b1717df263037329394e27fb4b9c677eda5af924458
      ? ./Make.dhall

let Metadata =
        ../Entry/Metadata.dhall
          sha256:8c240a00094238a73904af63ac0924b3e6aba1655312f20a2a27f88554e2febe
      ? ../Entry/Metadata.dhall

let makeTree
    : forall (tree : Type) -> Make tree -> List Metadata -> tree -> tree
    = \(tree : Type) ->
      \(make : Make tree) ->
      \(parents : List Metadata) ->
      \(leaf : tree) ->
        List/fold
          Metadata
          parents
          tree
          ( \(directory : Metadata) ->
            \(result : tree) ->
              make.directory (directory /\ { content = [ result ] })
          )
          leaf

in  makeTree
