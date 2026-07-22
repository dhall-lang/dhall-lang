{-|
@makeTreeFromList tree make parents xs@ places the trees @xs@ below the
directory @parent@. @parent@ is given as a non-empty list of 'Metadata' values
where each of those will be used to create the respective level of the directory
tree.
-}
let Make =
        missing
          sha256:33c52c16f76b41c3635b57a38ce0286f1ad8b3163a3f07a3b41b808ac5bfeb5b
      ? ./Make.dhall

let Metadata =
        missing
          sha256:8c240a00094238a73904af63ac0924b3e6aba1655312f20a2a27f88554e2febe
      ? ../Entry/Metadata.dhall

let NonEmpty =
        missing
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ../../NonEmpty/Type.dhall

let makeTreesFromList =
        missing
          sha256:02fce5b194da66c2f98bd05af0a3bfdccf0b57d7971f17bcafee436652d9a88e
      ? ./makeTreesFromList.dhall

let makeTreeFromList
    : forall (tree : Type) ->
      Make tree ->
      NonEmpty Metadata ->
      List tree ->
        tree
    = \(tree : Type) ->
      \(make : Make tree) ->
      \(parents : NonEmpty Metadata) ->
      \(leaves : List tree) ->
        make.directory
          (     parents.head
            /\  { content = makeTreesFromList tree make parents.tail leaves }
          )

in  makeTreeFromList
