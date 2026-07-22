{-|
@makeTreesFromList tree make parents xs@ places the trees @xs@ below the
directory @parent@. @parent@ is given as a list of 'Metadata' values where each
of those will be used to create the respective level of the directory tree.
-}
let Make =
        missing
          sha256:33c52c16f76b41c3635b57a38ce0286f1ad8b3163a3f07a3b41b808ac5bfeb5b
      ? ./Make.dhall

let Metadata =
        missing
          sha256:8c240a00094238a73904af63ac0924b3e6aba1655312f20a2a27f88554e2febe
      ? ../Entry/Metadata.dhall

let makeTreesFromList
    : forall (tree : Type) ->
      Make tree ->
      List Metadata ->
      List tree ->
        List tree
    = \(tree : Type) ->
      \(make : Make tree) ->
      \(parents : List Metadata) ->
      \(leaves : List tree) ->
        List/fold
          Metadata
          parents
          (List tree)
          ( \(directory : Metadata) ->
            \(content : List tree) ->
              [ make.directory (directory /\ { content }) ]
          )
          leaves

in  makeTreesFromList
