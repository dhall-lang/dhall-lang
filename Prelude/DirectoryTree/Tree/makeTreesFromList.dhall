{- | @makeTreesFromList tree make parents xs@ places the trees @xs@ below the
directory @parent@. @parent@ is given as a list of 'Metadata' values where each
of those will be used to create the respective level of the directory tree.
-}
let Make = ./Make.dhall

let Metadata = ../Entry/Metadata.dhall

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
