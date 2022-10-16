let Make = ./Make.dhall

let Metadata = ./Metadata.dhall

let NonEmpty = ../NonEmpty/Type.dhall

let makeTreesFromList = ./makeTreesFromList.dhall

-- | @makeTreeFromList tree make parents xs@ places the trees @xs@ below the
-- directory @parent@. @parent@ is given as a non-empty list of 'Metadata'
-- values where each of those will be used to create the respective level of the
-- directory tree.
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
