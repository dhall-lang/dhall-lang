let Make = ./Make.dhall

let Metadata = ./Metadata.dhall

-- | @makeTree tree make parents x@ places the tree @x@ below the directory
-- @parent@. @parent@ is given as a list of 'Metadata' values where each of
-- those will be used to create the respective level of the directory tree.
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
