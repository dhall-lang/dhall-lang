let File = ./File.dhall

let Make = ./Make.dhall

let Metadata = ./Metadata.dhall

let path
    : forall (tree : Type) -> Make tree -> List Metadata -> File -> tree
    = \(tree : Type) ->
      \(make : Make tree) ->
      \(parents : List Metadata) ->
      \(file : File) ->
        List/fold
          Metadata
          parents
          tree
          ( \(directory : Metadata) ->
            \(result : tree) ->
              make.directory (directory /\ { content = [ result ] })
          )
          (make.file file)

in  path
