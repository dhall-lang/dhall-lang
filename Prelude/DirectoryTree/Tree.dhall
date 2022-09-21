let Directory = ./Directory.dhall

let File = ./File.dhall

in  forall (tree : Type) ->
    forall  ( make
            : { directory : Directory tree -> tree, file : File -> tree }
            ) ->
      List tree
