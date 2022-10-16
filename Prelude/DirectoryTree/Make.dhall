let Directory = ./Directory.dhall

let File = ./File.dhall

in  \(tree : Type) ->
      { directory : Directory tree -> tree, file : File -> tree }
