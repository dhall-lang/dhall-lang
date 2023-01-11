let Directory = ../Entry/Directory.dhall

let File = ../Entry/File.dhall

in  \(tree : Type) ->
      { directory : Directory tree -> tree, file : File -> tree }
