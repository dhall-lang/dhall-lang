let Directory =
        missing
          sha256:163a4536fdd05ce122d793ceda01d03fcffb29463817ae8b7b5601bf4a06ce6e
      ? ../Entry/DirectoryEntry.dhall

let File =
        missing
          sha256:23a8cb29d96aeb623501519d9a62c5c49659e8d1c30b4ae4f2399809e3fd3a01
      ? ../Entry/FileEntry.dhall

in  \(tree : Type) ->
      { directory : Directory tree -> tree, file : File -> tree }
