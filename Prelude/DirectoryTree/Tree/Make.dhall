let Directory =
        missing
          sha256:163a4536fdd05ce122d793ceda01d03fcffb29463817ae8b7b5601bf4a06ce6e
      ? ../Entry/DirectoryEntry.dhall

let BinaryFile =
        missing
          sha256:f027db2893f6f38bbe6d868a91f22de530e85407237f00b147427c87d32bf55c
      ? ../Entry/BinaryFileEntry.dhall

let TextFile =
        missing
          sha256:23a8cb29d96aeb623501519d9a62c5c49659e8d1c30b4ae4f2399809e3fd3a01
      ? ../Entry/TextFileEntry.dhall

in  \(tree : Type) ->
      { directory : Directory tree -> tree
      , binary-file : BinaryFile -> tree
      , text-file : TextFile -> tree
      }
