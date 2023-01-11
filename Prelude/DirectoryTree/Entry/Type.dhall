let Metadata = ./Metadata.dhall

in  \(content : Type) -> Metadata //\\ { content : content }
