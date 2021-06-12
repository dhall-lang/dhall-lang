{-|
Create a Text value to be inserted into an XML element as content.

```
let XML = ./package.dhall

in  XML.render
    ( XML.node
      { name = "location"
      , attributes = XML.emptyAttributes
      , content = [ XML.text "/foo/bar" ]
      }
    )
= "<location>/foo/bar</location>"
```
-}
let XML =
        ./Type.dhall
          sha256:461930f3aab769ba537d1a4fd71f411504b0c8d1c1a78d65177be8ded0df8a5c
      ? ./Type.dhall

let text
    : Text → XML
    = λ(d : Text) →
      λ(XML : Type) →
      λ ( xml
        : { text : Text → XML
          , element :
              { attributes : List { mapKey : Text, mapValue : Text }
              , content : List XML
              , name : Text
              } →
                XML
          }
        ) →
        xml.text d

in  text
