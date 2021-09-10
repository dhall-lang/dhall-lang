{-|
Create a Text value to be inserted into an XML element as content with no
character escaping.

```
let XML = ./package.dhall

in  XML.render
    ( XML.element
      { name = "location"
      , attributes = XML.emptyAttributes
      , content = [ XML.rawText "<bar n=\"1\"/>" ]
      }
    )
= "<foo><bar n=\"1\"/></foo>"
```
-}
let XML =
        ./Type.dhall
          sha256:ab91a0edaf0513e0083b1dfae5efa160adc99b0e589775a4a699ab77cce528a9
      ? ./Type.dhall

let rawText
    : Text → XML
    = λ(d : Text) →
      λ(XML : Type) →
      λ ( xml
        : { text : Text → XML
          , rawText : Text → XML
          , element :
              { attributes : List { mapKey : Text, mapValue : Text }
              , content : List XML
              , name : Text
              } →
                XML
          }
        ) →
        xml.rawText d

in  rawText
