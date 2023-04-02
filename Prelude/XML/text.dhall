{-|
Create a Text value to be inserted into an XML element as content.

```
let XML = ./package.dhall

in  XML.render
    ( XML.element
      { name = "location"
      , attributes = XML.emptyAttributes
      , content = [ XML.text "/foo/bar" ]
      }
    )
= "<location>/foo/bar</location>"
```
-}
let XML =
        missing
          sha256:ab91a0edaf0513e0083b1dfae5efa160adc99b0e589775a4a699ab77cce528a9
      ? ./Type.dhall

let text
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
        xml.text d

in  text
