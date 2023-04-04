{-|
Create an XML element value.

```
let XML = ./package.dhall

in  XML.render
    ( XML.element
      { name = "foo"
      , attributes = XML.emptyAttributes
      , content =
          [ XML.leaf { name = "bar", attributes = [ XML.attribute "n" "1" ] }
          , XML.leaf { name = "baz", attributes = [ XML.attribute "n" "2" ] }
          ]
      }
    )

= "<foo><bar n=\"1\"/><baz n=\"2\"/></foo>"
```
-}
let XML =
        missing
          sha256:ab91a0edaf0513e0083b1dfae5efa160adc99b0e589775a4a699ab77cce528a9
      ? ./Type.dhall

let List/map =
        missing
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ../List/map.dhall

let Args =
        { attributes : List { mapKey : Text, mapValue : Text }
        , name : Text
        , content : List XML
        }
      : Type

let element
    : Args → XML
    = λ(elem : Args) →
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
        xml.element
          { attributes = elem.attributes
          , name = elem.name
          , content = List/map XML@1 XML (λ(x : XML@1) → x XML xml) elem.content
          }

in  element
