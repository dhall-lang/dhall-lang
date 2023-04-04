{-|
Create an XML element value without child elements.

```
let XML = ./package.dhall

in  XML.render (XML.leaf { name = "foobar", attributes = XML.emptyAttributes })

= "<foobar/>"
```
-}
let XML =
        missing
          sha256:ab91a0edaf0513e0083b1dfae5efa160adc99b0e589775a4a699ab77cce528a9
      ? ./Type.dhall

let element =
        missing
          sha256:79266d604e147caf37e985581523b684f7bac66de0c93dd828841df3dfc445f9
      ? ./element.dhall

let leaf
    : { attributes : List { mapKey : Text, mapValue : Text }, name : Text } →
        XML
    = λ ( elem
        : { attributes : List { mapKey : Text, mapValue : Text }, name : Text }
        ) →
        element (elem ⫽ { content = [] : List XML })

in  leaf
