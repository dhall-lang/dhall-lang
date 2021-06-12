{-|
Create an XML element value without child elements.

```
let XML = ./package.dhall

in  XML.render (XML.leaf { name = "foobar", attributes = XML.emptyAttributes })

= "<foobar/>"
```
-}
let XML =
        ./Type.dhall
          sha256:461930f3aab769ba537d1a4fd71f411504b0c8d1c1a78d65177be8ded0df8a5c
      ? ./Type.dhall

let element =
        ./element.dhall
          sha256:e0b948053c8cd8ccca9c39244d89e3f42db43d222531c18151551dfc75208b4b
      ? ./element.dhall

let leaf
    : { attributes : List { mapKey : Text, mapValue : Text }, name : Text } →
        XML
    = λ ( elem
        : { attributes : List { mapKey : Text, mapValue : Text }, name : Text }
        ) →
        element (elem ⫽ { content = [] : List XML })

in  leaf
