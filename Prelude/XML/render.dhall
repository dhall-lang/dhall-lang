{-|
Render an `XML` value as `Text`

*WARNING:* rendering does not include any XML injection mitigations,
therefore it should not be used to process arbitrary strings into
element attributes or element data.

For indentation and schema validation, see the `xmllint` utility
bundled with libxml2.

```
let XML = ./package.dhall

in  XML.render
    ( XML.node
      { name = "foo"
      , attributes = [ XML.attribute "a" "x", XML.attribute "b" (Natural/show 2) ]
      , content = [ XML.leaf { name = "bar", attributes = XML.emptyAttributes } ]
      }
    )
= "<foo a=\"x\" b=\"2\"><bar/></foo>"
```

-}

let XML =
        ./Type.dhall sha256:461930f3aab769ba537d1a4fd71f411504b0c8d1c1a78d65177be8ded0df8a5c
      ? ./Type.dhall

let Text/concatMap =
        ../Text/concatMap.dhall sha256:7a0b0b99643de69d6f94ba49441cd0fa0507cbdfa8ace0295f16097af37e226f
      ? ../Text/concatMap.dhall

let Text/concat =
        ../Text/concat.dhall sha256:731265b0288e8a905ecff95c97333ee2db614c39d69f1514cb8eed9259745fc0
      ? ../Text/concat.dhall

let Attr = { mapKey : Text, mapValue : Text }

let renderAttr = λ(x : Attr) → " ${x.mapKey}=\"${x.mapValue}\""

let render
    : XML → Text
    = λ(x : XML) →
        x
          Text
          { text = λ(d : Text) → d
          , element =
              λ ( elem
                : { attributes : List { mapKey : Text, mapValue : Text }
                  , content : List Text
                  , name : Text
                  }
                ) →
                let attribs = Text/concatMap Attr renderAttr elem.attributes

                in      "<${elem.name}${attribs}"
                    ++  ( if    Natural/isZero (List/length Text elem.content)
                          then  "/>"
                          else  ">${Text/concat elem.content}</${elem.name}>"
                        )
          }

in  render
