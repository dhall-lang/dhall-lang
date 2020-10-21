{-|
Dhall encoding of an arbitrary XML element

For example, the following XML element:

```
<foo n="1"><bar>baz</bar></foo>
```

... corresponds to the following Dhall expression:


```
λ(XML : Type)
 → λ ( xml
     : { text :
           Text → XML
       , element :
             { attributes :
                 List { mapKey : Text, mapValue : Text }
             , content :
                 List XML
             , name :
                 Text
             }
           → XML
       }
     )
 → xml.element
   { attributes =
       [ { mapKey = "n", mapValue = "1" } ]
   , content =
       [ xml.element
         { attributes =
             [] : List { mapKey : Text, mapValue : Text }
         , content =
             [ xml.text "baz" ]
         , name =
             "bar"
         }
       ]
   , name =
       "foo"
   }
```
-}
let XML/Type
    : Type
    = ∀(XML : Type) →
      ∀ ( xml
        : { text : Text → XML
          , element :
              { attributes : List { mapKey : Text, mapValue : Text }
              , content : List XML
              , name : Text
              } →
                XML
          }
        ) →
        XML

in  XML/Type
