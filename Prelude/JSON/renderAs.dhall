--| Render a `JSON` value as `Text` in either JSON or YAML format.
let JSON =
        missing
          sha256:5dc1135d5481cfd6fde625aaed9fcbdb7aa7c14f2e76726aa5fdef028a5c10f5
      ? ./core.dhall

let Function/identity =
        missing
          sha256:f78b96792b459cb664f41c6119bd8897dd04353a3343521d436cd82ad71cb4d4
      ? ../Function/identity.dhall

let Text/concatMap =
        missing
          sha256:7a0b0b99643de69d6f94ba49441cd0fa0507cbdfa8ace0295f16097af37e226f
      ? ../Text/concatMap.dhall

let List/map =
        missing
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ../List/map.dhall

let NonEmpty =
        missing
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ../NonEmpty/Type.dhall

let NonEmpty/toList =
        missing
          sha256:0977fe14b77232a4451dcf409c43df4589c4b3cdde7b613aab8df183be1b53f5
      ? ../NonEmpty/toList.dhall

let NonEmpty/concat =
        missing
          sha256:6d55181938c06c6b806877028f6a241912e9c0935d9a10dd958775bf21e0f64d
      ? ../NonEmpty/concat.dhall

let NonEmpty/map =
        missing
          sha256:93d53afe874bb2eed946c21ca5ada3c9716b7d00e6d8edfaba6484cd9c5a00bd
      ? ../NonEmpty/map.dhall

let NonEmpty/singleton =
        missing
          sha256:c9197aabe97695f7ca66f7419bf172d806b2c915594a8fc0d2ff6495db496ff2
      ? ../NonEmpty/singleton.dhall

let List/uncons
    : ∀(a : Type) → List a → Optional (NonEmpty a)
    = {- This version uses the `ls` argument only once to prevent cache blowups at the price
         of performing two passes over the list:
         A first one to reverse it, a second one with `List/fold` to determine
         the head element.
         See https://github.com/dhall-lang/dhall-lang/pull/1015#issuecomment-633381024
         for some context regarding the caching issue.
      -}
      λ(a : Type) →
      λ(ls : List a) →
        List/fold
          a
          (List/reverse a ls)
          (Optional (NonEmpty a))
          ( λ(x : a) →
            λ(acc : Optional (NonEmpty a)) →
              merge
                { None = Some (NonEmpty/singleton a x)
                , Some =
                    λ(ne : NonEmpty a) → Some (ne ⫽ { tail = ne.tail # [ x ] })
                }
                acc
          )
          (None (NonEmpty a))

let NonEmpty/mapHead
    : ∀(a : Type) → (a → a) → NonEmpty a → NonEmpty a
    = λ(a : Type) →
      λ(fn : a → a) →
      λ(ls : NonEmpty a) →
        ls ⫽ { head = fn ls.head }

let NonEmpty/mapTail
    : ∀(a : Type) → (a → a) → NonEmpty a → NonEmpty a
    = λ(a : Type) →
      λ(fn : a → a) →
      λ(ls : NonEmpty a) →
        ls ⫽ { tail = List/map a a fn ls.tail }

let NonEmpty/prepend
    : ∀(a : Type) → a → NonEmpty a → NonEmpty a
    = λ(a : Type) →
      λ(prefix : a) →
      λ(ls : NonEmpty a) →
        { head = prefix, tail = NonEmpty/toList a ls }

let NonYtpme
    : Type → Type
    = λ(a : Type) → { init : List a, last : a }

let List/unsnoc
    : ∀(a : Type) → List a → Optional (NonYtpme a)
    = λ(a : Type) →
      λ(ls : List a) →
        List/fold
          a
          ls
          (Optional (NonYtpme a))
          ( λ(x : a) →
            λ(acc : Optional (NonYtpme a)) →
              merge
                { None = Some { init = [] : List a, last = x }
                , Some =
                    λ(ny : NonYtpme a) → Some (ny ⫽ { init = [ x ] # ny.init })
                }
                acc
          )
          (None (NonYtpme a))

let NonEmpty/mapLast
    : ∀(a : Type) → (a → a) → NonEmpty a → NonEmpty a
    = λ(a : Type) →
      λ(fn : a → a) →
      λ(ls : NonEmpty a) →
        merge
          { Some = λ(x : NonYtpme a) → ls ⫽ { tail = x.init # [ fn x.last ] }
          , None = NonEmpty/singleton a (fn ls.head)
          }
          (List/unsnoc a ls.tail)

let NonEmpty/mapLeading
    : ∀(a : Type) → (a → a) → NonEmpty a → NonEmpty a
    = λ(a : Type) →
      λ(fn : a → a) →
      λ(ls : NonEmpty a) →
        merge
          { Some =
              λ(x : NonYtpme a) →
                { head = fn ls.head
                , tail = List/map a a fn x.init # [ x.last ]
                }
          , None = ls
          }
          (List/unsnoc a ls.tail)

let Lines
    : Type
    = NonEmpty Text

let Block
    : Type
    = < Simple : Text | Complex : Lines >

let Block/toLines
    : Block → Lines
    = λ(block : Block) →
        merge
          { Simple = NonEmpty/singleton Text
          , Complex = Function/identity Lines
          }
          block

let manyBlocks
    : ∀(a : Type) → Text → (NonEmpty a → Lines) → List a → Block
    = λ(a : Type) →
      λ(ifEmpty : Text) →
      λ(render : NonEmpty a → Lines) →
      λ(inputs : List a) →
        merge
          { Some = λ(inputs : NonEmpty a) → Block.Complex (render inputs)
          , None = Block.Simple ifEmpty
          }
          (List/uncons a inputs)

let blockToText
    : Block → Text
    = λ(block : Block) →
        Text/concatMap
          Text
          (λ(line : Text) → line ++ "\n")
          (NonEmpty/toList Text (Block/toLines block))

let addPrefix = λ(prefix : Text) → λ(line : Text) → prefix ++ line

let addIndent = addPrefix "  "

let indentTail = NonEmpty/mapTail Text addIndent

let Format =
        missing
          sha256:d7936b510cfc091faa994652af0eb5feb889cd44bc989edbe4f1eb8c5623caac
      ? ./Format.dhall

let ObjectField = { mapKey : Text, mapValue : Block }

let -- Essentially the same thing as `Text/show`, except that this does not
    -- escape `$`
    escape =
      List/fold
        (Text → Text)
        [ Text/replace "\"" "\\\""
        , Text/replace "\b" "\\b"
        , Text/replace "\f" "\\f"
        , Text/replace "\n" "\\n"
        , Text/replace "\r" "\\r"
        , Text/replace "\t" "\\t"
        , Text/replace "\\" "\\\\"
        ]
        Text
        (λ(replace : Text → Text) → λ(text : Text) → replace text)

let renderJSONStruct =
      λ(prefix : Text) →
      λ(suffix : Text) →
      λ(blocks : NonEmpty Lines) →
        let indent = List/map Text Text addIndent

        let appendComma
            : Lines → Lines
            = NonEmpty/mapLast Text (λ(line : Text) → line ++ ",")

        let blocks = NonEmpty/mapLeading Lines appendComma blocks

        let block = NonEmpty/concat Text blocks

        in  merge
              { None =
                  NonEmpty/singleton Text "${prefix} ${block.head} ${suffix}"
              , Some =
                  λ(ny : NonYtpme Text) →
                    { head = prefix
                    , tail =
                          indent ([ block.head ] # ny.init # [ ny.last ])
                        # [ suffix ]
                    }
              }
              (List/unsnoc Text block.tail)

let renderObject =
      λ(format : Format) →
      λ(fields : NonEmpty ObjectField) →
        let keystr = λ(field : ObjectField) → "\"${escape field.mapKey}\":"

        let prefixKeyOnFirst =
              λ(field : ObjectField) →
                NonEmpty/mapHead
                  Text
                  (addPrefix "${keystr field} ")
                  (Block/toLines field.mapValue)

        let prependKeyLine =
              λ(field : ObjectField) →
                NonEmpty/prepend
                  Text
                  (keystr field)
                  (Block/toLines field.mapValue)

        let renderYAMLField =
              λ(field : ObjectField) →
                merge
                  { Simple =
                      λ(line : Text) →
                        NonEmpty/singleton Text "${keystr field} ${line}"
                  , Complex = λ(_ : Lines) → indentTail (prependKeyLine field)
                  }
                  field.mapValue

        in  merge
              { JSON =
                  renderJSONStruct
                    "{"
                    "}"
                    (NonEmpty/map ObjectField Lines prefixKeyOnFirst fields)
              , YAML =
                  NonEmpty/concat
                    Text
                    (NonEmpty/map ObjectField Lines renderYAMLField fields)
              }
              format

let renderYAMLArrayField =
      λ(block : Block) →
        NonEmpty/mapHead
          Text
          (addPrefix "- ")
          (indentTail (Block/toLines block))

let renderArray =
      λ(format : Format) →
      λ(fields : NonEmpty Block) →
        merge
          { JSON =
              renderJSONStruct
                "["
                "]"
                (NonEmpty/map Block Lines Block/toLines fields)
          , YAML =
              NonEmpty/concat
                Text
                (NonEmpty/map Block Lines renderYAMLArrayField fields)
          }
          format

let renderAs
    : Format → JSON.Type → Text
    = λ(format : Format) →
      λ(json : JSON.Type) →
        blockToText
          ( json
              Block
              { string = λ(x : Text) → Block.Simple "\"${escape x}\""
              , double = λ(x : Double) → Block.Simple (Double/show x)
              , integer = λ(x : Integer) → Block.Simple (JSON.renderInteger x)
              , object = manyBlocks ObjectField "{}" (renderObject format)
              , array = manyBlocks Block "[]" (renderArray format)
              , bool =
                  λ(x : Bool) → Block.Simple (if x then "true" else "false")
              , null = Block.Simple "null"
              }
          )

let example0 =
      let data =
            JSON.array
              [ JSON.bool True
              , JSON.string "Hello"
              , JSON.object
                  [ { mapKey = "foo", mapValue = JSON.null }
                  , { mapKey = "bar", mapValue = JSON.double 1.0 }
                  ]
              ]

      let yaml =
              assert
            :   renderAs Format.YAML data
              ≡ ''
                - true
                - "Hello"
                - "foo": null
                  "bar": 1.0
                ''

      let json =
              assert
            :   renderAs Format.JSON data
              ≡ ''
                [
                  true,
                  "Hello",
                  {
                    "foo": null,
                    "bar": 1.0
                  }
                ]
                ''

      in  True

let example1 =
      let data =
            JSON.object
              [ { mapKey = "zero", mapValue = JSON.array ([] : List JSON.Type) }
              , { mapKey = "one", mapValue = JSON.array [ JSON.string "a" ] }
              , { mapKey = "two"
                , mapValue = JSON.array [ JSON.string "a", JSON.string "b" ]
                }
              ]

      let yaml =
              assert
            :   renderAs Format.YAML data
              ≡ ''
                "zero": []
                "one":
                  - "a"
                "two":
                  - "a"
                  - "b"
                ''

      let json =
              assert
            :   renderAs Format.JSON data
              ≡ ''
                {
                  "zero": [],
                  "one": [ "a" ],
                  "two": [
                    "a",
                    "b"
                  ]
                }
                ''

      in  True

let example2 =
      let data =
            JSON.object
              [ { mapKey = "zero"
                , mapValue =
                    JSON.object
                      (toMap {=} : List { mapKey : Text, mapValue : JSON.Type })
                }
              , { mapKey = "one"
                , mapValue = JSON.object (toMap { a = JSON.null })
                }
              , { mapKey = "two"
                , mapValue =
                    JSON.object (toMap { a = JSON.null, b = JSON.null })
                }
              ]

      let yaml =
              assert
            :   renderAs Format.YAML data
              ≡ ''
                "zero": {}
                "one":
                  "a": null
                "two":
                  "a": null
                  "b": null
                ''

      let json =
              assert
            :   renderAs Format.JSON data
              ≡ ''
                {
                  "zero": {},
                  "one": { "a": null },
                  "two": {
                    "a": null,
                    "b": null
                  }
                }
                ''

      in  True

let example3 =
      let specialCharacters =
            ''
            "\${"\b\f"}
            ${"\r"}	$''

      let data =
            JSON.object
              [ { mapKey = specialCharacters
                , mapValue = JSON.string specialCharacters
                }
              ]

      in    assert
          :   renderAs Format.JSON data
            ≡ ''
              { "\"\\\b\f\n\r\t$": "\"\\\b\f\n\r\t$" }
              ''

in  renderAs
