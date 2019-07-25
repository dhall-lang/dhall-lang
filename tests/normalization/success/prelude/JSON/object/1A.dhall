let JSON/Type = ../../../../../../Prelude/JSON/Type
let JSON = ../../../../../../Prelude/JSON/package.dhall
in  JSON.render
    (JSON.object ([] : List { mapKey : Text, mapValue : JSON/Type }))
