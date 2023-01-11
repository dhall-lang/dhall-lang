let Entry = ./Type.dhall

let User = ../User/Type.dhall

let Group = ../Group/Type.dhall

let Mode = ../Mode/Type.dhall

in  \(content : Type) ->
      { Type = Entry content
      , default = { user = None User, group = None Group, mode = None Mode }
      }
