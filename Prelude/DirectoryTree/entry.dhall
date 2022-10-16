let Entry = ./Entry.dhall

let User = ./User.dhall

let Group = ./Group.dhall

let Mode = ./Mode.dhall

in  \(content : Type) ->
      { Type = Entry content
      , default = { user = None User, group = None Group, mode = None Mode }
      }
