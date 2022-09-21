let User = ./User.dhall

let Group = ./Group.dhall

let Mode = ./Mode.dhall

in  \(content : Type) ->
      { name : Text
      , content : content
      , user : Optional User
      , group : Optional Group
      , mode : Optional Mode
      }
