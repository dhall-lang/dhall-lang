let User = ./User.dhall

let Group = ./Group.dhall

let Mode = ./Mode.dhall

in  { name : Text
    , user : Optional User
    , group : Optional Group
    , mode : Optional Mode
    }
