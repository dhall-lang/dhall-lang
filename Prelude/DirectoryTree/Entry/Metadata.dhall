-- An entry without content. That in turn implies that the type of the entry
-- (i.e. whether it is a file or a directory) is not (yet) fixed as well.

let User = ../User/Type.dhall

let Group = ../Group/Type.dhall

let Mode = ../Mode/Type.dhall

in  { name : Text
    , user : Optional User
    , group : Optional Group
    , mode : Optional Mode
    }
