let Entry =
        ./Type.dhall
          sha256:75148ae19175750e38705e11cda8dcc775b2ac08f22518ff2ef3f33a6273ef15
      ? ./Type.dhall

let User =
        ../User/Type.dhall
          sha256:8b25916612d2c9b17130d16b55c6bdb085dd118e692f72bf351a83b1d0ac8833
      ? ../User/Type.dhall

let Group =
        ../Group/Type.dhall
          sha256:83e6e8846153d94abf6f879808c94f5cdba3f486cc9e392eb6124b1dc67368cf
      ? ../Group/Type.dhall

let Mode =
        ../Mode/Type.dhall
          sha256:f05819ec2145e7dabf4aa167338bee6d326aabd81355dcf0b078e358bd34ec60
      ? ../Mode/Type.dhall

in  \(content : Type) ->
      { Type = Entry content
      , default = { user = None User, group = None Group, mode = None Mode }
      }
