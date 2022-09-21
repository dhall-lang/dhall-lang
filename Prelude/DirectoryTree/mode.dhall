-- | A schema for a blank file mode.
let Access = ./Access.dhall

in  { Type = ./Mode.dhall
    , default = { user = None Access, group = None Access, other = None Access }
    }
