{- | A schema for a blank file mode.
-}
let Access = ../Access/Type.dhall

in  { Type = ./Type.dhall
    , default = { user = None Access, group = None Access, other = None Access }
    }
