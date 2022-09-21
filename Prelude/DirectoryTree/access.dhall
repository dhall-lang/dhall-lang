-- | A schema for blank access rights.
{ Type = ./Access.dhall
, default = { read = None Bool, write = None Bool, execute = None Bool }
}
