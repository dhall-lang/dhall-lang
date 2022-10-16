-- | A schema for blank access rights.
{ Type = ./Type.dhall
, default = { read = None Bool, write = None Bool, execute = None Bool }
}
