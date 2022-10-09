let Mask = ./Mask.dhall

let Bool/not = ./../../Bool/not.dhall

-- | Inverts the flags set in a `Mask`.
let invert
    : Mask -> Mask
    = \(m : Access) ->
        { read = Bool/not m.read
        , write = Bool/not m.write
        , execute = Bool/not m.execute
        }

let example0 =
      let none = ./none.dhall

      let rwx = ./rwx.dhall

      in  assert : invert rwx === none

let example1 =
      let none = ./none.dhall

      let rwx = ./rwx.dhall

      in  assert : invert none === rwx

in  invert
