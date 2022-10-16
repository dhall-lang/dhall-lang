let Mask = ./Type.dhall

-- | @intersect a b@ intesects the flags of the two `Mask`s @a@ and @b@.
-- This resembles the bitwise "and", i.e. the value of a flag is 'True' if and
-- only if it is set to 'True' in both @a@ and @b@.
-- As a consequence @intersect a rwx == a@ and @intersect a none == none@ for
-- all @a@.
let intersect
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { read = m1.read && m2.read
        , write = m1.write && m2.write
        , execute = m1.execute && m2.execute
        }

let example0 =
      let a = ./r.dhall

      let rwx = ./rwx.dhall

      in  assert : intersect a rwx === a

let example1 =
      let a = ./r.dhall

      let none = ./none.dhall

      in  assert : intersect a none === none

in  intersect
