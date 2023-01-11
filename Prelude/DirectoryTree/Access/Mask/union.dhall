{- | @union a b@ intesects the flags of the two `Mask`s @a@ and @b@.
This resembles the bitwise "or", i.e. the value of a flag is 'True' if it is set
to 'True' in either @a@ and @b@. As a consequence @union a rwx == rwx@ and
@intersect a none == a@ for all @a@.
-}
let Mask = ./Type.dhall

let union
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { read = m1.read || m2.read
        , write = m1.write || m2.write
        , execute = m1.execute || m2.execute
        }

let example0 =
      let r = ./r.dhall

      let w = ./w.dhall

      let x = ./x.dhall

      let rwx = ./rwx.dhall

      in  assert : union r (union w x) === rwx

in  union
