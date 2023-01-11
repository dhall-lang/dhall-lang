{- | @intersect a b@ intesects the flags of the two `Mask`s @a@ and @b@.
This resembles the bitwise "and", i.e. the value of a flag is 'True' if and only
if it is set to 'True' in both @a@ and @b@. As a consequence
@intersect a rwx == a@ and @intersect a none == none@ for all @a@.
-}
let Mask =
        ./Type.dhall
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ./Type.dhall

let intersect
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { read = m1.read && m2.read
        , write = m1.write && m2.write
        , execute = m1.execute && m2.execute
        }

let example0 =
      let a =
              ./r.dhall
                sha256:26d7fc8df6194a8051946eac88d0d7fecce690bff6819e69b3c74edf65ac027a
            ? ./r.dhall

      let rwx =
              ./rwx.dhall
                sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27
            ? ./rwx.dhall

      in  assert : intersect a rwx === a

let example1 =
      let a =
              ./r.dhall
                sha256:26d7fc8df6194a8051946eac88d0d7fecce690bff6819e69b3c74edf65ac027a
            ? ./r.dhall

      let none =
              ./none.dhall
                sha256:db6c3bb734bb3288441f2664379706052943eaba35c021326a600d41ca766925
            ? ./none.dhall

      in  assert : intersect a none === none

in  intersect
