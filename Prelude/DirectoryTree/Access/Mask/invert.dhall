{-|
Inverts the flags set in a `Mask`.
-}
let Mask =
        missing
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ./Type.dhall

let Bool/not =
        missing
          sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4
      ? ../../../Bool/not.dhall

let invert
    : Mask -> Mask
    = \(m : Mask) ->
        { read = Bool/not m.read
        , write = Bool/not m.write
        , execute = Bool/not m.execute
        }

let example0 =
      let none =
              missing
                sha256:db6c3bb734bb3288441f2664379706052943eaba35c021326a600d41ca766925
            ? ./none.dhall

      let rwx =
              missing
                sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27
            ? ./rwx.dhall

      in  assert : invert rwx === none

let example1 =
      let none =
              missing
                sha256:db6c3bb734bb3288441f2664379706052943eaba35c021326a600d41ca766925
            ? ./none.dhall

      let rwx =
              missing
                sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27
            ? ./rwx.dhall

      in  assert : invert none === rwx

in  invert
