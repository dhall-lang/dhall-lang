{- | @union a b@ intesects the flags of the two `Mask`s @a@ and @b@.
This resembles the bitwise "or", i.e. the value of a flag is 'True' if it is set
to 'True' in either @a@ and @b@. As a consequence @union a rwx == rwx@ and
@intersect a none == a@ for all @a@.
-}
let Mask =
        ./Type.dhall
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ./Type.dhall

let union
    : Mask -> Mask -> Mask
    = \(m1 : Mask) ->
      \(m2 : Mask) ->
        { read = m1.read || m2.read
        , write = m1.write || m2.write
        , execute = m1.execute || m2.execute
        }

let example0 =
      let r =
              ./r.dhall
                sha256:26d7fc8df6194a8051946eac88d0d7fecce690bff6819e69b3c74edf65ac027a
            ? ./r.dhall

      let w =
              ./w.dhall
                sha256:9d10dfc672f61bbb2828c7be2121aae2502938c25adb47bb8cce3c40ba99821b
            ? ./w.dhall

      let x =
              ./x.dhall
                sha256:a966fd88c05a5912a6daa8409e0c9e396f0a4810b51def1e1f62a95e18235f10
            ? ./x.dhall

      let rwx =
              ./rwx.dhall
                sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27
            ? ./rwx.dhall

      in  assert : union r (union w x) === rwx

in  union
