{-| Permissions with all flags set: rwxrwxrwx
-}
let rwx =
      ../../Access/Mask/rwx.dhall
        sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27

in  { user = rwx, group = rwx, other = rwx }
