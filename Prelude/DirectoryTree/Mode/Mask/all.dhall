{-|
Permissions with all flags set: rwxrwxrwx
-}
let rwx =
        missing
          sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27
      ? ../../Access/Mask/rwx.dhall

in  { user = rwx, group = rwx, other = rwx }
