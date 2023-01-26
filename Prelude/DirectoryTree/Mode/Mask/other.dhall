{-|
Permissions with other flags set: ------rwx
-}
let rwx =
        ../../Access/Mask/rwx.dhall
          sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27
      ? ../../Access/Mask/rwx.dhall

let none =
        ../../Access/Mask/none.dhall
          sha256:db6c3bb734bb3288441f2664379706052943eaba35c021326a600d41ca766925
      ? ../../Access/Mask/none.dhall

in  { user = none, group = none, other = rwx }
