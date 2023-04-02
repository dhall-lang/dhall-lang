{-|
Permissions commonly used for executable files: rwxr-xr-x
-}
let rwx =
        missing
          sha256:cafda16b1ecc0d2f9a63f3aab229a02e18aebb054283c73e50517f1e3727cd27
      ? ../../Access/Mask/rwx.dhall

let rx =
        missing
          sha256:d9beaa853c0de6984c3e5b0e313b573942900f8024fd9aaad743d73df458dc5e
      ? ../../Access/Mask/rx.dhall

in    { user = rwx, group = rx, other = rx }
    :   missing
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall
