{-|
Permissions commonly used for regular files: rw-r--r--
-}
let r =
        missing
          sha256:26d7fc8df6194a8051946eac88d0d7fecce690bff6819e69b3c74edf65ac027a
      ? ../../Access/Mask/r.dhall

let rw =
        missing
          sha256:c3cce19f462b841e64adafbdf2963699a0031f5fc08b041c8ad364483c544d8b
      ? ../../Access/Mask/rw.dhall

in    { user = rw, group = r, other = r }
    :   missing
          sha256:4f97762058f24053e03997565a78800a5a2586159deaa265a4ee84a3d94ad471
      ? ./Type.dhall
