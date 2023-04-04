{-|
A complete mode with all access rights specified explicitely. For an incomplete
mode where some access rights might not be set see `Mode`.
-}
let Access/Mask =
        missing
          sha256:c0fa7626b69e117086439a7b4ee15d1a80e16e38fe2ccc13f55e6dd26030b4df
      ? ../../Access/Mask/Type.dhall

in  { user : Access/Mask, group : Access/Mask, other : Access/Mask }
