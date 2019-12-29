{- A record of functions useful for constructing `JSON` values.

   This is only a subset of what `package.dhall` exports. If you are
   not writing a JSON prelude function, you should use the
   `package.dhall` file instead.

   It is used internally by `render`, `renderYAML` and
   `omitNullFields` instead of `package.dhall` to avoid import
   cycles.
-}
{ Type =
      ./Type sha256:88bc5a41073bad3d9d95cc2f9872482cdcd68de969a60069725bbfc240247c72
    ? ./Type
, Tagged =
      ./Tagged sha256:21feca7d2b23f210d0696131d792e18a7d24fdcc85d41a49ba85b98670eba194
    ? ./Tagged
, Nesting =
      ./Nesting sha256:6284802edd41d5d725aa1ec7687e614e21ad1be7e14dd10996bfa9625105c335
    ? ./Nesting
, keyText =
      ./keyText sha256:f7b6c802ca5764d03d5e9a6e48d9cb167c01392f775d9c2c87b83cdaa60ea0cc
    ? ./keyText
, keyValue =
      ./keyValue sha256:a0a97199d280c4cce72ffcbbf93b7ceda0a569cf4d173ac98e0aaaa78034b98c
    ? ./keyValue
, string =
      ./string sha256:8047ed0f24d3ce6bed9307118b6f2c2b1dedd0b11aaeb82eb75234659d35ac74
    ? ./string
, number =
      ./number sha256:c082ea13c3c97f1d170307ed2739a8dca99c636987938000c3c40f301a9c9e5b
    ? ./number
, double =
      ./double sha256:c082ea13c3c97f1d170307ed2739a8dca99c636987938000c3c40f301a9c9e5b
    ? ./double
, integer =
      ./integer sha256:48ba084a961fbbf8189318da92cf7315334972567cb41f3b9b2f2bcee017f38d
    ? ./integer
, natural =
      ./natural sha256:5910dc54deb3f303b53294b462168332bf95f631777907ae264feb38ecb52ee1
    ? ./natural
, object =
      ./object sha256:cff12ab22ec0ecc46c8847ca9ee752cd6c505dd852e6cfdab4a638e5542a3274
    ? ./object
, array =
      ./array sha256:40dda03e1609a92b162500a0e1961e99598c9d2435f200be03112fb6b04256c8
    ? ./array
, bool =
      ./bool sha256:3774779126c3ef4ad705aae1ac56b76fa7264930527c4f7ba5120f3028ab1234
    ? ./bool
, null =
      ./null sha256:900eac8fb957f4c46fbba1480766de72b12851f24b0a17efb14b0467781377d1
    ? ./null
, renderInteger =
      ./renderInteger.dhall sha256:15b8d2ae46d5002832741927af787761df49798c911e2bf85db7a7b9cb5c078c
    ? ./renderInteger.dhall
}
